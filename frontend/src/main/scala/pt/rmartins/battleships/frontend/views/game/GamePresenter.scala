package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import io.udash.auth.AuthRequires
import io.udash.i18n.translatedDynamic
import org.scalajs.dom.window
import pt.rmartins.battleships.frontend.ApplicationContext.application
import pt.rmartins.battleships.frontend.routing.{RoutingInGameState, RoutingLoginPageState}
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.frontend.views.game.ModeType._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, PlayingMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.Utils.canPlaceInBoard
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import scalatags.JsDom.all.span

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Random, Success}

class GamePresenter(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    gameStateModel: ModelProperty[GameStateModel],
    chatModel: ModelProperty[ChatModel],
    screenModel: ModelProperty[ScreenModel],
    gameRpc: GameRPC,
    userService: UserContextService,
    translationsService: TranslationsService,
    notificationsCenter: NotificationsCenter
)(implicit
    ec: ExecutionContext
) extends Presenter[RoutingInGameState.type]
    with AuthRequires {

  import translationsService._

  val gameStateProperty: Property[Option[GameState]] =
    gameStateModel.bitransform(_.gameState)(GameStateModel(_))

  val enemyProperty: ReadableProperty[Option[SimplePlayer]] =
    gameStateModel.transform(_.gameState.map(_.enemy))

  val gameModeProperty: Property[Option[GameMode]] =
    gameStateProperty.bitransform[Option[GameMode]](_.map(_.gameMode)) {
      case None =>
        gameStateModel.get.gameState
      case Some(gameMode) =>
        gameStateModel.get.gameState.map(_.copy(gameMode = gameMode))
    }
  val modeTypeProperty: ReadableProperty[Option[ModeType]] =
    gameModeProperty.transform(_.map {
      case _: PreGameMode  => PreGameModeType
      case _: PlayingMode  => PlayingModeType
      case _: GameOverMode => GameOverModeType
    })

  val preGameModeProperty: Property[Option[PreGameMode]] =
    gameModeProperty.bitransform[Option[PreGameMode]] {
      case Some(preGameMode: PreGameMode) => Some(preGameMode)
      case _                              => None
    }(identity)

  val playingModeProperty: Property[Option[PlayingMode]] =
    gameModeProperty.bitransform[Option[PlayingMode]] {
      case Some(playingMode: PlayingMode) => Some(playingMode)
      case _                              => None
    }(identity)

  val rulesProperty: ReadableProperty[Option[Rules]] =
    gameStateProperty.transform(_.map(_.rules))

  val inPreGameMode: ReadableProperty[Boolean] =
    gameModeProperty.transform(_.exists(_.isPreGame))

  val playingMode: ReadableProperty[Boolean] =
    gameModeProperty.transform(_.exists(_.isPlaying))

  val inGameOverMode: ReadableProperty[Boolean] =
    gameModeProperty.transform(_.exists(_.isEndGame))

  val isMyTurnProperty: ReadableProperty[Boolean] =
    playingModeProperty.transform(_.exists(_.isMyTurn))

  val meProperty: ReadableProperty[Option[Player]] =
    gameStateProperty.transform(_.map(_.me))

  val selectedTabProperty: Property[String] =
    screenModel.subProp(_.selectedTab)

  val mousePositionProperty: Property[Option[Coordinate]] =
    gameModel.subProp(_.mousePosition)

  val lineDashOffset: Property[Int] =
    gameModel.subProp(_.lineDashOffset)

  private val msgCallback = notificationsCenter.onNewMsg { case msg =>
    chatModel.subSeq(_.msgs).append(msg)
  }

  private val onQuitGameCallback = notificationsCenter.onQuitGame { case _ =>
    val updatedGameState = None
    updateGameMode(gameStateProperty.get.map(_.gameMode), updatedGameState)
    gameModel.set(GameModel.default)
    gameStateProperty.set(updatedGameState)
    screenModel.set(ScreenModel.resetScreenModel(screenModel.get))
  }

  private val onGameStateCallback = notificationsCenter.onGameState { case updatedGameState =>
    updateGameMode(gameStateProperty.get.map(_.gameMode), Some(updatedGameState))
    gameStateProperty.set(Some(updatedGameState))
  }

  private val onGameModeCallback = notificationsCenter.onGameMode { case updatedGameMode =>
    val updatedGameState = gameStateProperty.get.map(_.copy(gameMode = updatedGameMode))
    updateGameMode(gameStateProperty.get.map(_.gameMode), updatedGameState)
    gameStateProperty.set(updatedGameState)
  }

  val chatMessagesProperty: ReadableSeqProperty[ChatMessage] =
    chatModel.subSeq(_.msgs)
  val chatMessagesSizeProperty: ReadableProperty[Int] =
    chatMessagesProperty.transform(_.size)
  val chatMessagesShowNotification: ReadableProperty[Option[Int]] =
    combine(
      chatMessagesSizeProperty,
      screenModel.subProp(_.lastSeenMessagesChat),
      selectedTabProperty
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.chatTab)
    }

  val myMovesHistoryProperty: ReadableSeqProperty[TurnPlay] =
    combine(
      gameStateProperty,
      screenModel.subProp(_.showMissesMoves),
      screenModel.subProp(_.showDisabledMoves),
      screenModel.subProp(_.disabledMovesSet)
    ).transformToSeq { case (gameStateOpt, showMissesMoves, showDisabledMoves, disabledMovesSet) =>
      gameStateOpt
        .map(
          _.me.turnPlayHistory.filter { turnPlay =>
            (showMissesMoves || turnPlay.hitHints.exists(_.isShip)) &&
            (showDisabledMoves || !disabledMovesSet(turnPlay.turn))
          }
        )
        .getOrElse(Seq.empty)
    }
  val myMovesHistorySizeProperty: ReadableProperty[Int] =
    myMovesHistoryProperty.transform(_.size)
  val myMovesHistoryShowNotification: ReadableProperty[Option[Int]] =
    combine(
      myMovesHistorySizeProperty,
      screenModel.subProp(_.lastSeenMessagesMyMoves),
      selectedTabProperty
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.myMovesTab)
    }

  val enemyMovesHistoryProperty: ReadableSeqProperty[TurnPlay] =
    gameStateProperty.transformToSeq(_.map(_.enemy.turnPlayHistory).getOrElse(Seq.empty))
  val enemyMovesHistorySizeProperty: ReadableProperty[Int] =
    enemyMovesHistoryProperty.transform(_.size)
  val enemyMovesHistoryShowNotification: ReadableProperty[Option[Int]] =
    combine(
      enemyMovesHistorySizeProperty,
      screenModel.subProp(_.lastSeenMessagesEnemyMoves),
      selectedTabProperty
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.enemyMovesTab)
    }

  private var lineDashOffsetIntervalHandle: Option[Int] = None
  private val lineDashOffsetIntervalMillis: Int = 200

  def setLineDashOffsetInterval(activate: Boolean): Unit = {
    lineDashOffsetIntervalHandle.foreach(window.clearTimeout)
    if (activate)
      lineDashOffsetIntervalHandle = Some(
        window.setInterval(
          () => {
            lineDashOffset.set((lineDashOffset.get + 1) % 17)
          },
          timeout = lineDashOffsetIntervalMillis
        )
      )
  }

  private var timeRemainingIntervalHandle: Option[Int] = None
  private val timeRemainingIntervalMillis: Int = 100

  def updateGameMode(fromGameMode: Option[GameMode], to: Option[GameState]): Unit = {
    (fromGameMode, to) match {
      case (None, Some(GameState(_, _, me, _, _: PreGameMode))) =>
        me.shipsLeftToPlace.headOption.foreach { headShip =>
          gameModel.subProp(_.selectedShip).set(Some(headShip))
        }
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
        setLineDashOffsetInterval(activate = false)

        selectedTabProperty.set(ScreenModel.chatTab)
        screenModel.subProp(_.lastSeenMessagesChat).set(chatMessagesSizeProperty.get)
        screenModel.subProp(_.screenResized).set((), force = true)
      case (
            None | Some(_: PreGameMode),
            Some(GameState(_, _, me, enemy, PlayingMode(_, _, turnAttackTypes, _, _)))
          ) =>
        selectedTabProperty.set(ScreenModel.myMovesTab)
        screenModel.subProp(_.lastSeenMessagesMyMoves).set(me.turnPlayHistory.size)
        screenModel.subProp(_.lastSeenMessagesEnemyMoves).set(enemy.turnPlayHistory.size)
        gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))
        gameModel.subProp(_.selectedShip).set(None)

        setLineDashOffsetInterval(activate = true)
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = Some(
          window.setInterval(
            () => {
              def reduceTime(timeRemaining: TimeRemaining): TimeRemaining = {
                val timeInterval = timeRemainingIntervalMillis
                timeRemaining match {
                  case TimeRemaining(totalTimeRemainingMillis, None) =>
                    TimeRemaining(Math.max(0, totalTimeRemainingMillis - timeInterval), None)
                  case TimeRemaining(totalTimeRemainingMillis, Some(turnTimeRemainingMillis)) =>
                    if (turnTimeRemainingMillis >= timeInterval)
                      TimeRemaining(
                        totalTimeRemainingMillis,
                        Some(turnTimeRemainingMillis - timeInterval)
                      )
                    else
                      TimeRemaining(
                        Math.max(
                          0,
                          totalTimeRemainingMillis + turnTimeRemainingMillis - timeInterval
                        ),
                        Some(0)
                      )
                }
              }

              val timeRemainingPropertyOpt: Property[Option[(TimeRemaining, TimeRemaining)]] =
                gameModel.subProp(_.timeRemaining)

              (playingModeProperty.get.map(_.isMyTurn), timeRemainingPropertyOpt.get) match {
                case (Some(isMyTurn), Some((myTimeRemaining, enemyTimeRemaining))) =>
                  timeRemainingPropertyOpt.set(
                    Some(
                      if (isMyTurn)
                        (reduceTime(myTimeRemaining), enemyTimeRemaining)
                      else
                        (myTimeRemaining, reduceTime(enemyTimeRemaining))
                    )
                  )
                case _ =>
              }
            },
            timeout = timeRemainingIntervalMillis
          )
        )
      case (_, Some(GameState(_, _, _, _, _: GameOverMode))) =>
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
        setLineDashOffsetInterval(activate = true)
      case (_, None) =>
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
        setLineDashOffsetInterval(activate = false)

        chatModel.subProp(_.msgs).set(Seq.empty)
        selectedTabProperty.set(ScreenModel.chatTab)
        screenModel.subProp(_.lastSeenMessagesChat).set(0)
      case _ =>
    }

    to.map(_.gameMode) match {
      case Some(PlayingMode(_, _, _, Some(myTimeRemaining), Some(enemyTimeRemaining))) =>
        gameModel.subProp(_.timeRemaining).set(Some((myTimeRemaining, enemyTimeRemaining)))
      case Some(GameOverMode(_, _, Some(myTimeRemaining), Some(enemyTimeRemaining), _)) =>
        gameModel.subProp(_.timeRemaining).set(Some((myTimeRemaining, enemyTimeRemaining)))
      case _ =>
        gameModel.subProp(_.timeRemaining).set(None)
    }

    (fromGameMode, to) match {
      case (None, Some(GameState(_, rules, _, _, _))) =>
        val shipCounter = preGameModel.get.shipCounter
        rules.gameFleet.shipsCounter.foreach { case (shipId, counter) =>
          shipCounter(shipId).set(counter)
        }
        preGameModel.subProp(_.boardSize).set(rules.boardSize)
      case _ =>
    }
  }

  private var newTurnAnimationHandle: Option[Int] = None
  private val newTurnAnimationMillis: Int = 50

  private def stopNewTurnAnimation(): Unit = {
    newTurnAnimationHandle.foreach(window.clearTimeout)
    newTurnAnimationHandle = None
  }

  playingModeProperty
    .transform(_.map(inGameMode => (inGameMode.turn, inGameMode.turnAttackTypes)))
    .listen {
      case Some((Turn(_, extraTurn), turnAttackTypes)) if gameModel.get.turnAttacksSent =>
        gameModel.subProp(_.turnAttacksSent).set(false)
        gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))
        screenModel
          .subProp(_.missilesPopupMillisOpt)
          .set(Some(BoardView.MissilesInitialPopupTime))
        screenModel
          .subProp(_.extraTurnPopup)
          .set(extraTurn.map(_ => BoardView.ExtraTurnPopupTime))

        if (newTurnAnimationHandle.isEmpty) {
          newTurnAnimationHandle = Some(
            window.setInterval(
              () => {
                val screen = screenModel.get

                screen.missilesPopupMillisOpt.foreach { missilesPopupMillis =>
                  val updatedTime = missilesPopupMillis - newTurnAnimationMillis
                  screenModel
                    .subProp(_.missilesPopupMillisOpt)
                    .set(Some(updatedTime).filter(_ > 0))
                }
                screen.extraTurnPopup.foreach { timeRemaining =>
                  val updatedTime = timeRemaining - newTurnAnimationMillis
                  screenModel
                    .subProp(_.extraTurnPopup)
                    .set(Some(updatedTime).filter(_ > 0))
                }

                if (screen.missilesPopupMillisOpt.isEmpty && screen.extraTurnPopup.isEmpty)
                  stopNewTurnAnimation()
              },
              newTurnAnimationMillis
            )
          )
        }
      case _ =>
    }

  override def handleState(state: RoutingInGameState.type): Unit = {
    chatModel.subProp(_.username).set(userService.getCurrentContext.username)

    gameRpc.getAllMessages.onComplete {
      case Failure(_) =>
      case Success(chatMessages) =>
        chatModel.subProp(_.msgs).set(chatMessages)
    }

    screenModel
      .subProp(_.extraTurnText)
      .set(span(translatedDynamic(Translations.Game.extraTurnPopup)(_.apply())).render)
    screenModel
      .subProp(_.myBoardTitle)
      .set(span(translatedDynamic(Translations.Game.myBoardTitle)(_.apply())).render)
    screenModel
      .subProp(_.enemyBoardTitle)
      .set(span(translatedDynamic(Translations.Game.enemyBoardTitle)(_.apply())).render)
    screenModel
      .subProp(_.realEnemyBoardTitle)
      .set(span(translatedDynamic(Translations.Game.realEnemyBoardTitle)(_.apply())).render)
    screenModel
      .subProp(_.previewBoardTitle)
      .set(span(translatedDynamic(Translations.Game.previewBoardTitle)(_.apply())).render)

    initializePreGameModel()
  }

  private def initializePreGameModel(): Unit = {
    val (boardSize, fleet) = Fleet.default10By10
    preGameModel.subProp(_.boardSize).set(boardSize)
    val shipCounter = preGameModel.get.shipCounter
    fleet.shipsCounter.foreach { case (shipId, counter) =>
      shipCounter(shipId).set(counter)
    }
    preGameModel
      .subProp(_.timeLimit)
      .set(
        Some(
          RuleTimeLimit(
            initialTotalTimeSeconds = 600,
            additionalTurnTimeSeconds = Some((10, false))
          )
        )
      )

    val allShipCounters = shipCounter.toList
    val combinedShipCounters = allShipCounters.map(_._2).combineToSeqProperty

    combine(combinedShipCounters, preGameModel.subProp(_.boardSize)).listen(
      { case (_, boardSize) =>
        val possibleCoorLazyList: LazyList[(Coordinate, Rotation)] =
          LazyList.from(
            for {
              x <- 0 until boardSize.x
              y <- 0 until boardSize.y
              rotation <- Rotation.all
            } yield (Coordinate(x, y), rotation)
          )

        def tryPlacingAllPreviewShips(): Option[(Board, Int)] = {
          val sortedShipCounters: List[(ShipId, Property[Int])] =
            allShipCounters.sortBy { case (shipId, _) => -Ship.allShipsMap(shipId).piecesSize }

          val initialBoard = Board(boardSize, Nil)

          def placePreviewShip(board: Board, ship: Ship): Option[Board] = {
            val result: Option[(Coordinate, Ship)] =
              Random
                .shuffle(possibleCoorLazyList)
                .map { case (coordinate, rotation) => (coordinate, ship.rotateTo(rotation)) }
                .find { case (coordinate, shipRotated) =>
                  canPlaceInBoard(board, shipRotated, coordinate)
                }

            result match {
              case None =>
                None
              case Some((coordinate, shipRotated)) =>
                Some(board.addShip(ShipInBoard(shipRotated, coordinate)))
            }
          }

          @tailrec
          def placeAll(board: Board, ships: List[Ship]): Option[Board] = {
            ships match {
              case Nil =>
                Some(board)
              case headShip :: next =>
                placePreviewShip(board, headShip) match {
                  case None =>
                    None
                  case Some(updatedBoard) =>
                    placeAll(updatedBoard, next)
                }
            }
          }

          @tailrec
          def placeAllLimited(
              initialBoard: Board,
              initialShips: List[Ship],
              tryCounter: Int,
              foundBoardOpt: Option[Board],
              successCounter: Int
          ): Option[(Board, Int)] =
            if (tryCounter < PreGameModel.MaxPreviewTries)
              placeAll(initialBoard, initialShips) match {
                case None =>
                  placeAllLimited(
                    initialBoard,
                    initialShips,
                    tryCounter + 1,
                    foundBoardOpt,
                    successCounter
                  )
                case Some(updatedBoard) =>
                  placeAllLimited(
                    initialBoard,
                    initialShips,
                    tryCounter + 1,
                    foundBoardOpt.orElse(Some(updatedBoard)),
                    successCounter + 1
                  )
              }
            else
              foundBoardOpt.map((_, successCounter))

          placeAllLimited(
            initialBoard,
            sortedShipCounters.flatMap { case (shipId, countProperty) =>
              List.fill(countProperty.get)(Ship.allShipsMap(shipId))
            },
            tryCounter = 0,
            foundBoardOpt = None,
            successCounter = 0
          )
        }

        val previewBoardOptProperty =
          preGameModel.subProp(_.previewBoardOpt)
        val updatedPreviewBoardOpt =
          tryPlacingAllPreviewShips()

        previewBoardOptProperty.set(updatedPreviewBoardOpt)
      },
      initUpdate = true
    )
  }

  def onCanvasResize(boardView: BoardView): Boolean = {
    screenModel.subProp(_.screenResized).set((), force = true)

    val width = Math.max(500, boardView.canvasDiv.clientWidth)
    val height = BoardView.CanvasSize.y
    screenModel.subProp(_.canvasSize).set(Coordinate(width, height))
    if (width != boardView.myBoardCanvas.width || height != boardView.myBoardCanvas.height) {
      boardView.myBoardCanvas.setAttribute("width", width.toString)
      boardView.myBoardCanvas.setAttribute("height", height.toString)
      true
    } else
      false
  }

  override def onClose(): Unit = {
    // remove callbacks from NotificationsCenter before exit
    msgCallback.cancel()

    onQuitGameCallback.cancel()
    onGameStateCallback.cancel()
    onGameModeCallback.cancel()
  }

  def sendMsg(): Unit = {
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, _)) =>
        val msgProperty = chatModel.subProp(_.msgInput)
        val msg = msgProperty.get.trim
        msgProperty.set("")
        if (msg.nonEmpty)
          gameRpc.sendMsg(gameId, msg)
      case _ =>
    }
  }

  private def createCurrentRules: Option[Rules] = {
    val preGame = preGameModel.get
    if (preGame.previewBoardOpt.exists(_._2 >= PreGameModel.MinPreviewTries)) {
      val defaultTurnAttackTypes = List.fill(3)(AttackType.Simple)
      val turnBonuses: List[TurnBonus] =
        List(
          TurnBonus(BonusType.FirstBlood, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
          TurnBonus(BonusType.DoubleKill, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
          TurnBonus(BonusType.TripleKill, List(ExtraTurn(List.fill(3)(AttackType.Simple))))
        )

      val gameFleet: Fleet =
        Fleet.fromShips(
          preGame.shipCounter.toList.flatMap { case (shipId, counterProperty) =>
            List.fill(counterProperty.get)(Ship.allShipsMap(shipId))
          }
        )

      Some(
        Rules(
          boardSize = preGame.boardSize,
          gameFleet = gameFleet,
          defaultTurnAttackTypes = defaultTurnAttackTypes,
          turnBonuses = turnBonuses,
          timeLimit = preGame.timeLimit
        )
      )
    } else
      None
  }

  def showErrorModal(): Unit = {
    screenModel.subProp(_.showErrorModal).set(true)
  }

  def startGameWithBots(): Unit = {
    createCurrentRules match {
      case None =>
        showErrorModal()
      case Some(rules) =>
        gameRpc.startGameWithBots(rules)
    }
  }

  def startGameWith(otherPlayerUsername: Username): Unit = {
    if (otherPlayerUsername.username.nonEmpty) {
      createCurrentRules match {
        case None =>
          showErrorModal()
        case Some(rules) =>
          gameRpc.startGameWith(otherPlayerUsername, rules)
      }
    }
  }

  def rematchGame(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, _)) =>
        gameRpc.rematchGame(gameId)
      case _ =>
    }

  def logout(): Unit =
    gameStateProperty.get match {
      case None =>
        Cookies.clearCookies()
        for {
          _ <- gameRpc.logout()
          _ <- userService.logout()
        } yield application.goTo(RoutingLoginPageState)
      case _ =>
    }

  def quitCurrentGame(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, _)) =>
        gameRpc.quitCurrentGame(gameId)
      case _ =>
    }

  def mouseMove(boardView: BoardView, mouseX: Int, mouseY: Int): Unit = {
    gameModel.subProp(_.mousePosition).set(Some(Coordinate(mouseX, mouseY)))

    (gameModel.get, gameStateProperty.get) match {
      case (
            GameModel(Some(_), Some(button @ (0 | 2)), _, _, _, selectedBoardMarkOpt, _, _),
            Some(
              gameState @ GameState(
                gameId,
                _,
                me,
                _,
                PlayingMode(_, _, _, _, _) | GameOverMode(_, _, _, _, _)
              )
            )
          ) =>
        (boardView.enemyBoardMouseCoordinate.get, selectedBoardMarkOpt, button) match {
          case (Some(enemyBoardCoor), _, 2) =>
            val (_, currentBoardMark) = me.enemyBoardMarks(enemyBoardCoor.x)(enemyBoardCoor.y)

            if (!currentBoardMark.isPermanent && currentBoardMark != BoardMark.Empty) {
              gameStateProperty.set(
                Some(gameState.copy(me = me.updateBoardMark(enemyBoardCoor, BoardMark.Empty)))
              )
              gameRpc.sendBoardMarks(gameId, List((enemyBoardCoor, BoardMark.Empty)))
            }
          case (Some(enemyBoardCoor), Some(selectedBoardMark), 0) =>
            val (_, currentBoardMark) = me.enemyBoardMarks(enemyBoardCoor.x)(enemyBoardCoor.y)

            if (!currentBoardMark.isPermanent && currentBoardMark != selectedBoardMark) {
              gameStateProperty.set(
                Some(gameState.copy(me = me.updateBoardMark(enemyBoardCoor, selectedBoardMark)))
              )
              gameRpc.sendBoardMarks(gameId, List((enemyBoardCoor, selectedBoardMark)))
            }
          case _ =>
        }
      case _ =>
    }
  }

  def mouseLeave(): Unit =
    gameModel.subProp(_.mousePosition).set(None)

  def mouseUp(): Unit =
    gameModel.subProp(_.mouseDown).set(None)

  def mouseDown(boardView: BoardView, button: Int): Unit = {
    gameModel.subProp(_.mouseDown).set(Some(button))

    (gameModel.get, gameStateProperty.get) match {
      case (
            GameModel(Some(_), Some(0), selectedShipOpt, _, _, _, _, _),
            Some(gameState @ GameState(_, _, me, _, _: PreGameMode))
          ) =>
        boardView.shipToPlaceHover.get.map(_.ship) match {
          case Some(ship) =>
            gameModel.subProp(_.selectedShip).set(Some(ship))
          case None =>
            (selectedShipOpt, boardView.myBoardMouseCoordinate.get) match {
              case (Some(ship), Some(boardCoor)) =>
                val roundedBoardCoor =
                  boardCoor.roundTo(me.myBoard.boardSize - ship.size + Coordinate(1, 1))

                tryToPlaceShips(gameState, me, ship, roundedBoardCoor)
              case (Some(_), None) =>
                gameModel.subProp(_.selectedShip).set(None)
              case _ =>
            }
        }
      case (
            GameModel(Some(_), Some(2), _, _, _, _, _, _),
            Some(
              gameState @ GameState(
                gameId,
                _,
                me,
                _,
                PlayingMode(_, _, _, _, _) | GameOverMode(_, _, _, _, _)
              )
            )
          ) =>
        boardView.enemyBoardMouseCoordinate.get match {
          case Some(enemyBoardCoor) =>
            val (_, currentBoardMark) = me.enemyBoardMarks(enemyBoardCoor.x)(enemyBoardCoor.y)

            if (!currentBoardMark.isPermanent && currentBoardMark != BoardMark.Empty) {
              gameStateProperty.set(
                Some(gameState.copy(me = me.updateBoardMark(enemyBoardCoor, BoardMark.Empty)))
              )
              gameRpc.sendBoardMarks(gameId, List((enemyBoardCoor, BoardMark.Empty)))
            }
          case _ =>
        }
      case (
            GameModel(
              Some(_),
              Some(0),
              selectedShip,
              turnAttacks,
              turnAttacksSent,
              selectedBoardMarkOpt,
              _,
              _
            ),
            Some(
              gameState @ GameState(
                gameId,
                _,
                me,
                _,
                PlayingMode(_, _, _, _, _) | GameOverMode(_, _, _, _, _)
              )
            )
          ) =>
        (
          boardView.enemyBoardMouseCoordinate.get,
          boardView.boardMarkHover.get,
          selectedBoardMarkOpt
        ) match {
          case (Some(enemyBoardCoor), None, Some(selectedBoardMark)) =>
            val (_, currentBoardMark) = me.enemyBoardMarks(enemyBoardCoor.x)(enemyBoardCoor.y)

            if (!currentBoardMark.isPermanent) {
              val updatedBoardMark =
                if (currentBoardMark == selectedBoardMark)
                  BoardMark.Empty
                else
                  selectedBoardMark

              gameStateProperty.set(
                Some(gameState.copy(me = me.updateBoardMark(enemyBoardCoor, updatedBoardMark)))
              )
              gameRpc.sendBoardMarks(gameId, List((enemyBoardCoor, updatedBoardMark)))
            }
          case (Some(enemyBoardCoor), None, None)
              if gameState.gameMode.isPlaying &&
                !turnAttacksSent && isValidCoordinateTarget(enemyBoardCoor) =>
            def setFirstMissile(turnAttacks: List[Attack]): List[Attack] =
              turnAttacks match {
                case Nil =>
                  Nil
                case (headAttack @ Attack(_, None)) :: next =>
                  headAttack.copy(coordinateOpt = Some(enemyBoardCoor)) :: next
                case headAttack :: next =>
                  headAttack :: setFirstMissile(next)
              }

            val turnAttackUpdated: List[Attack] =
              if (turnAttacks.exists(_.coordinateOpt.contains(enemyBoardCoor)))
                turnAttacks.map {
                  case Attack(attackType, Some(coordinate)) if coordinate == enemyBoardCoor =>
                    Attack(attackType, None)
                  case other =>
                    other
                }
              else
                setFirstMissile(turnAttacks)

            gameModel.subProp(_.turnAttacks).set(turnAttackUpdated)
          case (None, Some(boardMarkClicked), selectedBoardMarkOpt)
              if !selectedBoardMarkOpt.contains(boardMarkClicked) =>
            gameModel.subProp(_.selectedBoardMarkOpt).set(Some(boardMarkClicked))
          case (None, None, Some(_)) =>
            gameModel.subProp(_.selectedBoardMarkOpt).set(None)
          case _ =>
        }

        (
          boardView.enemyBoardMouseCoordinate.get,
          boardView.boardMarkHover.get,
          selectedShip,
          boardView.summaryShipHover.get
        ) match {
          case (_, _, Some(currentSelectedShip), Some(summaryShipHover))
              if currentSelectedShip.shipId == summaryShipHover.shipId =>
            gameModel.subProp(_.selectedShip).set(None)
          case (None, None, Some(_), None) =>
            gameModel.subProp(_.selectedShip).set(None)
          case (_, _, _, Some(summaryShipHover)) =>
            gameModel.subProp(_.selectedShip).set(Some(summaryShipHover))
          case _ =>
        }
      case _ =>
    }
  }

  private def tryToPlaceShips(
      gameState: GameState,
      me: Player,
      ship: Ship,
      coordinate: Coordinate
  ): Boolean =
    if (
      me.shipsLeftToPlace.exists(_.shipId == ship.shipId) &&
      canPlaceInBoard(me.myBoard, ship, coordinate)
    ) {
      val playerUpdated =
        me
          .modify(_.shipsLeftToPlace)
          .using(removeOneShip(ship.shipId, _))
          .modify(_.myBoard)
          .using(_.addShip(ShipInBoard(ship, coordinate)))

      gameStateProperty.set(Some(gameState.copy(me = playerUpdated)))

      val nextShipOpt =
        if (playerUpdated.shipsLeftToPlace.exists(_.shipId == ship.shipId))
          Some(ship)
        else
          playerUpdated.shipsLeftToPlace.headOption

      gameModel.subProp(_.selectedShip).set(nextShipOpt)

      true
    } else
      false

  def keyDown(key: String): Unit = {
    if (key == "R")
      rotateSelectedShip(1)
  }

  def mouseWheel(wheelRotation: Int): Unit =
    gameStateProperty.get match {
      case Some(GameState(_, _, _, _, _: PreGameMode)) =>
        if (wheelRotation != 0)
          rotateSelectedShip(wheelRotation)
      case Some(GameState(_, _, _, _, _: PlayingMode)) =>
        val nextIndex =
          gameModel.get.selectedBoardMarkOpt.flatMap(boardMark =>
            BoardView.BoardMarksSelectorOrder.zipWithIndex.find(_._1 == boardMark)
          ) match {
            case None if wheelRotation > 0 =>
              Some(0)
            case None if wheelRotation < 0 =>
              Some(BoardView.BoardMarksSelectorOrder.size - 1)
            case Some((_, currentIndex)) =>
              Some(currentIndex + wheelRotation)
                .filter(index => index >= 0 && index < BoardView.BoardMarksSelectorOrder.size)
          }
        gameModel
          .subProp(_.selectedBoardMarkOpt)
          .set(nextIndex.map(BoardView.BoardMarksSelectorOrder))
      case _ =>
    }

  private def rotateSelectedShip(directionDelta: Int): Unit =
    (gameModel.get.selectedShip, modeTypeProperty.get) match {
      case (Some(ship), Some(PreGameModeType)) =>
        gameModel
          .subProp(_.selectedShip)
          .set(Some(ship.rotateBy(directionDelta)))
      case _ =>
    }

  private def removeOneShip(shipId: ShipId, list: List[Ship]): List[Ship] =
    list match {
      case Nil =>
        Nil
      case headShip :: next if headShip.shipId == shipId =>
        next
      case headShip :: next =>
        headShip :: removeOneShip(shipId, next)
    }

  def cancelShipsPlacement(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, PreGameMode(true, _))) =>
        gameRpc.cancelShipsPlacement(gameId)
      case _ =>
    }

  def confirmShipPlacement(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, me, _, _)) =>
        gameRpc.confirmShips(gameId, me.myBoard.ships)
      case _ =>
    }

  def undoLastPlacedShip(): Unit =
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, _, me, _, _)) =>
        val undoShip: Option[Ship] =
          me.myBoard.ships match {
            case Nil =>
              None
            case ShipInBoard(headShip, _) :: _ =>
              Some(headShip)
          }

        gameStateProperty.set(
          Some(
            undoShip match {
              case None =>
                gameState
              case Some(headShip) =>
                gameState.copy(me =
                  me.modify(_.shipsLeftToPlace)
                    .using(headShip :: _)
                    .modify(_.myBoard)
                    .using(_.removeLastShip)
                )
            }
          )
        )

        gameModel.subProp(_.selectedShip).set(undoShip)

        cancelShipsPlacement()
      case _ =>
    }

  def resetPlacedShips(): Unit =
    gameStateProperty.get match {
      case Some(
            gameState @ GameState(
              _,
              Rules(_, Fleet(shipsInThisGame), _, _, _),
              me,
              _,
              _: PreGameMode
            )
          ) if me.myBoard.ships.nonEmpty =>
        val meUpdated: Player =
          me.modify(_.shipsLeftToPlace)
            .setTo(shipsInThisGame)
            .modify(_.myBoard)
            .using(_.resetBoard)

        gameStateProperty.set(Some(gameState.copy(me = meUpdated)))
        gameModel.get.selectedShip match {
          case None =>
            gameModel.subProp(_.selectedShip).set(meUpdated.shipsLeftToPlace.headOption)
          case Some(_) =>
        }

        cancelShipsPlacement()
      case _ =>
    }

  def randomPlacement(): Unit =
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, _, me, _, _: PreGameMode)) =>
        me.shipsLeftToPlace match {
          case headShip :: _ =>
            val possibleCoorList =
              for {
                x <- 0 until me.myBoard.boardSize.x
                y <- 0 until me.myBoard.boardSize.y
                rotation <- Rotation.all
              } yield (Coordinate(x, y), rotation)
            val result =
              Random
                .shuffle(possibleCoorList)
                .find { case (coor, rotation) =>
                  tryToPlaceShips(gameState, me, headShip.rotateTo(rotation), coor)
                }

            if (result.nonEmpty)
              randomPlacement()
          case Nil =>
        }
      case _ =>
    }

  def launchAttack(): Unit =
    (gameModel.get.turnAttacks, gameStateProperty.get) match {
      case (
            turnAttacks,
            Some(GameState(gameId, _, _, _, PlayingMode(_, turn, _, _, _)))
          ) if turnAttacks.forall(_.isPlaced) =>
        gameModel.subProp(_.turnAttacksSent).set(true)
        gameRpc.sendTurnAttacks(gameId, turn, turnAttacks)

        stopNewTurnAnimation()
      case _ =>
    }

  def isValidCoordinateTarget(enemyBoardCoor: Coordinate): Boolean =
    gameStateProperty.get match {
      case Some(GameState(_, _, me, _, _: PlayingMode)) =>
        val Coordinate(x, y) = enemyBoardCoor
        val (turnNumberOpt, boardMark) = me.enemyBoardMarks(x)(y)
        turnNumberOpt.isEmpty && !boardMark.isPermanent
      case _ =>
        false
    }

  private def updateLastSeen(selectedTab: String): Unit =
    if (selectedTab == ScreenModel.chatTab)
      screenModel.subProp(_.lastSeenMessagesChat).set(chatMessagesSizeProperty.get)
    else if (selectedTab == ScreenModel.myMovesTab)
      screenModel.subProp(_.lastSeenMessagesMyMoves).set(myMovesHistorySizeProperty.get)
    else if (selectedTab == ScreenModel.enemyMovesTab)
      screenModel.subProp(_.lastSeenMessagesEnemyMoves).set(enemyMovesHistorySizeProperty.get)

  def setSelectedTab(selectedTab: String): Unit = {
    updateLastSeen(selectedTabProperty.get)
    selectedTabProperty.set(selectedTab)
    updateLastSeen(selectedTabProperty.get)
  }

  def getPreGameShipProperty(shipId: ShipId): Property[Int] =
    preGameModel.get.shipCounter(shipId)

}
