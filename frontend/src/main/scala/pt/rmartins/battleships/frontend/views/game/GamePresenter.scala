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
import pt.rmartins.battleships.frontend.views.game.BoardView.InGameMarkSelector
import pt.rmartins.battleships.frontend.views.game.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.frontend.views.game.ModeType._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.GameMode._
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils
import pt.rmartins.battleships.shared.model.utils.BoardUtils.{BoardMarks, canPlaceInBoard}
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

  val preGameRulesProperty: Property[Rules] =
    preGameModel.subProp(_.rules)

  val gameStateProperty: Property[Option[GameState]] =
    gameStateModel.bitransform(_.gameState)(GameStateModel(_))

  val enemyProperty: ReadableProperty[Option[SimplePlayer]] =
    gameStateModel.transform(_.gameState.map(_.enemy))

  val enemyUsernameProperty: ReadableProperty[Option[Username]] =
    combine(
      preGameModel.subProp(_.inJoinedPreGame).transform(_.flatMap(_.enemyUsernameOpt)),
      gameStateModel.transform(_.gameState.map(_.enemy.username))
    ).transform { case (preGameEnemy, inGameEnemy) =>
      preGameEnemy.orElse(inGameEnemy)
    }

  val gameModeProperty: Property[Option[GameMode]] =
    gameStateProperty.bitransform[Option[GameMode]](_.map(_.gameMode)) {
      case None =>
        gameStateModel.get.gameState
      case Some(gameMode) =>
        gameStateModel.get.gameState.map(_.copy(gameMode = gameMode))
    }
  val modeTypeProperty: ReadableProperty[Option[ModeType]] =
    gameModeProperty.transform(_.map {
      case _: PlacingShipsMode => PreGameModeType
      case _: PlayingMode      => PlayingModeType
      case _: GameOverMode     => GameOverModeType
    })

  val placingShipsModeProperty: Property[Option[PlacingShipsMode]] =
    gameModeProperty.bitransform[Option[PlacingShipsMode]] {
      case Some(placingShipsMode: PlacingShipsMode) => Some(placingShipsMode)
      case _                                        => None
    }(identity)

  val playingModeProperty: Property[Option[PlayingMode]] =
    gameModeProperty.bitransform[Option[PlayingMode]] {
      case Some(playingMode: PlayingMode) => Some(playingMode)
      case _                              => None
    }(identity)

  val rulesProperty: ReadableProperty[Option[Rules]] =
    gameStateProperty.transform(_.map(_.rules))

  val inPlacingShipsMode: ReadableProperty[Boolean] =
    gameModeProperty.transform(_.exists(_.isPlacingShips))

  val inPlayingMode: ReadableProperty[Boolean] =
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

  private val onUpdatePreGameCallback = notificationsCenter.onUpdatePreGameState {
    case PreGameState(gameId, enemyUsername, confirmRules, enemyConfirmedRules, updateRules) =>
      preGameModel
        .subProp(_.inJoinedPreGame)
        .set(
          Some(
            PlayingAgainstPlayer(
              gameId,
              confirmed = confirmRules,
              enemyConfirmed = enemyConfirmedRules,
              enemyUsername
            )
          )
        )
      preGameModel.subProp(_.rules).set(updateRules)
  }

  private val onPreGameRulesPatchCallback = notificationsCenter.onPreGameRulesPatch {
    case PreGameRulesPatch(Some(boardSizePatch), _, _, _, _) =>
      preGameRulesProperty.set(preGameRulesProperty.get.copy(boardSize = boardSizePatch))
    case PreGameRulesPatch(_, Some((shipId, counter)), _, _, _) =>
      preGameRulesProperty.set(
        preGameRulesProperty.get
          .modify(_.gameFleet)
          .using(fleet =>
            Fleet(fleet.shipCounterMap.updated(shipId, (counter, Rotation.Rotation0)).toList)
          )
      )
    case PreGameRulesPatch(_, _, Some(defaultTurnAttacksPatch), _, _) =>
      preGameRulesProperty.set(
        preGameRulesProperty.get.copy(defaultTurnAttacks = defaultTurnAttacksPatch)
      )
    case PreGameRulesPatch(_, _, _, Some(turnBonusesPatch), _) =>
      preGameRulesProperty.set(preGameRulesProperty.get.copy(turnBonuses = turnBonusesPatch))
    case PreGameRulesPatch(_, _, _, _, Some(timeLimitPatch)) =>
      preGameRulesProperty.set(preGameRulesProperty.get.copy(timeLimit = timeLimitPatch))
    case _ =>
  }

  private val onPreGameConfirmStateCallback =
    notificationsCenter.onPreGameConfirmState { case (confirmState, enemyConfirmState) =>
      preGameModel.subProp(_.inJoinedPreGame).get match {
        case Some(playingAgainstPlayer @ PlayingAgainstPlayer(_, _, _, _)) =>
          preGameModel
            .subProp(_.inJoinedPreGame)
            .set(
              Some(
                playingAgainstPlayer.copy(
                  confirmed = confirmState,
                  enemyConfirmed = enemyConfirmState
                )
              )
            )
        case _ =>
      }
    }

  private val onQuitGameCallback = notificationsCenter.onQuitGame { case _ =>
    if (preGameModel.get.inJoinedPreGame.nonEmpty)
      clearPreGame()
    else {
      val updatedGameState = None
      updateGameState(gameStateProperty.get.map(_.gameMode), updatedGameState)
      gameModel.set(GameModel.default)
      gameStateProperty.set(updatedGameState)
      screenModel.set(ScreenModel.resetScreenModel(screenModel.get))
    }
  }

  private val onGameStateCallback = notificationsCenter.onGameState { case updatedGameState =>
    val currentGameState = gameStateProperty.get
    val mergedGameState: GameState = mergeGameState(currentGameState, updatedGameState)
    updateGameState(currentGameState.map(_.gameMode), Some(mergedGameState))
    gameStateProperty.set(Some(mergedGameState))
  }

  private val onGameModeCallback = notificationsCenter.onGameMode { case updatedGameMode =>
    val currentGameState = gameStateProperty.get
    val updatedGameState: Option[GameState] =
      currentGameState.map(_.copy(gameMode = updatedGameMode))
    val finalGameState: Option[GameState] =
      updatedGameState match {
        case None =>
          updateGameState(currentGameState.map(_.gameMode), None)
          None
        case Some(updatedGameState) =>
          val mergedGameState: GameState = mergeGameState(currentGameState, updatedGameState)
          updateGameState(currentGameState.map(_.gameMode), Some(mergedGameState))
          Some(mergedGameState)
      }
    gameStateProperty.set(finalGameState)
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

  val shipCounter: Map[ShipId, Property[Int]] =
    Ship.allShipsList.map(_.shipId -> Property(0)).toMap

  preGameModel
    .subProp(_.rules)
    .transform(_.gameFleet.shipCounterList)
    .listen(
      {
        _.map { case (shipId, (amount, _)) =>
          shipCounter(shipId).set(amount)
        }
      },
      initUpdate = true
    )

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

  private def clearPreGame(): Unit = {
    preGameModel.subProp(_.inJoinedPreGame).set(None)
  }

  private def mergeGameState(
      currentGameStateOpt: Option[GameState],
      updatedGameState: GameState
  ): GameState =
    (currentGameStateOpt, updatedGameState) match {
      case (
            Some(GameState(_, _, me, _, _: PlacingShipsMode)),
            GameState(_, _, _, _, _: PlacingShipsMode)
          ) =>
        updatedGameState.modify(_.me.myBoard).setTo(me.myBoard)
      case _ =>
        updatedGameState
    }

  private def updateGameState(
      fromGameState: Option[GameMode],
      toGameState: Option[GameState]
  ): Unit = {
    (fromGameState, toGameState) match {
      case (None, Some(GameState(_, rules, _, _, _: PlacingShipsMode))) =>
        clearPreGame()
        val shipsToPlace: List[Ship] =
          rules.gameFleet.shipsList.sortBy(_.shipBiggestToSmallestOrder)
        gameModel
          .subProp(_.shipsLeftToPlace)
          .set(shipsToPlace)
        shipsToPlace.headOption.foreach { headShip =>
          gameModel.subProp(_.selectedShip).set(Some(headShip))
        }
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
        setLineDashOffsetInterval(activate = false)

        selectedTabProperty.set(ScreenModel.chatTab)
        screenModel.subProp(_.lastSeenMessagesChat).set(chatMessagesSizeProperty.get)
        screenModel.subProp(_.screenResized).set((), force = true)
      case (
            None | Some(_: PlacingShipsMode),
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

              screenModel.subProp(_.tick).set(screenModel.get.tick + 1)
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

    toGameState.map(_.gameMode) match {
      case Some(PlayingMode(_, _, _, Some(myTimeRemaining), Some(enemyTimeRemaining))) =>
        gameModel.subProp(_.timeRemaining).set(Some((myTimeRemaining, enemyTimeRemaining)))
      case Some(GameOverMode(_, _, Some(myTimeRemaining), Some(enemyTimeRemaining), _)) =>
        gameModel.subProp(_.timeRemaining).set(Some((myTimeRemaining, enemyTimeRemaining)))
      case _ =>
        gameModel.subProp(_.timeRemaining).set(None)
    }

    (fromGameState, toGameState) match {
      case (Some(_: PlayingMode), Some(GameState(_, _, _, _, _: PlayingMode))) =>
        screenModel.subProp(_.newTurn).set((), force = true)
      case _ =>
    }

    (fromGameState, toGameState) match {
      case (None, Some(GameState(_, rules, _, _, _))) =>
        preGameModel.subProp(_.rules).set(rules)
        Ship.allShipsList.foreach { ship =>
          val shipId = ship.shipId
          shipCounter(shipId).set(
            rules.gameFleet.shipCounterMap.get(shipId).map(_._1).getOrElse(0)
          )
        }
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
    .transform(
      _.map(inGameMode => (inGameMode.isMyTurn, inGameMode.turn, inGameMode.turnAttackTypes))
    )
    .listen {
      case Some((isMyTurn, Turn(_, extraTurn), turnAttackTypes)) =>
        gameModel.get.turnAttacksQueuedStatus match {
          case AttacksQueuedStatus.NotSet =>
          case AttacksQueuedStatus.Queued if isMyTurn =>
            launchAttack()
          case AttacksQueuedStatus.Sent =>
            gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.NotSet)
            gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))
            screenModel
              .subProp(_.missilesPopupMillisOpt)
              .set(Some(BoardView.MissilesInitialPopupTime))
            screenModel
              .subProp(_.extraTurnPopup)
              .set(extraTurn.map(_ => BoardView.ExtraTurnPopupTime))

            newTurnAnimationHandle.foreach(window.clearInterval)
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
    val allShipCounters = shipCounter.toList
    val combinedShipCounters = allShipCounters.map(_._2).combineToSeqProperty

    val gameIdOpt: ReadableProperty[Option[GameId]] =
      preGameModel.subProp(_.inJoinedPreGame).transform {
        case Some(PlayingAgainstPlayer(gameId, _, _, _)) => Some(gameId)
        case _                                           => None
      }

    combinedShipCounters.listen { _ =>
      val gameFleet: Fleet =
        Fleet.fromShipIds(
          allShipCounters.map { case (shipId, countProperty) =>
            shipId -> countProperty.get
          }
        )
      preGameRulesProperty.set(preGameRulesProperty.get.copy(gameFleet = gameFleet))
    }

    preGameRulesProperty.transform(_.boardSize).listen { updatedBoardSize =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(gameId, PreGameRulesPatch(boardSizePatch = Some(updatedBoardSize)))
      }
    }

    allShipCounters.foreach { case (shipId, counterProperty) =>
      counterProperty.listen { counter =>
        gameIdOpt.get.foreach { gameId =>
          gameRpc.sendRulesPatch(
            gameId,
            PreGameRulesPatch(gameFleetPatch = Some((shipId, counter)))
          )
        }
      }
    }

    preGameRulesProperty.transform(_.defaultTurnAttacks).listen { updatedDefaultTurnAttacks =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(
          gameId,
          PreGameRulesPatch(defaultTurnAttacksPatch = Some(updatedDefaultTurnAttacks))
        )
      }
    }

    preGameRulesProperty.transform(_.timeLimit).listen { updatedTimeLimit =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(
          gameId,
          PreGameRulesPatch(timeLimitPatch = Some(updatedTimeLimit))
        )
      }
    }

    preGameRulesProperty.transform(_.turnBonuses).listen { updatedTurnBonuses =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(
          gameId,
          PreGameRulesPatch(turnBonusesPatch = Some(updatedTurnBonuses))
        )
      }
    }

    combine(combinedShipCounters, preGameRulesProperty.transform(_.boardSize)).listen(
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

    onUpdatePreGameCallback.cancel()
    onPreGameRulesPatchCallback.cancel()
    onPreGameConfirmStateCallback.cancel()
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

  private def createCurrentRules: Either[ErrorModalType, Rules] = {
    val preGame = preGameModel.get
    if (preGame.previewBoardOpt.exists(_._2 < PreGameModel.MinPreviewTries))
      Left(ErrorModalType.SmallBoardError)
    else if (preGame.rules.gameFleet.shipAmount == 0)
      Left(ErrorModalType.EmptyFleetError)
    else {
      Right(preGame.rules)
    }
  }

  def showErrorModal(errorModalType: ErrorModalType): Unit = {
    screenModel.subProp(_.errorModalType).set(Some(errorModalType), force = true)
  }

  def startGameWithBots(): Unit = {
    createCurrentRules match {
      case Left(errorModalType) => showErrorModal(errorModalType)
      case Right(rules)         => gameRpc.startGameWithBots(rules)
    }
  }

  def invitePlayer(otherPlayerUsername: Username): Unit =
    if (otherPlayerUsername.username.nonEmpty)
      createCurrentRules match {
        case Left(errorModalType) => showErrorModal(errorModalType)
        case Right(rules)         => gameRpc.invitePlayer(otherPlayerUsername, rules)
      }

  def confirmRules(): Unit =
    preGameModel.subProp(_.inJoinedPreGame).get match {
      case Some(playingAgainstPlayer @ PlayingAgainstPlayer(gameId, false, _, _)) =>
        preGameModel
          .subProp(_.inJoinedPreGame)
          .set(Some(playingAgainstPlayer.copy(confirmed = true)))
        gameRpc.confirmRules(gameId)
      case _ =>
    }

  def cancelRules(): Unit =
    preGameModel.subProp(_.inJoinedPreGame).get match {
      case Some(playingAgainstPlayer @ PlayingAgainstPlayer(gameId, true, _, _)) =>
        preGameModel
          .subProp(_.inJoinedPreGame)
          .set(Some(playingAgainstPlayer.copy(confirmed = false)))
        gameRpc.cancelRules(gameId)
      case _ =>
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
    preGameModel.get.inJoinedPreGame
      .flatMap(_.gameIdOpt)
      .orElse(gameStateProperty.get.map(_.gameId)) match {
      case Some(gameId) =>
        gameRpc.quitCurrentGame(gameId)
      case _ =>
    }

  def mouseMove(boardView: BoardView, mouseX: Int, mouseY: Int): Unit = {
    boardView.myBoardCanvas.setAttribute("tabindex", "0")
    boardView.myBoardCanvas.focus()

    gameModel.subProp(_.mousePosition).set(Some(Coordinate(mouseX, mouseY)))

    (gameModel.get, gameStateProperty.get) match {
      case (
            GameModel(_, Some(_), Some(button @ (0 | 2)), _, _, _, selectedBoardMarkOpt, _, _),
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
            val boardMarksOpt: Option[BoardMark] =
              (selectedBoardMark match {
                case InGameMarkSelector.ManualShipSelector =>
                  Some(BoardMark.ManualShip)
                case InGameMarkSelector.ManualWaterSelector =>
                  Some(BoardMark.ManualWater)
                case _ =>
                  None
              }).filter { boardMark =>
                val (_, currentBoardMark) = me.enemyBoardMarks(enemyBoardCoor.x)(enemyBoardCoor.y)
                !currentBoardMark.isPermanent && boardMark != currentBoardMark
              }

            boardMarksOpt.foreach { boardMark =>
              gameStateProperty.set(
                Some(gameState.copy(me = me.updateBoardMark(enemyBoardCoor, boardMark)))
              )
              gameRpc.sendBoardMarks(gameId, List((enemyBoardCoor, boardMark)))
            }
          case _ =>
        }
      case _ =>
    }
  }

  def mouseLeave(): Unit = {
    gameModel.subProp(_.mousePosition).set(None)
    mouseUp()
  }

  def mouseUp(): Unit =
    gameModel.subProp(_.mouseDown).set(None)

  def mouseDown(boardView: BoardView, button: Int): Unit = {
    boardView.myBoardCanvas.setAttribute("tabindex", "0")
    boardView.myBoardCanvas.focus()

    gameModel.subProp(_.mouseDown).set(Some(button))

    (gameModel.get, gameStateProperty.get) match {
      case (
            GameModel(_, Some(_), Some(0), selectedShipOpt, _, _, _, _, _),
            Some(gameState @ GameState(_, _, me, _, _: PlacingShipsMode))
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
            GameModel(_, Some(_), Some(2), selectedShip, turnAttacks, _, _, _, _),
            Some(
              gameState @ GameState(
                gameId,
                _,
                me,
                _,
                mode @ (PlayingMode(_, _, _, _, _) | GameOverMode(_, _, _, _, _))
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

            if (mode.isPlaying && turnAttacks.exists(_.coordinateOpt.contains(enemyBoardCoor))) {
              val turnAttackUpdated: List[Attack] =
                turnAttacks.map {
                  case Attack(attackType, Some(coordinate)) if coordinate == enemyBoardCoor =>
                    Attack(attackType, None)
                  case other =>
                    other
                }

              gameModel.subProp(_.turnAttacks).set(turnAttackUpdated)

              if (
                gameModel.get.turnAttacksQueuedStatus == AttacksQueuedStatus.Queued &&
                turnAttackUpdated.exists(!_.isPlaced)
              )
                gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.NotSet)
            }
          case None =>
            (selectedShip, boardView.summaryShipHover.get) match {
              case (Some(currentSelectedShip), Some(summaryShipHover))
                  if currentSelectedShip.shipId == summaryShipHover.shipId =>
                gameModel.subProp(_.selectedShip).set(None)
              case (Some(_), None) =>
                gameModel.subProp(_.selectedShip).set(None)
              case _ =>
            }
        }
      case (
            GameModel(
              _,
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
                Rules(boardSize, _, _, _, _),
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
          case (Some(enemyBoardCoor), None, Some(selectedInGameMark)) =>
            val updatedBoardMarksList: List[(Coordinate, BoardMark)] =
              (selectedInGameMark match {
                case InGameMarkSelector.ManualShipSelector =>
                  List((enemyBoardCoor, BoardMark.ManualShip))
                case InGameMarkSelector.ManualWaterSelector =>
                  List((enemyBoardCoor, BoardMark.ManualWater))
                case InGameMarkSelector.FillWaterSelector =>
                  def isShip(coordinate: Coordinate): Boolean =
                    me.enemyBoardMarks(coordinate.x)(coordinate.y)._2.isShip

                  @tailrec
                  def getAllNearShipCoor(
                      coorToCheck: List[Coordinate],
                      shipCoors: Set[Coordinate],
                      seen: Set[Coordinate]
                  ): Set[Coordinate] =
                    coorToCheck match {
                      case Nil => shipCoors
                      case center :: nextCoors =>
                        val newCoordinates: List[Coordinate] =
                          center.get8CoorAround.filterNot(seen).filter(_.isInsideBoard(boardSize))
                        val newShipCoordinates: List[Coordinate] =
                          newCoordinates.filter(isShip)
                        getAllNearShipCoor(
                          newShipCoordinates ++ nextCoors,
                          shipCoors ++ Set(center).filter(isShip) ++ newShipCoordinates,
                          (seen + center) ++ newCoordinates
                        )
                    }

                  val shipMarkPositions: Set[Coordinate] =
                    getAllNearShipCoor(List(enemyBoardCoor).filter(isShip), Set.empty, Set.empty)

                  if (shipMarkPositions.isEmpty)
                    Nil
                  else
                    shipMarkPositions
                      .flatMap(_.get8CoorAround)
                      .filter(_.isInsideBoard(boardSize))
                      .filterNot(shipMarkPositions)
                      .map { coor =>
                        coor -> BoardMark.ManualWater
                      }
                      .toList
              }).filter { case (coor, currentBoardMark) =>
                val boardMark = me.enemyBoardMarks(coor.x)(coor.y)._2
                !boardMark.isPermanent && boardMark != currentBoardMark
              }

            if (updatedBoardMarksList.nonEmpty) {
              val updatedBoardMarks: BoardMarks =
                updatedBoardMarksList.foldLeft(me.enemyBoardMarks) {
                  case (enemyBoardMarks, (boardCoor, newBoardMark)) =>
                    BoardUtils.updateBoardMarksUsing(
                      enemyBoardMarks,
                      boardCoor,
                      { case (turnNumberOpt, _) => (turnNumberOpt, newBoardMark) }
                    )
                }

              gameStateProperty.set(
                Some(gameState.copy(me = me.copy(enemyBoardMarks = updatedBoardMarks)))
              )
              gameRpc.sendBoardMarks(gameId, updatedBoardMarksList)
            }
          case (Some(enemyBoardCoor), None, None)
              if gameState.gameMode.isPlaying &&
                turnAttacksSent == AttacksQueuedStatus.NotSet &&
                isValidCoordinateTarget(enemyBoardCoor) =>
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
            gameModel.subProp(_.selectedInGameMarkOpt).set(Some(boardMarkClicked))
          case (None, None, Some(_)) =>
            gameModel.subProp(_.selectedInGameMarkOpt).set(None)
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
      gameModel.subProp(_.shipsLeftToPlace).get.exists(_.shipId == ship.shipId) &&
      canPlaceInBoard(me.myBoard, ship, coordinate)
    ) {
      val playerUpdated: Player =
        me
          .modify(_.myBoard)
          .using(_.addShip(ShipInBoard(ship, coordinate)))
      gameStateProperty.set(Some(gameState.copy(me = playerUpdated)))

      val updatedShipsLeftToPlace: List[Ship] =
        removeOneShip(ship.shipId, gameModel.subProp(_.shipsLeftToPlace).get)
      gameModel.subProp(_.shipsLeftToPlace).set(updatedShipsLeftToPlace)

      val nextShipOpt: Option[Ship] =
        if (updatedShipsLeftToPlace.exists(_.shipId == ship.shipId))
          Some(ship)
        else
          updatedShipsLeftToPlace.headOption
      gameModel.subProp(_.selectedShip).set(nextShipOpt)

      true
    } else
      false

  def keyDown(key: String): Unit = {
    gameStateProperty.get match {
      case Some(GameState(_, _, _, _, _: PlacingShipsMode)) =>
        if (key.equalsIgnoreCase("R"))
          rotateSelectedShip(1)
      case Some(GameState(_, _, _, _, _: PlayingMode)) =>
        if (key.equalsIgnoreCase(" "))
          launchAttack()
      case _ =>
    }
  }

  def mouseWheel(boardView: BoardView, wheelRotation: Int): Unit = {
    boardView.myBoardCanvas.setAttribute("tabindex", "0")
    boardView.myBoardCanvas.focus()

    gameStateProperty.get match {
      case Some(GameState(_, _, _, _, _: PlacingShipsMode)) =>
        if (wheelRotation != 0)
          rotateSelectedShip(wheelRotation)
      case Some(
            GameState(_, _, _, _, mode @ (PlayingMode(_, _, _, _, _) | GameOverMode(_, _, _, _, _)))
          ) =>
        val nextIndex =
          gameModel.get.selectedInGameMarkOpt.flatMap(selectedInGameMark =>
            BoardView.MarksSelectorOrder.zipWithIndex.find(_._1 == selectedInGameMark)
          ) match {
            case None if wheelRotation > 0 =>
              Some(0)
            case None if wheelRotation < 0 =>
              Some(BoardView.MarksSelectorOrder.size - 1)
            case Some((_, currentIndex)) =>
              if (mode.isPlaying)
                Some(currentIndex + wheelRotation)
                  .filter(index => index >= 0 && index < BoardView.MarksSelectorOrder.size)
              else
                Some(
                  (currentIndex + wheelRotation + BoardView.MarksSelectorOrder.size) %
                    BoardView.MarksSelectorOrder.size
                )
          }
        gameModel
          .subProp(_.selectedInGameMarkOpt)
          .set(nextIndex.map(BoardView.MarksSelectorOrder))
      case _ =>
    }
  }

  private def rotateSelectedShip(directionDelta: Int): Unit =
    (gameModel.get.selectedShip, modeTypeProperty.get) match {
      case (Some(ship), Some(PreGameModeType)) =>
        val updatedShip =
          if (directionDelta.abs != 1)
            ship.rotateBy(directionDelta)
          else {
            val possibleUpdatedShip = ship.rotateBy(directionDelta)
            if (
              possibleUpdatedShip.pieces.sortWith(Coordinate.defaultCompare(_, _) < 0) ==
                ship.pieces.sortWith(Coordinate.defaultCompare(_, _) < 0)
            )
              possibleUpdatedShip.rotateBy(directionDelta)
            else
              possibleUpdatedShip
          }
        gameModel.subProp(_.selectedShip).set(Some(updatedShip))
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
      case Some(GameState(gameId, _, _, _, PlacingShipsMode(true, _))) =>
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

        undoShip.foreach { headShip =>
          val shipsLeftToPlaceProperty = gameModel.subProp(_.shipsLeftToPlace)
          shipsLeftToPlaceProperty.set(headShip :: shipsLeftToPlaceProperty.get)
          gameStateProperty.set(
            Some(gameState.copy(me = me.modify(_.myBoard).using(_.removeLastShip)))
          )
        }

        gameModel.subProp(_.selectedShip).set(undoShip)

        cancelShipsPlacement()
      case _ =>
    }

  def resetPlacedShips(): Unit =
    gameStateProperty.get match {
      case Some(
            gameState @ GameState(
              _,
              Rules(_, fleet: Fleet, _, _, _),
              me,
              _,
              _: PlacingShipsMode
            )
          ) if me.myBoard.ships.nonEmpty =>
        val initialShips: List[Ship] =
          fleet.shipsList.sortBy(_.shipBiggestToSmallestOrder)
        gameModel.subProp(_.shipsLeftToPlace).set(initialShips)
        gameStateProperty.set(Some(gameState.copy(me = me.modify(_.myBoard).using(_.resetBoard))))
        gameModel.get.selectedShip match {
          case None =>
            gameModel.subProp(_.selectedShip).set(initialShips.headOption)
          case Some(_) =>
        }

        cancelShipsPlacement()
      case _ =>
    }

  def randomPlacement(): Unit =
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, _, me, _, _: PlacingShipsMode)) =>
        gameModel.subProp(_.shipsLeftToPlace).get match {
          case headShip :: _ =>
            val possibleCoorList =
              for {
                x <- 0 until me.myBoard.boardSize.x
                y <- 0 until me.myBoard.boardSize.y
                rotation <- Rotation.all
              } yield (Coordinate(x, y), rotation)
            val result: Option[(Coordinate, Rotation)] =
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

  def cancelQueuedAttacks(): Unit =
    (
      gameModel.get.turnAttacksQueuedStatus,
      gameStateProperty.get
    ) match {
      case (AttacksQueuedStatus.Queued, Some(GameState(_, _, _, _, _: PlayingMode))) =>
        gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.NotSet)
      case _ =>
    }

  def launchAttack(): Unit =
    (
      gameModel.get.turnAttacksQueuedStatus,
      gameModel.get.turnAttacks,
      gameStateProperty.get
    ) match {
      case (
            AttacksQueuedStatus.NotSet | AttacksQueuedStatus.Queued,
            turnAttacks,
            Some(GameState(gameId, _, _, _, PlayingMode(isMyTurn, turn, _, _, _)))
          ) if turnAttacks.forall(_.isPlaced) =>
        if (isMyTurn) {
          gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.Sent)
          gameRpc.sendTurnAttacks(gameId, turn, turnAttacks)
          stopNewTurnAnimation()
        } else {
          gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.Queued)
        }
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
    shipCounter(shipId)

  def addToEnemyTimeSeconds(secondsToAdd: Int): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, PlayingMode(_, _, _, _, Some(TimeRemaining(_, _))))) =>
        gameRpc.addToEnemyTimeSeconds(gameId, secondsToAdd)
      case _ =>
    }

}
