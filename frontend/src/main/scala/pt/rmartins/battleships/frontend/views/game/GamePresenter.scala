package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import io.udash.auth.AuthRequires
import org.scalajs.dom.html.Div
import org.scalajs.dom.window
import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.UserContextService
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.shared.model.auth.Permission
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.Utils.canPlaceInBoard
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import pt.rmartins.battleships.shared.rpc.server.secure.chat.ChatRPC

import java.util.Date
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Random, Success}

class GamePresenter(
    gameModel: ModelProperty[GameModel],
    gameStateModel: ModelProperty[GameStateModel],
    chatModel: ModelProperty[ChatModel],
    screenModel: ModelProperty[ScreenModel],
    gameRpc: GameRPC,
    chatRpc: ChatRPC,
    userService: UserContextService,
    notificationsCenter: NotificationsCenter
)(implicit
    ec: ExecutionContext
) extends Presenter[RoutingInGameState.type]
    with AuthRequires {

  requireAuthenticated()(userService.getCurrentContext)

  val gameStateProperty: Property[Option[GameState]] =
    gameStateModel.bitransform(_.gameState)(GameStateModel(_))

  val gameModeProperty: Property[Option[GameMode]] =
    gameStateProperty.bitransform[Option[GameMode]](_.map(_.gameMode)) {
      case None =>
        gameStateModel.get.gameState
      case Some(gameMode) =>
        gameStateModel.get.gameState.map(_.copy(gameMode = gameMode))
    }

  val inGameModeProperty: Property[Option[InGameMode]] =
    gameModeProperty.bitransform[Option[InGameMode]] {
      case Some(mode: InGameMode) => Some(mode)
      case _                      => None
    }(identity)

  val rulesProperty: ReadableProperty[Option[Rules]] =
    gameStateProperty.transform(_.map(_.rules))

  val inPreGameMode: ReadableProperty[Boolean] =
    inGameModeProperty.transform(_.exists(_.isPreGame))

  val isMyTurnProperty: ReadableProperty[Boolean] =
    inGameModeProperty.transform(_.exists(_.isMyTurn))

  val meProperty: ReadableProperty[Option[Player]] =
    gameStateProperty.transform(_.map(_.me))

  val selectedTabProperty: ReadableProperty[String] =
    screenModel.subProp(_.selectedTab)

  val mousePositionProperty: ReadableProperty[Option[Coordinate]] =
    gameModel.subProp(_.mousePosition)

  private val msgCallback = notificationsCenter.onNewMsg { case msg =>
    chatModel.subSeq(_.msgs).append(msg)
  }

  private val connectionsCallback = notificationsCenter.onConnectionsCountChange { case count =>
    chatModel.subProp(_.connectionsCount).set(count)
  }

  private val onQuitGameCallback = notificationsCenter.onQuitGame { case _ =>
    gameModel.set(GameModel.default)
    gameStateProperty.set(None)
    screenModel.set(ScreenModel.default.copy(canvasSize = screenModel.get.canvasSize))
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

  private var timeRemainingIntervalHandle: Option[Int] = None
  private val timeRemainingIntervalMillis: Int = 100

  def updateGameMode(fromGameMode: Option[GameMode], to: Option[GameState]): Unit =
    (fromGameMode, to) match {
      case (None, Some(GameState(_, _, me, _, _: PreGameMode))) =>
        me.shipsLeftToPlace.headOption.foreach { headShip =>
          gameModel.subProp(_.selectedShip).set(Some(headShip))
        }
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
      case (
            None | Some(_: PreGameMode),
            Some(GameState(_, _, _, _, InGameMode(_, _, turnAttackTypes, _, _)))
          ) =>
        screenModel.subProp(_.selectedTab).set(ScreenModel.myMovesTab)
        gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))

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

              inGameModeProperty.get.foreach {
                case inGameMode @ InGameMode(
                      isMyTurn,
                      _,
                      _,
                      Some(myTimeRemaining),
                      Some(enemyTimeRemaining)
                    ) =>
                  inGameModeProperty.set(
                    Some(
                      if (isMyTurn)
                        inGameMode
                          .modify(_.myTimeRemaining)
                          .setTo(Some(reduceTime(myTimeRemaining)))
                      else
                        inGameMode
                          .modify(_.enemyTimeRemaining)
                          .setTo(Some(reduceTime(enemyTimeRemaining)))
                    )
                  )
                case _ =>
              }
            },
            timeout = timeRemainingIntervalMillis
          )
        )
      case (_, None | Some(GameState(_, _, _, _, _: GameOverMode))) =>
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
      case _ =>
    }

  private var newTurnAnimationHandle: Option[Int] = None
  private val newTurnAnimationMillis: Int = 50

  private def stopNewTurnAnimation(): Unit = {
    newTurnAnimationHandle.foreach(window.clearTimeout)
    newTurnAnimationHandle = None
  }

  inGameModeProperty
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
    if (hasReadAccess) {
      chatRpc.latestMessages().onComplete {
        case Success(msgs) =>
          chatModel.subSeq(_.msgs).prepend(msgs: _*)
        case Failure(ex) =>
          chatModel
            .subSeq(_.msgs)
            .set(Seq(ChatMessage(s"ERROR: ${ex.getMessage}", "System", new Date)))
      }
    } else {
      chatModel
        .subSeq(_.msgs)
        .set(Seq(ChatMessage(s"You don't have access to read the messages.", "System", new Date)))
    }

    chatModel.subProp(_.username).set(userService.getCurrentContext.username)

    chatRpc.connectedClientsCount().onComplete {
      case Success(count) =>
        chatModel.subProp(_.connectionsCount).set(count)
      case Failure(ex) =>
        chatModel.subProp(_.connectionsCount).set(-1)
        chatModel
          .subSeq(_.msgs)
          .set(Seq(ChatMessage(s"ERROR: ${ex.getMessage}", "System", new Date)))
    }
  }

  def onCanvasResize(canvasDiv: Div): Unit = {
    val width = Math.max(500, canvasDiv.clientWidth)
    val height = BoardView.CanvasSize.y
    screenModel.subProp(_.canvasSize).set(Coordinate(width, height))
  }

  override def onClose(): Unit = {
    // remove callbacks from NotificationsCenter before exit
    msgCallback.cancel()
    connectionsCallback.cancel()

    onQuitGameCallback.cancel()
    onGameStateCallback.cancel()
    onGameModeCallback.cancel()
  }

  def sendMsg(): Unit = {
    val msgProperty = chatModel.subProp(_.msgInput)
    val msg = msgProperty.get.trim
    msgProperty.set("")
    if (msg.nonEmpty) {
      chatRpc.sendMsg(msg)
    }
  }

  def startGameWithBots(): Unit =
    gameRpc.startGameWithBots()

  def startGameWith(): Unit =
    gameRpc.startGameWith(
      Username(if (chatModel.get.username.username == "player1") "player2" else "player1")
    )

  def restartGame(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, _)) =>
        gameRpc.restartGame(gameId)
      case _ =>
    }

  def quitCurrentGame(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, _)) =>
        gameRpc.quitCurrentGame(gameId)
      case _ =>
    }

  def hasReadAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatRead))

  def hasWriteAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatWrite))

  def mouseMove(boardView: BoardView, mouseX: Int, mouseY: Int): Unit = {
    gameModel.subProp(_.mousePosition).set(Some(Coordinate(mouseX, mouseY) - boardView.AbsMargin))

    (gameModel.get, gameStateProperty.get) match {
      case (
            GameModel(Some(_), Some(button @ (0 | 2)), _, _, _, selectedBoardMarkOpt),
            Some(
              gameState @ GameState(
                gameId,
                _,
                me,
                _,
                InGameMode(_, _, _, _, _) | GameOverMode(_, _, _, _)
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
            GameModel(Some(_), Some(0), selectedShipOpt, _, _, _),
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
            GameModel(Some(_), Some(2), _, _, _, _),
            Some(
              gameState @ GameState(
                gameId,
                _,
                me,
                _,
                InGameMode(_, _, _, _, _) | GameOverMode(_, _, _, _)
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
            GameModel(Some(_), Some(0), _, turnAttacks, turnAttacksSent, selectedBoardMarkOpt),
            Some(
              gameState @ GameState(
                gameId,
                _,
                me,
                _,
                InGameMode(_, _, _, _, _) | GameOverMode(_, _, _, _)
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
              if gameState.gameMode.isInGame &&
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
          .using(_.addShip(ShipInGame(ship, coordinate)))

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
    if (wheelRotation != 0)
      rotateSelectedShip(wheelRotation)

  private def rotateSelectedShip(directionDelta: Int): Unit =
    (gameModel.get.selectedShip, gameModeProperty.get) match {
      case (Some(ship), Some(PreGameMode(_, _))) =>
        gameModel
          .subProp(_.selectedShip)
          .set(Some(ship.rotateBy(directionDelta)))
      case _ =>
    }

  private def removeOneShip(shipId: Int, list: List[Ship]): List[Ship] =
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
            case ShipInGame(headShip, _) :: _ =>
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
      case Some(gameState @ GameState(_, Rules(shipsInThisGame, _, _, _), me, _, PreGameMode(_, _)))
          if me.myBoard.ships.nonEmpty =>
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
            Some(GameState(gameId, _, _, _, InGameMode(_, turn, _, _, _)))
          ) if turnAttacks.forall(_.isPlaced) =>
        gameModel.subProp(_.turnAttacksSent).set(true)
        gameRpc.sendTurnAttacks(gameId, turn, turnAttacks)

        stopNewTurnAnimation()
      case _ =>
    }

  def isValidCoordinateTarget(enemyBoardCoor: Coordinate): Boolean =
    gameStateProperty.get match {
      case Some(GameState(_, _, me, _, _: InGameMode)) =>
        val Coordinate(x, y) = enemyBoardCoor
        val (turnNumberOpt, boardMark) = me.enemyBoardMarks(x)(y)
        turnNumberOpt.isEmpty && !boardMark.isPermanent
      case _ =>
        false
    }

  def setSelectedTab(selectedTab: String): Unit = {
    screenModel.subProp(_.selectedTab).set(selectedTab)
  }

}
