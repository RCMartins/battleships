package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import io.udash.auth.AuthRequires
import org.scalajs.dom.html.Div
import org.scalajs.dom.{Event, document}
import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.UserContextService
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.views.game.BoardView.ToPlaceShip
import pt.rmartins.battleships.shared.model.auth.Permission
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.GameMode.{InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import pt.rmartins.battleships.shared.rpc.server.secure.chat.ChatRPC

import java.util.Date
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Random, Success}

class GamePresenter(
    gameModel: ModelProperty[GameModel],
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

  private val msgCallback = notificationsCenter.onNewMsg { case msg =>
    chatModel.subSeq(_.msgs).append(msg)
  }

  private val connectionsCallback = notificationsCenter.onConnectionsCountChange { case count =>
    chatModel.subProp(_.connectionsCount).set(count)
  }

  private val onQuitGameCallback = notificationsCenter.onQuitGame { case _ =>
    chatRpc.sendMsg("Game quit!")
    gameModel.subProp(_.myGameState).set(None)
  }

  private val onGameStateCallback = notificationsCenter.onGameState { case updatedGameState =>
//    chatRpc.sendMsg("New Game State!")
    gameModel.subProp(_.myGameState).set(Some(updatedGameState))
  }

  private val onGameModeCallback = notificationsCenter.onGameMode { case updatedGameMode =>
//    chatRpc.sendMsg("New Game Mode!")
    gameModel
      .subProp(_.myGameState)
      .set(gameModel.get.myGameState.map(_.copy(gameMode = updatedGameMode)))
  }

//  private val receiveHitsCallback = notificationsCenter.onReceiveHits { case hits =>
  //    model.subSeq(_.msgs).append(msg)
//  }

  val inGameMode: ReadableProperty[Boolean] =
    gameModel.subProp(_.myGameState).transform {
      case Some(GameState(_, _, _, InGameMode(_, _, _))) => true
      case _                                             => false
    }

  val inGameModeOpt: ReadableProperty[Option[InGameMode]] =
    gameModel.subProp(_.myGameState).transform {
      case Some(GameState(_, _, _, mode @ InGameMode(_, _, _))) => Some(mode)
      case _                                                    => None
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

    chatModel.subProp(_.username).set(userService.getCurrentContext.name)

    chatRpc.connectedClientsCount().onComplete {
      case Success(count) =>
        chatModel.subProp(_.connectionsCount).set(count)
      case Failure(ex) =>
        chatModel.subProp(_.connectionsCount).set(-1)
        chatModel
          .subSeq(_.msgs)
          .set(Seq(ChatMessage(s"ERROR: ${ex.getMessage}", "System", new Date)))
    }

//    screenModel.touch()

//    document.onload = { (_: Event) =>

//    }
  }

//  document.addEventListener(
//    "DOMContentLoaded",
//    { (_: Event) =>
//      onCanvasResize()
//    }
//  )

//  document.onload = { (_: Event) => onCanvasResize() }

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

  def startGameWith(): Unit = {
    gameRpc.startGameWith(if (chatModel.get.username == "player1") "player2" else "player1")
  }

  def quitCurrentGame(): Unit =
    gameModel.get match {
      case GameModel(Some(GameState(gameId, _, _, _)), _, _) =>
        gameRpc.quitCurrentGame(gameId)
      case _ =>
    }

  def hasReadAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatRead))

  def hasWriteAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatWrite))

//  def hasActiveNotificationsCenterCallbacks: Boolean =
//  msgCallback.isActive && connectionsCallback.isActive

  def mouseMove(mouseX: Int, mouseY: Int): Unit = {
    gameModel.subProp(_.mousePosition).set(Some(Coordinate(mouseX, mouseY)))
  }

  def mouseLeave(): Unit =
    gameModel.subProp(_.mousePosition).set(None)

  def mouseClick(boardView: BoardView): Unit = {
    gameModel.get match {
      case GameModel(
            Some(gameState @ GameState(_, me, _, PreGameMode(_, _, _))),
            Some(mousePosition),
            selectedShipOpt
          ) =>
        boardView.getShipInCoor(boardView.getAllShipsCoordinates.get, mousePosition) match {
          case Some(ToPlaceShip(ship, _, _, _)) =>
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
      case GameModel(
            Some(gameState @ GameState(_, me, _, gameMode @ InGameMode(_, _, initialTurnAttacks))),
            Some(mousePosition),
            _
          ) =>
        boardView.enemyBoardMouseCoordinate.get.foreach {
          case enemyBoardCoordinate if isValidCoordinateTarget(enemyBoardCoordinate) =>
            def setFirstMissile(turnAttacks: List[Attack]): List[Attack] =
              turnAttacks match {
                case Nil =>
                  Nil
                case (headAttack @ Attack(_, None)) :: next =>
                  headAttack.copy(coordinateOpt = Some(enemyBoardCoordinate)) :: next
                case headAttack :: next =>
                  headAttack :: setFirstMissile(next)
              }

            val turnAttackUpdated: List[Attack] =
              if (initialTurnAttacks.exists(_.coordinateOpt.contains(enemyBoardCoordinate)))
                initialTurnAttacks.map {
                  case Attack(attackType, Some(coordinate)) if coordinate == enemyBoardCoordinate =>
                    Attack(attackType, None)
                  case other =>
                    other
                }
              else
                setFirstMissile(initialTurnAttacks)

            gameModel
              .subProp(_.myGameState)
              .set(
                Some(
                  gameState
                    .modify(_.gameMode)
                    .setTo(gameMode.modify(_.turnAttacks).setTo(turnAttackUpdated))
                )
              )
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
  ): Boolean = {
    if (canPlace(me.myBoard, ship, coordinate)) {
      val playerUpdated =
        me
          .modify(_.shipsLeftToPlace)
          .using(removeOneShip(ship.shipId, _))
          .modify(_.myBoard)
          .using(_.addShip(ShipInGame(ship, coordinate)))

      gameModel
        .subProp(_.myGameState)
        .set(Some(gameState.copy(me = playerUpdated)))

      val nextShipOpt =
        if (playerUpdated.shipsLeftToPlace.exists(_.shipId == ship.shipId))
          Some(ship)
        else
          playerUpdated.shipsLeftToPlace.headOption

      gameModel
        .subProp(_.selectedShip)
        .set(nextShipOpt)

      true
    } else
      false
  }

  def keyDown(key: String): Unit = {
    print(key)
    if (key == "R")
      rotateSelectedShip(1)
  }

  def mouseWheel(wheelRotation: Int): Unit =
    if (wheelRotation != 0)
      rotateSelectedShip(wheelRotation)

  def canPlace(myBoard: Board, shipToPlace: Ship, boardCoor: Coordinate): Boolean = {
    val actualPiecePositions = shipToPlace.pieces.map(_ + boardCoor)

    actualPiecePositions.forall(_.isInside(myBoard.boardSize)) &&
    !actualPiecePositions
      .exists(coor => myBoard.ships.exists(_.shipActualPieces.exists(_.distance(coor) <= 1)))
  }

  private def rotateSelectedShip(directionDelta: Int): Unit =
    gameModel.get match {
      case GameModel(
            Some(GameState(_, _, _, PreGameMode(_, _, _))),
            _,
            Some(ship)
          ) =>
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
    gameModel.get match {
      case GameModel(Some(GameState(gameId, _, _, PreGameMode(_, true, _))), _, _) =>
        gameRpc.cancelShipsPlacement(gameId)
      case _ =>
    }

  def confirmShipPlacement(): Unit =
    gameModel.get match {
      case GameModel(Some(GameState(gameId, me, _, _)), _, _) =>
        gameRpc.confirmShips(gameId, me.myBoard.ships)
      case _ =>
    }

  def undoLastPlacedShip(): Unit =
    gameModel.get match {
      case GameModel(Some(gameState @ GameState(_, me, _, _)), _, _) =>
        val undoShip: Option[Ship] =
          me.myBoard.ships match {
            case Nil =>
              None
            case ShipInGame(headShip, _) :: _ =>
              Some(headShip)
          }

        gameModel
          .subProp(_.myGameState)
          .set(
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
      case _ =>
    }

  def resetPlacedShips(): Unit =
    gameModel.get match {
      case GameModel(
            Some(gameState @ GameState(_, me, _, PreGameMode(shipsToPlace, _, _))),
            _,
            _
          ) if me.myBoard.ships.nonEmpty =>
        gameModel
          .subProp(_.myGameState)
          .set(
            Some(
              gameState.copy(me =
                me.modify(_.shipsLeftToPlace)
                  .setTo(shipsToPlace)
                  .modify(_.myBoard)
                  .using(_.resetBoard)
              )
            )
          )
        cancelShipsPlacement()
      case _ =>
    }

  def randomPlacement(): Unit = {
    gameModel.get match {
      case GameModel(Some(gameState @ GameState(_, me, _, PreGameMode(_, _, _))), _, _) =>
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
  }

  def launchAttack(): Unit = {
    println("launchAttack!")
    gameModel.get match {
      case GameModel(
            Some(GameState(gameId, _, _, InGameMode(_, halfTurns, turnAttacks))),
            _,
            _
          ) if turnAttacks.forall(_.isPlaced) =>
        gameRpc.sendTurnAttacks(gameId, halfTurns, turnAttacks)
      case gameModel =>
        println(gameModel.myGameState.map(_.gameMode))
    }
  }

  def isValidCoordinateTarget(enemyBoardCoor: Coordinate): Boolean = {
    gameModel.get match {
      case GameModel(
            Some(GameState(_, me, _, InGameMode(_, _, _))),
            _,
            _
          ) =>
        val Coordinate(x, y) = enemyBoardCoor
        val (turnNumberOpt, boardMark) = me.enemyBoardMarks(x)(y)
        turnNumberOpt.isEmpty && !boardMark.isPermanent
      case _ =>
        false
    }
  }

  def setSelectedTab(selectedTab: String): Unit = {
    screenModel.subProp(_.selectedTab).set(selectedTab)
  }

}
