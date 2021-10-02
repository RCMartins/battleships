package pt.rmartins.battleships.frontend.views.game

import io.udash._
import io.udash.auth.AuthRequires
import org.scalajs.dom.raw.KeyboardEvent
import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.UserContextService
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.views.game.BoardView.ToPlaceShip
import pt.rmartins.battleships.shared.model.auth.Permission
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.GameMode.PreGameMode
import pt.rmartins.battleships.shared.model.game.{
  Board,
  Coordinate,
  GameState,
  Player,
  Rotation,
  Ship,
  ShipInGame
}
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import pt.rmartins.battleships.shared.rpc.server.secure.chat.ChatRPC

import java.util.Date
import scala.concurrent.ExecutionContext
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Random, Success}
import com.softwaremill.quicklens.ModifyPimp

class GamePresenter(
    gameModel: ModelProperty[GameModel],
    chatModel: ModelProperty[ChatModel],
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

  private val onGameStateCallback = notificationsCenter.onGameView { case updatedGameView =>
    chatRpc.sendMsg("New Game State!")
    gameModel.subProp(_.myGameState).set(Some(updatedGameView))
  }

//  private val receiveHitsCallback = notificationsCenter.onReceiveHits { case hits =>
  //    model.subSeq(_.msgs).append(msg)
//  }

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
  }

  override def onClose(): Unit = {
    // remove callbacks from NotificationsCenter before exit
    msgCallback.cancel()
    connectionsCallback.cancel()

    onGameStateCallback.cancel()
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

  def hasReadAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatRead))

  def hasWriteAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatWrite))

  def hasActiveNotificationsCenterCallbacks: Boolean =
    msgCallback.isActive && connectionsCallback.isActive

  def moveMouse(mouseX: Int, mouseY: Int): Unit = {
    gameModel.subProp(_.mousePosition).set(Some(Coordinate(mouseX, mouseY)))
  }

  def moveLeave(): Unit =
    gameModel.subProp(_.mousePosition).set(None)

  def moveClick(boardView: BoardView): Unit = {
    gameModel.get match {
      case GameModel(
            Some(gameState @ GameState(_, me, _, PreGameMode(_))),
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
            Some(GameState(_, _, _, PreGameMode(_))),
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

  def confirmShipPlacement(): Unit = {
    gameModel.get match {
      case GameModel(Some(GameState(gameId, me, _, _)), _, _) =>
        gameRpc.confirmShips(gameId, me.myBoard.ships)
      case _ =>
    }
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

  def resetPlaceShips(): Unit =
    gameModel.get match {
      case GameModel(Some(gameState @ GameState(_, me, _, PreGameMode(shipsToPlace))), _, _) =>
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
      case _ =>
    }

  // TODO currently it's not random at all!
  def randomPlacement(): Unit = {
    gameModel.get match {
      case GameModel(Some(gameState @ GameState(_, me, _, PreGameMode(_))), _, _) =>
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

}
