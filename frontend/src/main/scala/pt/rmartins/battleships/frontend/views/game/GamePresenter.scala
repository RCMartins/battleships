package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import io.udash.auth.AuthRequires
import org.scalajs.dom.html.Div
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

  val gameModeProperty: ReadableProperty[Option[GameMode]] =
    gameStateProperty.transform(_.map(_.gameMode))

  val inGameModeProperty: ReadableProperty[Option[InGameMode]] =
    gameModeProperty.transform {
      case Some(mode @ InGameMode(_, _, _)) => Some(mode)
      case _                                => None
    }

  val inPreGameMode: ReadableProperty[Boolean] =
    inGameModeProperty.transform(_.exists(_.isPreGame))

  val isMyTurnProperty: ReadableProperty[Boolean] =
    inGameModeProperty.transform(_.exists(_.isMyTurn))

  val selectedTabProperty: ReadableProperty[String] =
    screenModel.subProp(_.selectedTab)

  private val msgCallback = notificationsCenter.onNewMsg { case msg =>
    chatModel.subSeq(_.msgs).append(msg)
  }

  private val connectionsCallback = notificationsCenter.onConnectionsCountChange { case count =>
    chatModel.subProp(_.connectionsCount).set(count)
  }

  private val onQuitGameCallback = notificationsCenter.onQuitGame { case _ =>
    chatRpc.sendMsg("Game quit!")
    gameStateProperty.set(None)
  }

  private val onGameStateCallback = notificationsCenter.onGameState { case updatedGameState =>
    (inGameModeProperty.get, updatedGameState.gameMode) match {
      case (Some(InGameMode(_, halfTurns1, _)), InGameMode(_, halfTurns2, _))
          if halfTurns1 != halfTurns2 =>
        chatRpc.sendMsg(s"Changed turn $halfTurns1 -> $halfTurns2")
      case _ =>
    }
    gameStateProperty.set(Some(updatedGameState))
  }

  private val onGameModeCallback = notificationsCenter.onGameMode { case updatedGameMode =>
    (inGameModeProperty.get, updatedGameMode) match {
      case (Some(InGameMode(_, halfTurns1, _)), InGameMode(_, halfTurns2, _))
          if halfTurns1 != halfTurns2 =>
        chatRpc.sendMsg(s"Changed turn $halfTurns1 -> $halfTurns2")
      case _ =>
    }
    gameStateProperty.set(gameStateProperty.get.map(_.copy(gameMode = updatedGameMode)))
  }

  inGameModeProperty.transform(_.map(_.turnAttackTypes)).listen {
    case Some(turnAttackTypes) =>
      if (gameModel.get.turnAttacksSent)
        gameModel.subProp(_.turnAttacksSent).set(false)
      gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))
    case None =>
      gameModel.subProp(_.turnAttacks).set(Nil)
  }

  inGameModeProperty
    .transform(_.map(inGameMode => (inGameMode.isMyTurn, inGameMode.turnAttackTypes)))
    .listen {
      case Some((false, turnAttackTypes)) if gameModel.get.turnAttacksSent =>
        gameModel.subProp(_.turnAttacksSent).set(false)
        gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))
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
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _)) =>
        gameRpc.quitCurrentGame(gameId)
      case _ =>
    }

  def hasReadAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatRead))

  def hasWriteAccess: Boolean =
    userService.currentContext.exists(_.has(Permission.ChatWrite))

  def mouseMove(boardView: BoardView, mouseX: Int, mouseY: Int): Unit = {
    gameModel.subProp(_.mousePosition).set(Some(Coordinate(mouseX, mouseY) - boardView.AbsMargin))
  }

  def mouseLeave(): Unit =
    gameModel.subProp(_.mousePosition).set(None)

  def mouseClick(boardView: BoardView): Unit = {
    (gameModel.get, gameStateProperty.get) match {
      case (
            GameModel(Some(mousePosition), selectedShipOpt, _, _, _),
            Some(gameState @ GameState(_, me, _, PreGameMode(_, _, _)))
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
      case (
            GameModel(Some(_), _, turnAttacks, turnAttacksSent, selectedBoardMarkOpt),
            Some(gameState @ GameState(gameId, me, _, InGameMode(_, _, _)))
          ) =>
//        println(
//          (
//            boardView.enemyBoardMouseCoordinate.get,
//            boardView.boardMarkHover.get,
//            selectedBoardMarkOpt
//          )
//        )

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
              if !turnAttacksSent && isValidCoordinateTarget(enemyBoardCoor) =>
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
  ): Boolean = {
    if (canPlace(me.myBoard, ship, coordinate)) {
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
  }

  def keyDown(key: String): Unit = {
    if (key == "R")
      rotateSelectedShip(1)
  }

  def mouseWheel(wheelRotation: Int): Unit =
    if (wheelRotation != 0)
      rotateSelectedShip(wheelRotation)

  def canPlace(myBoard: Board, shipToPlace: Ship, boardCoor: Coordinate): Boolean = {
    val actualPiecePositions = shipToPlace.pieces.map(_ + boardCoor)

    actualPiecePositions.forall(_.isInsideBoard(myBoard.boardSize)) &&
    !actualPiecePositions
      .exists(coor => myBoard.ships.exists(_.shipActualPieces.exists(_.distance(coor) <= 1)))
  }

  private def rotateSelectedShip(directionDelta: Int): Unit =
    (gameModel.get.selectedShip, gameModeProperty.get) match {
      case (Some(ship), Some(PreGameMode(_, _, _))) =>
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
      case Some(GameState(gameId, _, _, PreGameMode(_, true, _))) =>
        gameRpc.cancelShipsPlacement(gameId)
      case _ =>
    }

  def confirmShipPlacement(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, me, _, _)) =>
        gameRpc.confirmShips(gameId, me.myBoard.ships)
      case _ =>
    }

  def undoLastPlacedShip(): Unit =
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, me, _, _)) =>
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
      case _ =>
    }

  def resetPlacedShips(): Unit =
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, me, _, PreGameMode(shipsToPlace, _, _)))
          if me.myBoard.ships.nonEmpty =>
        gameStateProperty.set(
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
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, me, _, PreGameMode(_, _, _))) =>
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
    (gameModel.get.turnAttacks, gameStateProperty.get) match {
      case (
            turnAttacks,
            Some(GameState(gameId, _, _, InGameMode(_, halfTurns, _)))
          ) if turnAttacks.forall(_.isPlaced) =>
        gameModel.subProp(_.turnAttacksSent).set(true)
        gameRpc.sendTurnAttacks(gameId, halfTurns, turnAttacks)
      case _ =>
    }
  }

  def isValidCoordinateTarget(enemyBoardCoor: Coordinate): Boolean = {
    gameStateProperty.get match {
      case Some(GameState(_, me, _, InGameMode(_, _, _))) =>
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
