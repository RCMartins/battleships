package pt.rmartins.battleships.backend.services

import com.softwaremill.quicklens.ModifyPimp
import io.udash.rpc.ClientId
import pt.rmartins.battleships.shared.model.game.GameMode.{InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game.HitHint.{ShipHit, Water}
import pt.rmartins.battleships.shared.model.game._

import java.util.UUID
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

class GameService(rpcClientsService: RpcClientsService) {

  private val activeGamesByPlayer: mutable.Map[String, Game] = mutable.Map.empty
  private val activeGames: mutable.Map[GameId, Game] = mutable.Map.empty

  case class ServerMyBoard(
      boardSize: Coordinate,
      ships: List[ShipInGame]
  ) {

    lazy val efficientShipCheck: Vector[Vector[Option[ShipInGame]]] =
      ships
        .flatMap(shipInGame => shipInGame.shipActualPieces.map(_ -> shipInGame))
        .foldLeft(Vector.fill(boardSize.x)(Vector.fill(boardSize.y)(Option.empty[ShipInGame]))) {
          case (vectorMatrix, (shipPiece, shipInGame)) =>
            vectorMatrix.updated(
              shipPiece.x,
              vectorMatrix(shipPiece.x).updated(shipPiece.y, Some(shipInGame))
            )
        }

  }

  def updateVectorUsing(
      marks: Vector[Vector[(Option[Int], BoardMark)]],
      coordinate: Coordinate,
      f: ((Option[Int], BoardMark)) => (Option[Int], BoardMark)
  ): Vector[Vector[(Option[Int], BoardMark)]] = {
    val vectorX: Vector[(Option[Int], BoardMark)] = marks(coordinate.x)
    marks.updated(coordinate.x, vectorX.updated(coordinate.y, f(vectorX(coordinate.y))))
  }

  case class ServerEnemyBoard(
      boardSize: Coordinate,
      boardMarks: Vector[Vector[(Option[Int], BoardMark)]],
      shipsLeft: Int
  ) {

    def updateMarks(
        turnNumber: Int,
        hits: List[(Coordinate, Option[ShipInGame])]
    ): ServerEnemyBoard = {
      val allWater = hits.forall(_._2.isEmpty)
      val allShipHit = hits.forall(_._2.nonEmpty)

      val updatedBoardMarks =
        hits.foldLeft(boardMarks) { case (marks, (coor, _)) =>
          if (allWater)
            updateVectorUsing(marks, coor, _ => (Some(turnNumber), BoardMark.Miss))
          else if (allShipHit)
            updateVectorUsing(marks, coor, _ => (Some(turnNumber), BoardMark.ShipHit))
          else
            updateVectorUsing(marks, coor, { case (_, boardMark) => (Some(turnNumber), boardMark) })
        }
      copy(boardMarks = updatedBoardMarks)
    }

  }

  case class ServerPlayer(
      clientId: ClientId,
      username: String,
      myBoard: ServerMyBoard,
      enemyBoard: ServerEnemyBoard,
      turnPlayHistory: List[TurnPlay]
  ) {

    def toPlayer(game: Game): Player =
      if (game.halfTurnsOpt.isEmpty)
        Player(
          clientId.id,
          username,
          game.shipsToPlace,
          Board(myBoard.boardSize, myBoard.ships),
          Vector.empty,
          Nil
        )
      else
        Player(
          clientId.id,
          username,
          Nil,
          Board(myBoard.boardSize, myBoard.ships),
          enemyBoard.boardMarks,
          turnPlayHistory
        )

  }

  case class Game(
      gameId: GameId,
      boardSize: Coordinate,
      shipsToPlace: List[Ship],
      player1: ServerPlayer,
      player2: ServerPlayer,
      player1First: Boolean,
      halfTurnsOpt: Option[Int],
      gameOver: Boolean
  ) {

    def getPlayer(playerUsername: String): ServerPlayer =
      if (player1.username == playerUsername) player1 else player2

    def enemyPlayer(player: ServerPlayer): ServerPlayer =
      if (player1.username == player.username) player2 else player1

    def toGameStatePlayer1: GameState =
      toGameState(player1, player2)

    def toGameStatePlayer2: GameState =
      toGameState(player2, player1)

    private def toGameState(me: ServerPlayer, enemy: ServerPlayer): GameState = {
      val gameMode =
        halfTurnsOpt match {
          case None =>
            PreGameMode(shipsToPlace, me.myBoard.ships.nonEmpty, enemy.myBoard.ships.nonEmpty)
          case Some(halfTurns) =>
            InGameMode(
              (if (player1First) player1.username else player2.username) == me.username,
              halfTurns,
              turnAttacks = List.fill(3)(Attack(AttackType.Simple, None))
            )
        }

      GameState(
        gameId,
        me.toPlayer(this),
        SimplePlayer(
          enemy.clientId.id,
          enemy.username,
          enemy.enemyBoard.boardSize,
          enemy.turnPlayHistory
        ),
        gameMode
      )
    }

    def placeShips(playerUsername: String, shipPositions: List[ShipInGame]): Game =
      updatePlayer(getPlayer(playerUsername).modify(_.myBoard.ships).setTo(shipPositions))

    def updatePlayer(updatedPlayer: ServerPlayer): Game =
      if (updatedPlayer.username == player1.username)
        copy(player1 = updatedPlayer)
      else
        copy(player2 = updatedPlayer)

    def getCurrentTurnPlayer: Option[ServerPlayer] =
      halfTurnsOpt.map(halfTurns =>
        if (halfTurns % 2 == 1)
          if (player1First) player1 else player2
        else if (player1First) player2
        else player1
      )

  }

  def reload(clientId: ClientId, playerUsername: String): Future[Unit] = Future {
    activeGamesByPlayer.get(playerUsername).foreach { game =>
      val updatedGame2 =
        if (game.player1.username == playerUsername) {
          val updatedGame = game.modify(_.player1.clientId).setTo(clientId)
          rpcClientsService.sendGameState(clientId, updatedGame.toGameStatePlayer1)
          updatedGame
        } else {
          val updatedGame = game.modify(_.player2.clientId).setTo(clientId)
          rpcClientsService.sendGameState(clientId, updatedGame.toGameStatePlayer2)
          updatedGame
        }

      activeGamesByPlayer.update(playerUsername, updatedGame2)
      activeGames.update(game.gameId, updatedGame2)
    }
  }

  def startGame(player1Username: String, player2Username: String): Future[Unit] = Future {
    (
      rpcClientsService.getClientIdByUsername(player1Username),
      rpcClientsService.getClientIdByUsername(player2Username)
    ) match {
      case (Some(player1Id), Some(player2Id)) =>
        val shipsToPlace: List[Ship] =
          (
            List.fill(4)(Ship.Submarine) ++
              List.fill(3)(Ship.PatrolBoat2) ++
              List.fill(2)(Ship.PatrolBoat3) ++
              List.fill(1)(Ship.PatrolBoat4) ++
              List.fill(1)(Ship.Carrier)
          )
            .groupBy(_.shipId)
            .toList
            .sortBy { case (id, list) =>
              (-list.head.piecesSize, id)
            }
            .flatMap(_._2)

        val boardSize = Coordinate(10, 10)
        val myBoard: ServerMyBoard =
          ServerMyBoard(boardSize, Nil)
        val enemyBoard: ServerEnemyBoard =
          ServerEnemyBoard(
            boardSize,
            Vector.fill(10)(Vector.fill(10)((None, BoardMark.Empty))),
            shipsToPlace.size
          )

        val player1: ServerPlayer =
          ServerPlayer(player1Id, player1Username, myBoard, enemyBoard, Nil)
        val player2: ServerPlayer =
          ServerPlayer(player2Id, player2Username, myBoard, enemyBoard, Nil)

        val player1First: Boolean = Random.nextBoolean()
        val gameId = GameId(UUID.randomUUID().toString)
        val game = Game(
          gameId = gameId,
          boardSize = boardSize,
          shipsToPlace = shipsToPlace,
          player1 = player1,
          player2 = player2,
          player1First = player1First,
          halfTurnsOpt = None,
          gameOver = false
        )
        activeGamesByPlayer.update(player1Username, game)
        activeGamesByPlayer.update(player2Username, game)
        activeGames += gameId -> game

        updateBothGameState(game)
      case _ =>
        rpcClientsService.sendMessage(s"Error starting game!")
    }
  }

  def quitCurrentGame(gameId: GameId, playerUsername: String): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some(game) =>
          activeGamesByPlayer.remove(game.player1.username)
          activeGamesByPlayer.remove(game.player2.username)
          activeGames.remove(gameId)

          updateBothQuitGame(game)
      }
    }

  def confirmShips(
      gameId: GameId,
      playerUsername: String,
      shipPositions: List[ShipInGame]
  ): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some(game) => // TODO check game mode is InPlacingShips...
          val gameWithShips = game.placeShips(playerUsername, shipPositions)

          val readyToStart =
            gameWithShips.player1.myBoard.ships.nonEmpty &&
              gameWithShips.player2.myBoard.ships.nonEmpty

          val gameWithUpdatedMode =
            if (readyToStart)
              gameWithShips.copy(halfTurnsOpt = Some(1))
            else
              gameWithShips

          activeGamesByPlayer.update(gameWithUpdatedMode.player1.username, gameWithUpdatedMode)
          activeGamesByPlayer.update(gameWithUpdatedMode.player2.username, gameWithUpdatedMode)
          activeGames.update(gameId, gameWithUpdatedMode)

          if (readyToStart)
            updateBothGameState(gameWithUpdatedMode)
          else
            updateBothGameMode(gameWithUpdatedMode)
      }
    }

  def cancelShipsPlacement(gameId: GameId, playerUsername: String): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some(game) => // TODO check game mode is InPlacingShips...
          val player = game.getPlayer(playerUsername)
          if (player.myBoard.ships.nonEmpty) {
            val updatedPlayer = player.modify(_.myBoard.ships).setTo(Nil)
            val updatedGame = game.updatePlayer(updatedPlayer)

            activeGamesByPlayer.update(game.player1.username, updatedGame)
            activeGamesByPlayer.update(game.player2.username, updatedGame)
            activeGames.update(gameId, updatedGame)

            updateBothGameMode(updatedGame)
          }
      }
    }

  def sendTurnAttacks(
      gameId: GameId,
      playerUsername: String,
      halfTurns: Int,
      turnAttacks: List[Attack]
  ): Future[Unit] = {
    def validateTurnAttacks(me: ServerPlayer): Boolean =
      turnAttacks.forall {
        case Attack(_, Some(Coordinate(x, y))) =>
          val (hitTurnOpt, boardMark) = me.enemyBoard.boardMarks(x)(y)
          hitTurnOpt.isEmpty && !boardMark.isPermanent
        case _ =>
          false
      }

    Future {
      activeGames.get(gameId).map(game => (game, game.getPlayer(playerUsername))) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some((game, player))
            if !game.gameOver &&
              game.halfTurnsOpt.contains(halfTurns) &&
              game.getCurrentTurnPlayer.exists(_.username == player.username) &&
              validateTurnAttacks(player) =>
          val enemy = game.enemyPlayer(player)
          val hits: List[(Coordinate, Option[ShipInGame])] =
            turnAttacks.map(_.coordinateOpt.get).map { case coor @ Coordinate(x, y) =>
              coor -> enemy.myBoard.efficientShipCheck(x)(y)
            }

          val hitHints: List[HitHint] =
            hits
              .groupBy(_._2)
              .toList
              .flatMap {
                case (None, listOfMisses) =>
                  listOfMisses.map(_ => HitHint.Water)
                case (Some(shipInGame), listOfHits) =>
                  val hitsOnTheShip =
                    listOfHits.map(_ => HitHint.ShipHit(shipInGame.ship.shipId, destroyed = false))
                  val shipIsDestroyed =
                    shipInGame.shipActualPieces.forall { case pieceCoor @ Coordinate(x, y) =>
                      listOfHits.exists(_._1 == pieceCoor) ||
                        player.enemyBoard.boardMarks(x)(y)._1.nonEmpty
                    }
                  if (shipIsDestroyed)
                    hitsOnTheShip.head.copy(destroyed = true) :: hitsOnTheShip.tail
                  else
                    hitsOnTheShip
              }
              .sortBy {
                case Water =>
                  Int.MaxValue
                case ShipHit(shipId, destroyed) =>
                  shipId + (if (destroyed) 10000 else 0)
              }

          val turnNumber: Int =
            (halfTurns + 1) / 2

          val turnPlay: TurnPlay =
            TurnPlay(turnNumber, turnAttacks, hitHints)

          val updatedPlayer =
            player
              .modify(_.turnPlayHistory)
              .using(turnPlay :: _)
              .modify(_.enemyBoard)
              .using(_.updateMarks(turnNumber, hits))
              .modify(_.enemyBoard.shipsLeft)
              .using(_ - hitHints.count(_.isDestroyed))

          val gameOver = updatedPlayer.enemyBoard.shipsLeft == 0
          val updatedGame: Game =
            if (gameOver)
              game.modify(_.gameOver).setTo(true)
            else
              game.updatePlayer(updatedPlayer).modify(_.halfTurnsOpt).using(_.map(_ + 1))

          activeGamesByPlayer.update(game.player1.username, updatedGame)
          activeGamesByPlayer.update(game.player2.username, updatedGame)
          activeGames.update(gameId, updatedGame)

          updateBothGameState(updatedGame)
        case Some((game, player)) =>
          rpcClientsService.sendMessage(
            s"Invalid request! ${(
              !game.gameOver,
              game.halfTurnsOpt.contains(halfTurns),
              game.getCurrentTurnPlayer.exists(_.username == player.username),
              validateTurnAttacks(player),
              game.getCurrentTurnPlayer,
              player.username,
              validateTurnAttacks(player),
              game.halfTurnsOpt,
              halfTurns
            )}"
          )
      }
    }
  }

  private def updateBothGameState(game: Game): Unit = {
    rpcClientsService.sendGameState(
      game.player1.clientId,
      game.toGameStatePlayer1
    )
    rpcClientsService.sendGameState(
      game.player2.clientId,
      game.toGameStatePlayer2
    )
  }

  private def updateBothGameMode(game: Game): Unit = {
    rpcClientsService.sendGameMode(
      game.player1.clientId,
      game.toGameStatePlayer1.gameMode
    )
    rpcClientsService.sendGameMode(
      game.player2.clientId,
      game.toGameStatePlayer2.gameMode
    )
  }

  private def updateBothQuitGame(game: Game): Unit = {
    rpcClientsService.sendQuitGame(game.player1.clientId)
    rpcClientsService.sendQuitGame(game.player2.clientId)
  }

}
