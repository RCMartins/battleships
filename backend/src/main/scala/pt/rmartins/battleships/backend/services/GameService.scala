package pt.rmartins.battleships.backend.services

import com.softwaremill.quicklens.ModifyPimp
import io.udash.rpc.ClientId
import pt.rmartins.battleships.shared.model.game.GameMode.{InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._

import java.util.UUID
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

class GameService(rpcClientsService: RpcClientsService) {

  private val activeGames: mutable.Map[GameId, Game] = mutable.Map.empty

  case class ServerPlayer(
      clientId: String,
      username: String,
      placedShips: List[ShipInGame],
      board: Board
  ) {

    def toPlayer(game: Game): Player =
      game.gameMode match {
        case PreGameMode(shipsToPlace) =>
          Player(clientId, username, shipsToPlace, board)
        case InGameMode(_, _) =>
          Player(clientId, username, Nil, board)
      }

  }

  case class Game(
      gameId: GameId,
      initialBoard: Board,
      player1: ServerPlayer,
      player2: ServerPlayer,
      ships: List[Ship],
      player1First: Boolean,
      gameMode: GameMode
  ) {

    def toGameStatePlayer1: GameState =
      toGameState(player1, SimplePlayer(player2.clientId, player2.username))

    def toGameStatePlayer2: GameState =
      toGameState(player2, SimplePlayer(player1.clientId, player1.username))

    private def toGameState(me: ServerPlayer, enemy: SimplePlayer): GameState =
      GameState(gameId, me.toPlayer(this), enemy, gameMode)

    def placeShips(player: ServerPlayer, shipPositions: List[ShipInGame]): Game =
      if (player.username == player1.username)
        this.modify(_.player1.placedShips).setTo(shipPositions)
      else
        this.modify(_.player2.placedShips).setTo(shipPositions)

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

        val initialBoard = Board(Coordinate(10, 10), Nil, 0)
        val player1 = ServerPlayer(player1Id.id, player1Username, Nil, initialBoard)
        val player2 = ServerPlayer(player2Id.id, player2Username, Nil, initialBoard)

        val player1First: Boolean = Random.nextBoolean()
        val gameId = GameId(UUID.randomUUID().toString)
        val game = Game(
          gameId,
          initialBoard,
          player1,
          player2,
          shipsToPlace,
          player1First,
          GameMode.PreGameMode(shipsToPlace)
        )
        activeGames += gameId -> game

        updateBothPlayers(game)
      case _ =>
        rpcClientsService.sendMessage(s"Error starting game!")
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
          // TODO check username is valid...
          val player = if (game.player1.username == playerUsername) game.player1 else game.player2
          val updatedGame = game.placeShips(player, shipPositions)

          val updatedGame2 =
            if (
              updatedGame.player1.placedShips.nonEmpty && updatedGame.player2.placedShips.nonEmpty
            )
              updatedGame.copy(gameMode =
                InGameMode(
                  (if (updatedGame.player1First) game.player1 else game.player2).username,
                  1
                )
              )
            else
              updatedGame

          activeGames.update(gameId, updatedGame2)
          updateBothPlayers(updatedGame2)
      }
    }

  private def updateBothPlayers(game: Game): Unit = {
    rpcClientsService.sendGameState(
      ClientId(game.player1.clientId),
      game.toGameStatePlayer1
    )
    rpcClientsService.sendGameState(
      ClientId(game.player2.clientId),
      game.toGameStatePlayer2
    )
  }

}
