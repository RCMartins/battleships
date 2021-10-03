package pt.rmartins.battleships.backend.rpc.secure.game

import io.udash.auth._
import pt.rmartins.battleships.backend.services.{ChatService, GameService, RpcClientsService}
import pt.rmartins.battleships.shared.model.auth.{Permission, UserContext}
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.{Attack, Coordinate, GameId, GameState, ShipInGame}
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import pt.rmartins.battleships.shared.rpc.server.secure.chat.ChatRPC

import scala.concurrent.Future

/** Verifies user's permissions and passes valid requests to the services. */
class GameEndpoint(implicit
    gameService: GameService,
    rpcClientsService: RpcClientsService,
    ctx: UserContext
) extends GameRPC
    with AuthRequires {

  override def startGameWith(otherPlayerUsername: String): Future[Unit] =
    gameService.startGame(ctx.name, otherPlayerUsername)

  override def quitCurrentGame(gameId: GameId): Future[Unit] =
    gameService.quitCurrentGame(gameId, ctx.name)

  override def confirmShips(
      gameId: GameId,
      shipPositions: List[ShipInGame]
  ): Future[Unit] =
    gameService.confirmShips(gameId, ctx.name, shipPositions)

  override def cancelShipsPlacement(gameId: GameId): Future[Unit] =
    gameService.cancelShipsPlacement(gameId, ctx.name)

  def sendTurnAttacks(gameId: GameId, halfTurns: Int, turnAttacks: List[Attack]): Future[Unit] =
    gameService.sendTurnAttacks(gameId, ctx.name, halfTurns, turnAttacks)

}
