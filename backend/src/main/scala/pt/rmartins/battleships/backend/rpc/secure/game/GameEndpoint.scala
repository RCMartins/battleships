package pt.rmartins.battleships.backend.rpc.secure.game

import io.udash.auth._
import pt.rmartins.battleships.backend.services.{ChatService, GameService, RpcClientsService}
import pt.rmartins.battleships.shared.model.auth.{Permission, UserContext}
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.{Coordinate, GameId, GameState, ShipInGame}
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

  override def startGameWith(otherPlayer: String): Future[Unit] =
    gameService.startGame(ctx.name, otherPlayer)

  override def confirmShips(
      gameId: GameId,
      shipPositions: List[ShipInGame]
  ): Future[Unit] =
    gameService.confirmShips(gameId, ctx.name, shipPositions)

  override def sendTurnPlay(hits: Seq[Coordinate]): Future[Boolean] =
    ???

}
