package pt.rmartins.battleships.backend.rpc.auth

import io.udash.rpc.ClientId
import pt.rmartins.battleships.backend.services.{AuthService, GameService, RpcClientsService}
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.model.game.Username
import pt.rmartins.battleships.shared.rpc.server.open.AuthRPC

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class AuthEndpoint(implicit
    authService: AuthService,
    gameService: GameService,
    rpcClientsService: RpcClientsService,
    clientId: ClientId
) extends AuthRPC {

  override def login(username: Username, password: String): Future[UserContext] = {
    val response = authService.login(username, password)
    response.onComplete {
      case Success(ctx) =>
        // let rpcClientsService know about a new authenticated connection
        rpcClientsService.registerAuthenticatedConnection(clientId, username, ctx)
        gameService.reload(clientId, username)
      case Failure(_) => // ignore
    }

    response
  }

}