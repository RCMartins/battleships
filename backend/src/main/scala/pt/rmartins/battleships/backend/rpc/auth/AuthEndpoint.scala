package pt.rmartins.battleships.backend.rpc.auth

import io.udash.rpc.ClientId
import pt.rmartins.battleships.backend.services.{AuthService, GameService, RpcClientsService}
import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
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

  override def loginUsername(username: Username): Future[UserContext] =
    processLoginResponse(authService.loginUsername(username))

  override def loginToken(userToken: UserToken, username: Username): Future[UserContext] =
    processLoginResponse(authService.loginToken(userToken, username))

  private def processLoginResponse(loginResponseF: Future[UserContext]): Future[UserContext] = {
    loginResponseF.onComplete {
      case Success(ctx) =>
        // let rpcClientsService know about a new authenticated connection
        rpcClientsService.registerAuthenticatedConnection(clientId, ctx.username, ctx)
        gameService.reload(clientId, ctx.username)
      case Failure(_) =>
      // ignore
    }

    loginResponseF
  }

}
