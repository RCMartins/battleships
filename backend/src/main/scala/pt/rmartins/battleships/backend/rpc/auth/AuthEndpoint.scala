package pt.rmartins.battleships.backend.rpc.auth

import io.udash.rpc.ClientId
import pt.rmartins.battleships.backend.services.{AuthService, GameService, RpcClientsService}
import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
import pt.rmartins.battleships.shared.model.game.{AuthError, Username}
import pt.rmartins.battleships.shared.rpc.server.open.AuthRPC

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success, Try}

class AuthEndpoint(implicit
    authService: AuthService,
    gameService: GameService,
    rpcClientsService: RpcClientsService,
    clientId: ClientId
) extends AuthRPC {

  override def logout(userToken: UserToken): Future[Unit] =
    authService.logout(userToken)

  override def loginUsername(username: Username): Future[Either[AuthError, UserContext]] =
    processLoginResponse(authService.loginUsername(username))

  override def loginToken(
      userToken: UserToken,
      username: Username
  ): Future[Either[AuthError, UserContext]] =
    processLoginResponse(authService.loginToken(userToken, username))

  private def processLoginResponse(
      loginResponseF: Future[Either[AuthError, UserContext]]
  ): Future[Either[AuthError, UserContext]] = {
    loginResponseF.onComplete {
      case Success(Right(ctx)) =>
        // let rpcClientsService know about a new authenticated connection
        rpcClientsService.registerAuthenticatedConnection(clientId, ctx.username, ctx)
        gameService.reload(clientId, ctx.username)
      case _ =>
      // ignore
    }

    loginResponseF
  }

}
