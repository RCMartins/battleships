package pt.rmartins.battleships.frontend.services

import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.model.game.{AuthError, Username}
import pt.rmartins.battleships.shared.rpc.server.MainServerRPC
import pt.rmartins.battleships.shared.rpc.server.secure.SecureRPC

import scala.concurrent.{ExecutionContext, Future}

/** Holds an active `UserContext` and provides access to the `SecureRPC` with a current `UserToken`.
  */
class UserContextService(rpc: MainServerRPC)(implicit ec: ExecutionContext) {
  private var userContext: Option[UserContext] = None

  def currentContext: Option[UserContext] = userContext
  def getCurrentContext: UserContext =
    userContext.getOrElse(throw new SharedExceptions.UnauthorizedException)

  def logout(): Future[Unit] = {
    userContext match {
      case None =>
        Future.successful(())
      case Some(ctx) =>
        rpc.auth().logout(ctx.token).map { _ =>
          userContext = None
          ()
        }
    }
  }

  /** Sends login request and saves returned context. */
  def loginUsername(username: Username): Future[Either[AuthError, UserContext]] = {
    if (userContext.isDefined)
      Future.successful(Right(userContext.get))
    else {
      rpc
        .auth()
        .loginUsername(username)
        .map {
          case Left(error) =>
            userContext = None
            Left(error)
          case Right(ctx) =>
            userContext = Some(ctx)
            Right(ctx)
        }
    }
  }

  def loginToken(
      userToken: UserToken,
      username: Username
  ): Future[Either[AuthError, UserContext]] = {
    if (userContext.isDefined)
      Future.successful(Right(userContext.get))
    else {
      rpc.auth().loginToken(userToken, username).map {
        case Left(error) =>
          userContext = None
          Left(error)
        case Right(ctx) =>
          userContext = Some(ctx)
          Right(ctx)
      }
    }
  }

  /** Provides access to SecureRPC with current UserToken. */
  def secureRpc(): Option[SecureRPC] =
    userContext.map(ctx => rpc.secure(ctx.token))
}
