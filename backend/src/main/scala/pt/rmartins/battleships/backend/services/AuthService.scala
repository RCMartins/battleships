package pt.rmartins.battleships.backend.services

import com.avsystem.commons._
import com.softwaremill.quicklens.ModifyPimp
import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
import pt.rmartins.battleships.shared.model.game.{AuthError, Username}

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex

class AuthService() {
  private val usersLogged: MHashSet[Username] = MHashSet.empty
  private val tokens: MMap[UserToken, UserContext] = MMap.empty

  private val ValidUsernameRegex: Regex = "[a-zA-Z0-9 \\-+*=_!@#$%&<>.]+".r

  private def validUsername(username: Username): Boolean =
    username.username.exists(_.isLetterOrDigit) && ValidUsernameRegex.matches(username.username) &&
      username != GameService.BotUsername

  def logout(userToken: UserToken): Future[Unit] = Future {
    tokens.synchronized {
      tokens.remove(userToken).foreach { ctx =>
        usersLogged.remove(ctx.username)
      }
    }
  }

  /** Tries to authenticate user with provided credentials. */
  def loginUsername(username: Username): Future[Either[AuthError, UserContext]] = Future {
    val usernameTrimmed = username.modify(_.username).using(_.trim)

    if (tokens.synchronized(usersLogged(usernameTrimmed)))
      Left(AuthError.UserAlreadyExists)
    else if (!validUsername(usernameTrimmed))
      Left(AuthError.UsernameInvalid)
    else {
      val token = UserToken(UUID.randomUUID().toString)
      val ctx = UserContext(
        token,
        usernameTrimmed
      )

      tokens.synchronized {
        tokens(token) = ctx
        usersLogged += usernameTrimmed
      }

      Right(ctx)
    }
  }

  def loginToken(userToken: UserToken, username: Username): Future[Either[AuthError, UserContext]] =
    Future {
      tokens.synchronized {
        tokens.get(userToken)
      } match {
        case Some(userContext) if userContext.username == username =>
          Right(userContext)
        case _ =>
          Left(AuthError.UnauthorizedException)
      }
    }

  def findUserCtx(userToken: UserToken): Option[UserContext] =
    tokens.synchronized { tokens.get(userToken) }
}
