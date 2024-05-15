package pt.rmartins.battleships.backend.services

import com.avsystem.commons._
import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
import pt.rmartins.battleships.shared.model.game.{AuthError, Username}

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex

class AuthService() {

  private val usersLogged: MHashSet[Username] = MHashSet.empty
  private val tokens: MMap[UserToken, UserContext] = MMap.empty

  private val ValidNonLettersRegex: Regex = "[ \\d\\-+*=_!@#$%&<>.\"']*".r

  private def validUsername(username: Username): Boolean =
    username.username.exists(_.isLetterOrDigit) &&
      ValidNonLettersRegex.matches(username.username.filterNot(_.isLetter)) &&
      username.toLowerCase != GameService.BotUsername.toLowerCase

  def logout(userToken: UserToken): Future[Unit] = Future {
    tokens.synchronized {
      tokens.remove(userToken).foreach { ctx =>
        usersLogged.remove(ctx.username.toLowerCase)
      }
    }
  }

  /** Tries to authenticate user with provided credentials. */
  def loginUsername(username: Username): Future[Either[AuthError, UserContext]] = Future {
    val usernameLower = username.toLowerCase

    if (usernameLower.username.length > Username.MaximumUserNameLength)
      Left(AuthError.UsernameTooLong)
    else if (!validUsername(usernameLower))
      Left(AuthError.UsernameInvalid)
    else if (tokens.synchronized(usersLogged(usernameLower)))
      Left(AuthError.UserAlreadyExists)
    else {
      val token = UserToken(UUID.randomUUID().toString)
      val ctx = UserContext(
        token,
        username
      )

      tokens.synchronized {
        tokens(token) = ctx
        usersLogged += usernameLower
      }

      println(s"New connection: $username")

      Right(ctx)
    }
  }

  def loginToken(userToken: UserToken, username: Username): Future[Either[AuthError, UserContext]] =
    Future {
      tokens.synchronized {
        tokens.get(userToken)
      } match {
        case Some(userContext) if userContext.username.toLowerCase == username.toLowerCase =>
          Right(userContext)
        case _ =>
          Left(AuthError.UnauthorizedException)
      }
    }

  def findUserCtx(userToken: UserToken): Option[UserContext] =
    tokens.synchronized { tokens.get(userToken) }

}
