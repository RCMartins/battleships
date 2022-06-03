package pt.rmartins.battleships.backend.services

import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.Inside.inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.model.game._

class AuthServiceTest extends AsyncWordSpec with Matchers with AsyncMockFactory {

  "loginUsername" should {

    "Return 'UsernameInvalid' error is the username is 'Bot'" in {
      val playerUsername1 = Username("Bot")
      val playerUsername2 = Username("bot")

      val service = new AuthService()

      for {
        response1 <- service.loginUsername(playerUsername1)
        response2 <- service.loginUsername(playerUsername2)
      } yield {
        response1 should be(Left(AuthError.UsernameInvalid))
        response2 should be(Left(AuthError.UsernameInvalid))
      }
    }

    "Return 'UsernameInvalid' error is the username is empty" in {
      val playerUsername = Username("")

      val service = new AuthService()

      for {
        response <- service.loginUsername(playerUsername)
      } yield {
        response should be(Left(AuthError.UsernameInvalid))
      }
    }

    "Return 'UsernameInvalid' error is the username has invalid characters" in {
      val playerUsername1 = Username("cool£name")
      val playerUsername2 = Username("player2§")

      val service = new AuthService()

      for {
        response1 <- service.loginUsername(playerUsername1)
        response2 <- service.loginUsername(playerUsername2)
      } yield {
        response1 should be(Left(AuthError.UsernameInvalid))
        response2 should be(Left(AuthError.UsernameInvalid))
      }
    }

    "Return 'UsernameTooLong' error is the username is too long" in {
      val playerUsername = Username("R" * 26)

      val service = new AuthService()

      for {
        response <- service.loginUsername(playerUsername)
      } yield {
        response should be(Left(AuthError.UsernameTooLong))
      }
    }

    "Return 'UserAlreadyExists' error is the username already exists" in {
      val playerUsername = Username("player")

      val service = new AuthService()

      for {
        _ <- service.loginUsername(playerUsername)
        response <- service.loginUsername(playerUsername)
      } yield {
        response should be(Left(AuthError.UserAlreadyExists))
      }
    }

    "Return the username context with name with diacritics" in {
      val playerUsername1 = Username("çÇáàäâ@ÀÁÂÃÄÅ")
      val playerUsername2 = Username("ÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ")

      val service = new AuthService()

      for {
        response1 <- service.loginUsername(playerUsername1)
        response2 <- service.loginUsername(playerUsername2)
      } yield {
        inside(response1) { case Right(UserContext(_, username)) =>
          username should be(playerUsername1)
        }
        inside(response2) { case Right(UserContext(_, username)) =>
          username should be(playerUsername2)
        }
      }
    }

    "Return the username context" in {
      val playerUsername = Username("player")

      val service = new AuthService()

      for {
        response <- service.loginUsername(playerUsername)
      } yield {
        inside(response) { case Right(UserContext(_, username)) =>
          username should be(playerUsername)
        }
      }
    }

  }

}
