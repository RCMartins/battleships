package pt.rmartins.battleships.frontend.views.login

import io.udash._
import pt.rmartins.battleships.frontend.routing._
import pt.rmartins.battleships.frontend.services.UserContextService
import pt.rmartins.battleships.frontend.views.game.Cookies
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.model.game.{AuthError, Username}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

/** Contains the business logic of this view. */
class LoginPagePresenter(
    model: ModelProperty[LoginPageModel],
    userService: UserContextService,
    application: Application[RoutingState]
)(implicit ec: ExecutionContext)
    extends Presenter[RoutingLoginPageState.type] {

  override def handleState(state: RoutingLoginPageState.type): Unit = {
    Cookies.getLoginCookieData().foreach { case (userToken, username) =>
      loggingIn(username, userService.loginToken(userToken, username))
    }
  }

  def login(): Future[Unit] = {
    val username = model.subProp(_.username).get.trim
    loggingIn(username, userService.loginUsername(username))
  }

  private def loggingIn(
      username: Username,
      loginF: Future[Either[AuthError, UserContext]]
  ): Future[Unit] = {
    model.subProp(_.username).set(username)
    model.subProp(_.waitingForResponse).set(true)
    model.subProp(_.errors).set(Seq.empty)

    def failure(): Unit = {
      model.subProp(_.waitingForResponse).set(false)
      Cookies.clearLoginCookieData()
    }

    loginF
      .map {
        case Left(error) =>
          failure()
          Left(error)
        case Right(ctx) =>
          model.subProp(_.waitingForResponse).set(false)
          Cookies.saveLoginCookieData(ctx.token, ctx.username)
          Right(ctx)
      }
      .andThen {
        case Success(Right(_)) =>
          application.goTo(RoutingInGameState)
        case Success(Left(AuthError.UnauthorizedException)) =>
        case Success(Left(AuthError.UserAlreadyExists)) =>
          model.subProp(_.errors).set(Seq(Translations.Auth.userAlreadyExists))
        case Success(Left(AuthError.UsernameInvalid)) =>
          model.subProp(_.errors).set(Seq(Translations.Auth.userNotFound))
        case _ =>
          model.subProp(_.errors).set(Seq(Translations.Global.unknownError))
      }
      .map(_ => ())
  }
}
