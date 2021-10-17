package pt.rmartins.battleships.frontend.views.login

import io.udash._
import pt.rmartins.battleships.frontend.routing._
import pt.rmartins.battleships.frontend.services.UserContextService
import pt.rmartins.battleships.frontend.views.game.Cookies
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.model.game.Username

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Contains the business logic of this view. */
class LoginPagePresenter(
    model: ModelProperty[LoginPageModel],
    userService: UserContextService,
    application: Application[RoutingState]
)(implicit ec: ExecutionContext)
    extends Presenter[RoutingLoginPageState.type] {

  override def handleState(state: RoutingLoginPageState.type): Unit = {
    Cookies.getCookieData().foreach { case (userToken, username) =>
      loggingIn(username, userService.loginToken(userToken, username))
    }
  }

  def login(): Future[Unit] = {
    val username = model.subProp(_.username).get
    loggingIn(username, userService.loginUsername(username))
  }

  private def loggingIn(username: Username, loginF: Future[UserContext]): Future[Unit] = {
    model.subProp(_.username).set(username)
    model.subProp(_.waitingForResponse).set(true)
    model.subProp(_.errors).set(Seq.empty)

    def failure(): Unit = {
      model.subProp(_.waitingForResponse).set(false)
      Cookies.clearCookies()
    }

    loginF
      .map { ctx =>
        Cookies.saveCookieData(ctx.token, ctx.username)
        ()
      }
      .andThen {
        case Success(_) =>
          model.subProp(_.waitingForResponse).set(false)
          application.goTo(RoutingInGameState)
        case Failure(_: SharedExceptions.UnauthorizedException) =>
          failure()
        case Failure(_: SharedExceptions.UserAlreadyExists) =>
          failure()
          model.subProp(_.errors).set(Seq(Translations.Auth.userAlreadyExists))
        case Failure(_: SharedExceptions.UsernameInvalid) =>
          failure()
          model.subProp(_.errors).set(Seq(Translations.Auth.userNotFound))
        case Failure(_) =>
          failure()
          model.subProp(_.errors).set(Seq(Translations.Global.unknownError))
      }
  }
}
