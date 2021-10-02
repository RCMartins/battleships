package pt.rmartins.battleships.frontend.views.login

import pt.rmartins.battleships.frontend.routing.{
  RoutingInGameState,
  RoutingLoginPageState,
  RoutingState
}
import pt.rmartins.battleships.frontend.services.UserContextService
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.SharedExceptions
import io.udash._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Contains the business logic of this view. */
class LoginPagePresenter(
    model: ModelProperty[LoginPageModel],
    userService: UserContextService,
    application: Application[RoutingState]
)(implicit ec: ExecutionContext)
    extends Presenter[RoutingLoginPageState.type] {

  /** We don't need any initialization, so it's empty. */
  override def handleState(state: RoutingLoginPageState.type): Unit = {
    if (userService.currentContext.isDefined) {
      application.goTo(RoutingInGameState)
    }
  }

  def login(): Future[Unit] = {
    model.subProp(_.waitingForResponse).set(true)
    model.subProp(_.errors).set(Seq.empty)

    val username = model.subProp(_.username).get
    val password = model.subProp(_.password).get
    userService.login(username, password).map(_ => ()).andThen {
      case Success(_) =>
        model.subProp(_.waitingForResponse).set(false)
        application.goTo(RoutingInGameState)
      case Failure(_: SharedExceptions.UserNotFound) =>
        model.subProp(_.waitingForResponse).set(false)
        model.subProp(_.errors).set(Seq(Translations.Auth.userNotFound))
      case Failure(_) =>
        model.subProp(_.waitingForResponse).set(false)
        model.subProp(_.errors).set(Seq(Translations.Global.unknownError))
    }
  }
}
