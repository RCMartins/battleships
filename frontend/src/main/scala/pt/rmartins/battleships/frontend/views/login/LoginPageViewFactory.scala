package pt.rmartins.battleships.frontend.views.login

import pt.rmartins.battleships.frontend.routing.{RoutingLoginPageState, RoutingState}
import pt.rmartins.battleships.frontend.services._
import io.udash._

/** Prepares model, view and presenter for demo view. */
class LoginPageViewFactory(
    userService: UserContextService,
    application: Application[RoutingState],
    translationsService: TranslationsService
) extends ViewFactory[RoutingLoginPageState.type] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override def create(): (View, Presenter[RoutingLoginPageState.type]) = {
    // Main model of the view
    val model = ModelProperty(
      LoginPageModel("", "", false, Seq.empty)
    )
    val presenter = new LoginPagePresenter(model, userService, application)
    val view = new LoginPageView(model, presenter, translationsService)
    (view, presenter)
  }
}
