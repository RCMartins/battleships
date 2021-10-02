package pt.rmartins.battleships.frontend.routing

import pt.rmartins.battleships.frontend.ApplicationContext
import pt.rmartins.battleships.frontend.views.RootViewFactory
import pt.rmartins.battleships.frontend.views.login.LoginPageViewFactory
import io.udash._
import pt.rmartins.battleships.frontend.views.game.GameViewFactory

class StatesToViewFactoryDef extends ViewFactoryRegistry[RoutingState] {
  def matchStateToResolver(state: RoutingState): ViewFactory[_ <: RoutingState] =
    state match {
      case RoutingRootState =>
        new RootViewFactory(
          ApplicationContext.translationsService
        )
      case RoutingLoginPageState =>
        new LoginPageViewFactory(
          ApplicationContext.userService,
          ApplicationContext.application,
          ApplicationContext.translationsService
        )
      case RoutingInGameState =>
        new GameViewFactory(
          ApplicationContext.userService,
          ApplicationContext.translationsService,
          ApplicationContext.notificationsCenter
        )
    }
}
