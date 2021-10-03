package pt.rmartins.battleships.frontend.views.game

import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.shared.model.SharedExceptions
import io.udash._
import pt.rmartins.battleships.shared.model.game.Coordinate

class GameViewFactory(
    userService: UserContextService,
    translationsService: TranslationsService,
    notificationsCenter: NotificationsCenter
) extends ViewFactory[RoutingInGameState.type] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override def create(): (View, Presenter[RoutingInGameState.type]) = {
    val gameModel = ModelProperty[GameModel](GameModel(None, None, None))
    val chatModel = ModelProperty[ChatModel](ChatModel("", Seq.empty, "", 0))
    val screenModel = ModelProperty[ScreenModel](ScreenModel(Coordinate(1000, 400), "chat"))

    val rpc = userService.secureRpc()
    if (rpc.isEmpty) throw SharedExceptions.UnauthorizedException()

    val gamePresenter =
      new GamePresenter(
        gameModel,
        chatModel,
        screenModel,
        rpc.get.game(),
        rpc.get.chat(),
        userService,
        notificationsCenter
      )
    val view = new GameView(gameModel, chatModel, screenModel, gamePresenter, translationsService)

    (view, gamePresenter)
  }
}
