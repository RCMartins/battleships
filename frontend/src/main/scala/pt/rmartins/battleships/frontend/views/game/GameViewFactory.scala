package pt.rmartins.battleships.frontend.views.game

import io.udash._
import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.model.game.Coordinate

class GameViewFactory(
    userService: UserContextService,
    translationsService: TranslationsService,
    notificationsCenter: NotificationsCenter
) extends ViewFactory[RoutingInGameState.type] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override def create(): (View, Presenter[RoutingInGameState.type]) = {
    val gameModel = ModelProperty[GameModel](GameModel.default)
    val gameStateModel = ModelProperty[GameStateModel](GameStateModel(None))
    val chatModel = ModelProperty[ChatModel](ChatModel.default)
    val screenModel = ModelProperty[ScreenModel](ScreenModel.default)

    val rpc = userService.secureRpc()
    if (rpc.isEmpty) throw SharedExceptions.UnauthorizedException()

    val gamePresenter: GamePresenter =
      new GamePresenter(
        gameModel,
        gameStateModel,
        chatModel,
        screenModel,
        rpc.get.game(),
        rpc.get.chat(),
        userService,
        notificationsCenter
      )
    val gameView: GameView =
      new GameView(
        gameModel,
        gameStateModel,
        chatModel,
        screenModel,
        gamePresenter,
        translationsService
      )

    (gameView, gamePresenter)
  }
}
