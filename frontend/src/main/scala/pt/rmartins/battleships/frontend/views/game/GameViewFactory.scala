package pt.rmartins.battleships.frontend.views.game

import io.udash._
import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.rpc.server.secure.SecureRPC

import scala.concurrent.ExecutionContext.Implicits.global

class GameViewFactory(
    userService: UserContextService,
    translationsService: TranslationsService,
    notificationsCenter: NotificationsCenter
) extends ViewFactory[RoutingInGameState.type] {

  override def create(): (View, Presenter[RoutingInGameState.type]) = {
    val preGameModel = ModelProperty[PreGameModel](PreGameModel.default)
    val gameModel = ModelProperty[GameModel](GameModel.default)
    val gameStateModel = ModelProperty[GameStateModel](GameStateModel(None))
    val chatModel = ModelProperty[ChatModel](ChatModel.default)
    val screenModel = ModelProperty[ScreenModel](ScreenModel.default)
    val translationsModel = ModelProperty[TranslationsModel](TranslationsModel.default)

    val rpcOpt: Option[SecureRPC] = userService.secureRpc()
    if (rpcOpt.isEmpty) throw SharedExceptions.UnauthorizedException()

    val gamePresenter: GamePresenter =
      new GamePresenter(
        preGameModel,
        gameModel,
        gameStateModel,
        chatModel,
        screenModel,
        translationsModel,
        rpcOpt.get.game(),
        userService,
        translationsService,
        notificationsCenter
      )
    val canvasUtils: CanvasUtils =
      new CanvasUtils(gamePresenter)
    val viewUtils: ViewUtils =
      new ViewUtils(canvasUtils)
    val preGameView =
      new PreGameView(
        preGameModel,
        screenModel,
        translationsModel,
        gamePresenter,
        canvasUtils,
        viewUtils,
        translationsService
      )

    val boardView: BoardView =
      new BoardView(gameModel, screenModel, translationsModel, gamePresenter, canvasUtils)
    val gameView: GameView =
      new GameView(
        preGameModel,
        gameModel,
        gameStateModel,
        chatModel,
        screenModel,
        translationsModel,
        gamePresenter,
        translationsService,
        boardView,
        preGameView,
        viewUtils
      )

    (gameView, gamePresenter)
  }
}
