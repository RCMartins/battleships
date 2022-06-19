package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.macwire.wire
import io.udash._
import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.frontend.views.menu._
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
    val gameStateModel = ModelProperty[GameStateModel](GameStateModel(None, None))
    val chatModel = ModelProperty[ChatModel](ChatModel.default)
    val screenModel = ModelProperty[ScreenModel](ScreenModel.default)
    val translationsModel = ModelProperty[TranslationsModel](TranslationsModel.default)

    val rpcOpt: Option[SecureRPC] = userService.secureRpc()
    if (rpcOpt.isEmpty) throw SharedExceptions.UnauthorizedException()
    val gameRpc = rpcOpt.get.game()

    val gamePresenter: GamePresenter = wire[GamePresenter]
    val mousePresenter: MousePresenter = wire[MousePresenter]
    val canvasUtils: CanvasUtils = wire[CanvasUtils]
    val viewUtils: ViewUtils = wire[ViewUtils]
    val gameModals: GameModals = wire[GameModals]
    val preGameView: PreGameView = wire[PreGameView]
    val boardView: BoardView = wire[BoardView]
    val playesVsUtils: PlayesVsUtils = wire[PlayesVsUtils]
    val playerVsBotsView: PlayerVsBotsView = wire[PlayerVsBotsView]
    val playerVsPlayerView: PlayerVsPlayerView = wire[PlayerVsPlayerView]
    val gameView: GameView = wire[GameView]

    (gameView, gamePresenter)
  }

}
