package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.macwire.wire
import io.udash._
import pt.rmartins.battleships.frontend.routing.RoutingInGameState
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.frontend.views.menu.{PuzzlesView, _}
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.rpc.server.secure.SecureRPC

import scala.concurrent.ExecutionContext.Implicits.global

class GameViewFactory(
    userService: UserContextService,
    translationsService: TranslationsService,
    notificationsCenter: NotificationsCenter
) extends ViewFactory[RoutingInGameState.type] {

  override def create(): (View, Presenter[RoutingInGameState.type]) = {
    val preGameModel = ModelProperty[PreGameModel](PreGameModel.Default)
    val gameModel = ModelProperty[GameModel](GameModel.Default)
    val gameStateModel = ModelProperty[GameStateModel](GameStateModel.Default)
    val chatModel = ModelProperty[ChatModel](ChatModel.Default)
    val screenModel = ModelProperty[ScreenModel](ScreenModel.Default)
    val translationsModel = ModelProperty[TranslationsModel](TranslationsModel.Default)

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
    val chatUtils: ChatUtils = wire[ChatUtils]
    val playesVsUtils: PlayerVsUtils = wire[PlayerVsUtils]
    val playerVsBotsView: PlayerVsBotsView = wire[PlayerVsBotsView]
    val playerVsPlayerView: PlayerVsPlayerView = wire[PlayerVsPlayerView]
    val puzzlesView: PuzzlesView = wire[PuzzlesView]
    val gameView: GameView = wire[GameView]

    (gameView, gamePresenter)
  }

}
