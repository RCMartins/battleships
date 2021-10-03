package pt.rmartins.battleships.frontend.services.rpc

import io.udash.utils.CallbacksHandler
import pt.rmartins.battleships.shared.model.game.{GameMode, GameState}
import pt.rmartins.battleships.shared.rpc.client.game.GameNotificationsRPC

class GameService(
    gameStateListeners: CallbacksHandler[GameState],
    gameModeListeners: CallbacksHandler[GameMode],
    quitGameListeners: CallbacksHandler[Unit]
) extends GameNotificationsRPC {

  override def gameStateUpdate(gameState: GameState): Unit =
    gameStateListeners.fire(gameState)

  override def gameModeUpdate(gameMode: GameMode): Unit =
    gameModeListeners.fire(gameMode)

  override def sendQuitGame(): Unit =
    quitGameListeners.fire(())

}
