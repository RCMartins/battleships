package pt.rmartins.battleships.frontend.services.rpc

import io.udash.utils.CallbacksHandler
import pt.rmartins.battleships.shared.model.game.GameState
import pt.rmartins.battleships.shared.rpc.client.game.GameNotificationsRPC

class GameService(
    gameStateListeners: CallbacksHandler[GameState]
) extends GameNotificationsRPC {

  override def gameStateUpdate(gameState: GameState): Unit =
    gameStateListeners.fire(gameState)

}
