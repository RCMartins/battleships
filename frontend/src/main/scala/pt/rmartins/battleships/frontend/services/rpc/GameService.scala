package pt.rmartins.battleships.frontend.services.rpc

import io.udash.utils.CallbacksHandler
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.rpc.client.game.GameNotificationsRPC

class GameService(
    preGameStateListeners: CallbacksHandler[PreGameState],
    preGameConfirmStateListeners: CallbacksHandler[(Boolean, Boolean)],
    preGameEnemyRulesPatchListeners: CallbacksHandler[PreGameRulesPatch],
    gameStateListeners: CallbacksHandler[GameState],
    gameModeListeners: CallbacksHandler[GameMode],
    quitGameListeners: CallbacksHandler[Unit]
) extends GameNotificationsRPC {

  override def sendPreGameState(preGameState: PreGameState): Unit =
    preGameStateListeners.fire(preGameState)

  override def sendPreGameConfirmState(acceptedRules: Boolean, enemyAcceptedRules: Boolean): Unit =
    preGameConfirmStateListeners.fire((acceptedRules, enemyAcceptedRules))

  override def sendPreGameRulesPatch(preGameRulesPatch: PreGameRulesPatch): Unit =
    preGameEnemyRulesPatchListeners.fire(preGameRulesPatch)

  override def gameStateUpdate(gameState: GameState): Unit =
    gameStateListeners.fire(gameState)

  override def gameModeUpdate(gameMode: GameMode): Unit =
    gameModeListeners.fire(gameMode)

  override def sendQuitGame(): Unit =
    quitGameListeners.fire(())

}
