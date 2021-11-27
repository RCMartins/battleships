package pt.rmartins.battleships.shared.rpc.client.game

import io.udash.rpc.DefaultClientRpcCompanion
import pt.rmartins.battleships.shared.model.game._

trait GameNotificationsRPC {

  def sendPreGameState(preGameState: PreGameState): Unit

  def sendPreGameConfirmState(acceptedRules: Boolean, enemyAcceptedRules: Boolean): Unit

  def sendPreGameRulesPatch(preGameRulesPatch: PreGameRulesPatch): Unit

  def gameStateUpdate(gameState: GameState): Unit

  def gameModeUpdate(gameMode: GameMode): Unit

  def sendQuitGame(): Unit

}

object GameNotificationsRPC extends DefaultClientRpcCompanion[GameNotificationsRPC]
