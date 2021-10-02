package pt.rmartins.battleships.shared.rpc.client.game

import io.udash.rpc.DefaultClientRpcCompanion
import pt.rmartins.battleships.shared.model.game.GameState

trait GameNotificationsRPC {

  def gameStateUpdate(gameState: GameState): Unit

}

object GameNotificationsRPC extends DefaultClientRpcCompanion[GameNotificationsRPC]
