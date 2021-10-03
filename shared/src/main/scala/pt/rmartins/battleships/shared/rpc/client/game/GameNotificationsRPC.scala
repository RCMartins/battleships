package pt.rmartins.battleships.shared.rpc.client.game

import io.udash.rpc.DefaultClientRpcCompanion
import pt.rmartins.battleships.shared.model.game.{GameMode, GameState}

trait GameNotificationsRPC {

  def gameStateUpdate(gameState: GameState): Unit

  def gameModeUpdate(gameMode: GameMode): Unit

  def sendQuitGame(): Unit

}

object GameNotificationsRPC extends DefaultClientRpcCompanion[GameNotificationsRPC]
