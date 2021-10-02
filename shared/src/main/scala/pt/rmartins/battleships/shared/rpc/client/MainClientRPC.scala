package pt.rmartins.battleships.shared.rpc.client

import pt.rmartins.battleships.shared.rpc.client.chat.ChatNotificationsRPC
import io.udash.rpc._
import pt.rmartins.battleships.shared.rpc.client.game.GameNotificationsRPC

trait MainClientRPC {
  def game(): GameNotificationsRPC
  def chat(): ChatNotificationsRPC
}

object MainClientRPC extends DefaultClientRpcCompanion[MainClientRPC]
