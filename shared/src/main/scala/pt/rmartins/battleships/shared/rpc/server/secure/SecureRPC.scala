package pt.rmartins.battleships.shared.rpc.server.secure

import pt.rmartins.battleships.shared.rpc.server.secure.chat.ChatRPC
import io.udash.rpc._
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC

trait SecureRPC {
  def game(): GameRPC
  def chat(): ChatRPC
}

object SecureRPC extends DefaultServerRpcCompanion[SecureRPC]
