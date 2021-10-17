package pt.rmartins.battleships.shared.rpc.server.secure

import io.udash.rpc._
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC

trait SecureRPC {
  def game(): GameRPC
}

object SecureRPC extends DefaultServerRpcCompanion[SecureRPC]
