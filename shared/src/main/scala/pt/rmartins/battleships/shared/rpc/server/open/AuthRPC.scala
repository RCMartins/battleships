package pt.rmartins.battleships.shared.rpc.server.open

import pt.rmartins.battleships.shared.model.auth.UserContext
import io.udash.rpc._
import pt.rmartins.battleships.shared.model.game.Username

import scala.concurrent.Future

trait AuthRPC {

  /** Verifies provided username and password. Returns a UserContext if provided data is valid.
    */
  def login(username: Username, password: String): Future[UserContext]
}

object AuthRPC extends DefaultServerRpcCompanion[AuthRPC]
