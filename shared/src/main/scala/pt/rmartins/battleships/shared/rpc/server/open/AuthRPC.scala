package pt.rmartins.battleships.shared.rpc.server.open

import io.udash.rpc._
import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
import pt.rmartins.battleships.shared.model.game.Username

import scala.concurrent.Future

trait AuthRPC {

  /** Verifies provided username. Returns a UserContext if username is unique.
    */
  def loginUsername(username: Username): Future[UserContext]

  /** Verifies provided token and username. Returns a UserContext if provided data is valid.
    */
  def loginToken(userToken: UserToken, username: Username): Future[UserContext]
}

object AuthRPC extends DefaultServerRpcCompanion[AuthRPC]
