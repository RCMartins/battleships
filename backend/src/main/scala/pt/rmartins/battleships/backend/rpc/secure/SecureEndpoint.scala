package pt.rmartins.battleships.backend.rpc.secure

import pt.rmartins.battleships.backend.rpc.secure.game.GameEndpoint
import pt.rmartins.battleships.backend.services.DomainServices
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import pt.rmartins.battleships.shared.rpc.server.secure.SecureRPC

class SecureEndpoint(implicit domainServices: DomainServices, ctx: UserContext) extends SecureRPC {
  import domainServices._

  lazy val gameEndpoint = new GameEndpoint

  override def game(): GameRPC = gameEndpoint

}
