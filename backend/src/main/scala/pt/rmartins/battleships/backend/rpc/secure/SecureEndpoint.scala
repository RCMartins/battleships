package pt.rmartins.battleships.backend.rpc.secure

import pt.rmartins.battleships.backend.rpc.secure.chat.ChatEndpoint
import pt.rmartins.battleships.backend.rpc.secure.game.GameEndpoint
import pt.rmartins.battleships.backend.services.DomainServices
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import pt.rmartins.battleships.shared.rpc.server.secure.SecureRPC
import pt.rmartins.battleships.shared.rpc.server.secure.chat.ChatRPC

class SecureEndpoint(implicit domainServices: DomainServices, ctx: UserContext) extends SecureRPC {
  import domainServices._

  lazy val gameEndpoint = new GameEndpoint

  lazy val chatEndpoint = new ChatEndpoint

  override def game(): GameRPC = gameEndpoint

  override def chat(): ChatRPC = chatEndpoint

}
