package pt.rmartins.battleships.backend.rpc.secure.chat

import pt.rmartins.battleships.backend.services.{ChatService, RpcClientsService}
import pt.rmartins.battleships.shared.model.auth.{Permission, UserContext}
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.rpc.server.secure.chat.ChatRPC
import io.udash.auth._

import scala.concurrent.Future

/** Verifies user's permissions and passes valid requests to the services. */
class ChatEndpoint(implicit
    chatService: ChatService,
    rpcClientsService: RpcClientsService,
    ctx: UserContext
) extends ChatRPC
    with AuthRequires {

  override def sendMsg(msg: String): Future[Unit] = {
    require(Permission.ChatWrite)
    chatService.sendMsg(ctx.name, msg)
  }

  override def latestMessages(): Future[Seq[ChatMessage]] = {
    require(Permission.ChatRead)
    chatService.latestMessages()
  }

  override def connectedClientsCount(): Future[Int] =
    Future.successful(rpcClientsService.authenticatedClients.size)

}
