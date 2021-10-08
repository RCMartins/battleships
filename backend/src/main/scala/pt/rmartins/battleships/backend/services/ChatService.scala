package pt.rmartins.battleships.backend.services

import pt.rmartins.battleships.shared.model.auth.Permission
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.Username

import java.util.Date
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ChatService(rpcClientsService: RpcClientsService) {
  private val msgs: mutable.ArrayBuffer[ChatMessage] = mutable.ArrayBuffer.empty

  /** Saves new message and notifies all clients with read access. */
  def sendMsg(username: Username, text: String): Future[Unit] = Future {
    val trimmed = text.trim
    if (trimmed.nonEmpty) {
      val msg = ChatMessage(trimmed, username.username, new Date())
      msgs.synchronized(msgs += msg)

      rpcClientsService.authenticatedClients.foreach { case (id, context) =>
        if (context.has(Permission.ChatRead)) {
          rpcClientsService.sendToClient(id).chat().newMessage(msg)
        }
      }
    }
  }

  /** Returns all existing messages. */
  def latestMessages(): Future[Seq[ChatMessage]] = Future {
    msgs.synchronized(msgs.toVector)
  }
}
