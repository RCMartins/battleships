package pt.rmartins.battleships.backend.services

import io.udash.rpc.{ClientId, ClientRPCTarget, DefaultClientRPC}
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.GameState
import pt.rmartins.battleships.shared.rpc.client.MainClientRPC

import java.util.Date
import scala.collection.mutable
import scala.concurrent.Future

/** Helper object for server -> client calls. */
class RpcClientsService(sendToClientFactory: ClientRPCTarget => MainClientRPC) {

  private val clients: mutable.Set[ClientId] = mutable.HashSet.empty
  private val authClients: mutable.Map[ClientId, UserContext] = mutable.HashMap.empty
  private val clientUsernames: mutable.Map[String, ClientId] = mutable.HashMap.empty

  def sendToClient(target: ClientRPCTarget): MainClientRPC =
    sendToClientFactory(target)

  /** Returns active client ids. */
  def activeClients: Set[ClientId] = clients.toSet

  def getClientIdByUsername(username: String): Option[ClientId] = clientUsernames.get(username)

  /** Returns authenticated client ids. */
  def authenticatedClients: Map[ClientId, UserContext] = authClients.toMap

  /** Adds new connection ID to the set. */
  def registerConnection(clientId: ClientId): Unit = {
    clients += clientId
  }

  /** Adds new connection ID to the authenticated set. */
  def registerAuthenticatedConnection(
      clientId: ClientId,
      username: String,
      userContext: UserContext
  ): Unit = {
    clientUsernames.get(username) match {
      case Some(oldClientId) =>
        clientUsernames.remove(username)
        unregisterConnection(oldClientId)
      case None =>
    }
    clientUsernames += username -> clientId

    authClients(clientId) = userContext
    broadcastAuthConnectionsCount()
  }

  /** Removes connection ID from the list. */
  def unregisterConnection(clientId: ClientId): Unit = {
    clients -= clientId
    authClients.remove(clientId)
    broadcastAuthConnectionsCount()
    clientUsernames.toList.find(_._2 == clientId).foreach(pair => clientUsernames.remove(pair._1))
  }

  private def broadcastAuthConnectionsCount(): Unit =
    authClients.foreach { case (id, _) =>
      sendToClient(id).chat().connectionsCountUpdate(authClients.size)
    }

  def sendGameState(clientId: ClientId, gameState: GameState): Unit =
    authClients.get(clientId) match {
      case Some(_) =>
        sendToClient(clientId).game().gameStateUpdate(gameState)
      case _ =>
    }

  def sendMessage(message: String): Unit = {
    authClients.foreach { case (id, _) =>
      sendToClient(id).chat().newMessage(ChatMessage(message, "System", new Date()))
    }
  }

//  def sendGameState(player1: ClientId, player2: ClientId, gameState: ): Unit = {
//    List(player1, player2).map(clientId => (clientId, authClients.get(clientId))) match {
//      case clientIds @ List(_, _) =>
//        clientIds.foreach { case (id, _) =>
//          sendToClient(id).game().gameStateUpdate(gameState)
//        }
//      case _ =>
//    }
//  }

}

object RpcClientsService {
  val defaultSendToClientFactory: ClientRPCTarget => MainClientRPC = {
    import scala.concurrent.ExecutionContext.Implicits.global
    (target: ClientRPCTarget) => new DefaultClientRPC[MainClientRPC](target).get
  }
}
