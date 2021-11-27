package pt.rmartins.battleships.backend.services

import io.udash.rpc.{ClientId, ClientRPCTarget, DefaultClientRPC}
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.rpc.client.MainClientRPC

import java.util.Date
import scala.collection.mutable

/** Helper object for server -> client calls. */
class RpcClientsService(sendToClientFactory: ClientRPCTarget => MainClientRPC) {

  private val clients: mutable.Set[ClientId] = mutable.HashSet.empty
  private val authClients: mutable.Map[ClientId, UserContext] = mutable.HashMap.empty
  private val clientUsernames: mutable.Map[Username, ClientId] = mutable.HashMap.empty

  def sendToClient(target: ClientRPCTarget): MainClientRPC =
    sendToClientFactory(target)

  /** Returns active client ids. */
  def activeClients: Set[ClientId] = clients.toSet

  def getClientIdByUsername(username: Username): Option[ClientId] =
    clientUsernames.get(username.toLowerCase)

  /** Returns authenticated client ids. */
  def authenticatedClients: Map[ClientId, UserContext] = authClients.toMap

  /** Adds new connection ID to the set. */
  def registerConnection(clientId: ClientId): Unit = {
    clients += clientId
  }

  /** Adds new connection ID to the authenticated set. */
  def registerAuthenticatedConnection(
      clientId: ClientId,
      username: Username,
      userContext: UserContext
  ): Unit = {
    val lowercaseName = username.toLowerCase
    clientUsernames.get(lowercaseName) match {
      case Some(oldClientId) =>
        clientUsernames.remove(lowercaseName)
        unregisterConnection(oldClientId)
      case None =>
    }
    clientUsernames += lowercaseName -> clientId

    authClients(clientId) = userContext
  }

  /** Removes connection ID from the list. */
  def unregisterConnection(clientId: ClientId): Unit = {
    clients -= clientId
    authClients.remove(clientId).foreach(ctx => clientUsernames.remove(ctx.username.toLowerCase))
  }

  def sendPreGameState(clientId: ClientId, preGameState: PreGameState): Unit =
    authClients.get(clientId) match {
      case Some(_) =>
        sendToClient(clientId).game().sendPreGameState(preGameState)
      case _ =>
    }

  def sendPreGameConfirmStates(
      clientId: ClientId,
      acceptedRules: Boolean,
      enemyAcceptedRules: Boolean
  ): Unit =
    authClients.get(clientId) match {
      case Some(_) =>
        sendToClient(clientId).game().sendPreGameConfirmState(acceptedRules, enemyAcceptedRules)
      case _ =>
    }

  def sendPreGameRulesPatch(clientId: ClientId, preGameRulesPatch: PreGameRulesPatch): Unit =
    authClients.get(clientId) match {
      case Some(_) =>
        sendToClient(clientId).game().sendPreGameRulesPatch(preGameRulesPatch)
      case _ =>
    }

  def sendGameState(clientId: ClientId, gameState: GameState): Unit =
    authClients.get(clientId) match {
      case Some(_) =>
        sendToClient(clientId).game().gameStateUpdate(gameState)
      case _ =>
    }

  def sendGameMode(clientId: ClientId, gameMode: GameMode): Unit =
    authClients.get(clientId) match {
      case Some(_) =>
        sendToClient(clientId).game().gameModeUpdate(gameMode)
      case _ =>
    }

  def sendQuitGame(clientId: ClientId): Unit =
    authClients.get(clientId) match {
      case Some(_) =>
        sendToClient(clientId).game().sendQuitGame()
      case _ =>
    }

  def sendMessage(clientId: ClientId, message: String): Unit = {
    sendToClient(clientId).chat().newMessage(ChatMessage(message, "System", new Date()))
  }

  def sendMessage(clientId: ClientId, chatMessage: ChatMessage): Unit = {
    sendToClient(clientId).chat().newMessage(chatMessage)
  }

}

object RpcClientsService {
  val defaultSendToClientFactory: ClientRPCTarget => MainClientRPC = {
    import scala.concurrent.ExecutionContext.Implicits.global
    (target: ClientRPCTarget) => new DefaultClientRPC[MainClientRPC](target).get
  }
}
