package pt.rmartins.battleships.shared.rpc.client.game

import io.udash.rpc.DefaultClientRpcCompanion
import pt.rmartins.battleships.shared.model.game._

trait GameNotificationsRPC {

  def sendInviteRequest(inviterUsername: Username): Unit

  def sendInviteResponse(invitedUsername: Username, inviteAnswer: Boolean): Unit

  def sendPreGameState(preGameState: PreGameState): Unit

  def sendPreGameConfirmState(acceptedRules: Boolean, enemyAcceptedRules: Boolean): Unit

  def sendPreGameRulesPatch(preGameRulesPatch: PreGameRulesPatch): Unit

  def sendPlayerRequest(playerRequestType: PlayerRequestType): Unit

  def sendPlayerRequestAnswer(playerRequestType: PlayerRequestType, answer: Boolean): Unit

  def gameStateUpdate(gameState: GameState): Unit

  def gameModeUpdate(gameMode: GameMode): Unit

  def sendQuitGame(): Unit

  def newUserErrorMessage(userError: UserError): Unit

}

object GameNotificationsRPC extends DefaultClientRpcCompanion[GameNotificationsRPC]
