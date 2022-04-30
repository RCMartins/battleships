package pt.rmartins.battleships.frontend.services.rpc

import io.udash.utils.CallbacksHandler
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.rpc.client.game.GameNotificationsRPC

class GameService(
    sendInviteRequestListeners: CallbacksHandler[Username],
    sendInviteResponseListeners: CallbacksHandler[(Username, Boolean)],
    preGameStateListeners: CallbacksHandler[PreGameState],
    preGameConfirmStateListeners: CallbacksHandler[(Boolean, Boolean)],
    preGameEnemyRulesPatchListeners: CallbacksHandler[PreGameRulesPatch],
    gamePlayerRequestListeners: CallbacksHandler[PlayerRequestType],
    gamePlayerRequestAnswerListeners: CallbacksHandler[(PlayerRequestType, Boolean)],
    gameStateListeners: CallbacksHandler[GameState],
    gameModeListeners: CallbacksHandler[GameMode],
    quitGameListeners: CallbacksHandler[Unit],
    userErrorMessageListeners: CallbacksHandler[UserError]
) extends GameNotificationsRPC {

  override def sendInviteRequest(inviterUsername: Username): Unit =
    sendInviteRequestListeners.fire(inviterUsername)

  override def sendInviteResponse(invitedUsername: Username, inviteAnswer: Boolean): Unit =
    sendInviteResponseListeners.fire((invitedUsername, inviteAnswer))

  override def sendPreGameState(preGameState: PreGameState): Unit =
    preGameStateListeners.fire(preGameState)

  override def sendPreGameConfirmState(acceptedRules: Boolean, enemyAcceptedRules: Boolean): Unit =
    preGameConfirmStateListeners.fire((acceptedRules, enemyAcceptedRules))

  override def sendPreGameRulesPatch(preGameRulesPatch: PreGameRulesPatch): Unit =
    preGameEnemyRulesPatchListeners.fire(preGameRulesPatch)

  override def sendPlayerRequest(playerRequestType: PlayerRequestType): Unit =
    gamePlayerRequestListeners.fire(playerRequestType)

  override def sendPlayerRequestAnswer(
      playerRequestType: PlayerRequestType,
      answer: Boolean
  ): Unit =
    gamePlayerRequestAnswerListeners.fire((playerRequestType, answer))

  override def gameStateUpdate(gameState: GameState): Unit =
    gameStateListeners.fire(gameState)

  override def gameModeUpdate(gameMode: GameMode): Unit =
    gameModeListeners.fire(gameMode)

  override def sendQuitGame(): Unit =
    quitGameListeners.fire(())

  override def newUserErrorMessage(userError: UserError): Unit =
    userErrorMessageListeners.fire(userError)

}
