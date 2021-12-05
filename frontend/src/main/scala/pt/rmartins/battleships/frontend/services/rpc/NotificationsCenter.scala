package pt.rmartins.battleships.frontend.services.rpc

import io.udash.utils.{CallbacksHandler, Registration}
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game._

/** Provides notifications about new messages and connections status. */
class NotificationsCenter {
  private[rpc] val logInListeners: CallbacksHandler[String] =
    new CallbacksHandler[String]
  private[rpc] val msgListeners: CallbacksHandler[ChatMessage] =
    new CallbacksHandler[ChatMessage]
  private[rpc] val quitGameListeners: CallbacksHandler[Unit] =
    new CallbacksHandler[Unit]
  private[rpc] val preGameStateListeners: CallbacksHandler[PreGameState] =
    new CallbacksHandler[PreGameState]
  private[rpc] val preGameConfirmStateListeners: CallbacksHandler[(Boolean, Boolean)] =
    new CallbacksHandler[(Boolean, Boolean)]
  private[rpc] val preGameEnemyRulesPatchListeners: CallbacksHandler[PreGameRulesPatch] =
    new CallbacksHandler[PreGameRulesPatch]
  private[rpc] val gamePlayerRequestListeners: CallbacksHandler[PlayerRequestType] =
    new CallbacksHandler[PlayerRequestType]
  private[rpc] val gamePlayerRequestAnswerListeners
      : CallbacksHandler[(PlayerRequestType, Boolean)] =
    new CallbacksHandler[(PlayerRequestType, Boolean)]
  private[rpc] val gameStateListeners: CallbacksHandler[GameState] =
    new CallbacksHandler[GameState]
  private[rpc] val gameModeListeners: CallbacksHandler[GameMode] =
    new CallbacksHandler[GameMode]
  private[rpc] val receiveHitsListeners: CallbacksHandler[Seq[Coordinate]] =
    new CallbacksHandler[Seq[Coordinate]]

  def onLogin(
      callback: logInListeners.CallbackType
  ): Registration =
    logInListeners.register(callback)

  def onNewMsg(callback: msgListeners.CallbackType): Registration =
    msgListeners.register(callback)

  def onUpdatePreGameState(
      callback: preGameStateListeners.CallbackType
  ): Registration =
    preGameStateListeners.register(callback)

  def onPreGameConfirmState(
      callback: preGameConfirmStateListeners.CallbackType
  ): Registration =
    preGameConfirmStateListeners.register(callback)

  def onPreGameRulesPatch(
      callback: preGameEnemyRulesPatchListeners.CallbackType
  ): Registration =
    preGameEnemyRulesPatchListeners.register(callback)

  def onPlayerRequest(
      callback: gamePlayerRequestListeners.CallbackType
  ): Registration =
    gamePlayerRequestListeners.register(callback)

  def onPlayerRequestAnswer(
      callback: gamePlayerRequestAnswerListeners.CallbackType
  ): Registration =
    gamePlayerRequestAnswerListeners.register(callback)

  def onQuitGame(
      callback: quitGameListeners.CallbackType
  ): Registration =
    quitGameListeners.register(callback)

  def onGameState(
      callback: gameStateListeners.CallbackType
  ): Registration =
    gameStateListeners.register(callback)

  def onGameMode(
      callback: gameModeListeners.CallbackType
  ): Registration =
    gameModeListeners.register(callback)

  def onReceiveHits(
      callback: receiveHitsListeners.CallbackType
  ): Registration =
    receiveHitsListeners.register(callback)

}
