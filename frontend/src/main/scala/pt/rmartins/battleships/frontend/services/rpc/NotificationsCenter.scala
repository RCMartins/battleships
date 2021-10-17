package pt.rmartins.battleships.frontend.services.rpc

import io.udash.utils.{CallbacksHandler, Registration}
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.{Coordinate, GameMode, GameState}

/** Provides notifications about new messages and connections status. */
class NotificationsCenter {
  private[rpc] val logInListeners: CallbacksHandler[String] =
    new CallbacksHandler[String]
  private[rpc] val msgListeners: CallbacksHandler[ChatMessage] =
    new CallbacksHandler[ChatMessage]
  private[rpc] val quitGameListeners: CallbacksHandler[Unit] =
    new CallbacksHandler[Unit]
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
