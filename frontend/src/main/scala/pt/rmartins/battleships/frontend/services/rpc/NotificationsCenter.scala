package pt.rmartins.battleships.frontend.services.rpc

import io.udash.utils.{CallbacksHandler, Registration}
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.{Coordinate, GameState}

/** Provides notifications about new messages and connections status. */
class NotificationsCenter {
  private[rpc] val logInListeners: CallbacksHandler[String] =
    new CallbacksHandler[String]
  private[rpc] val msgListeners: CallbacksHandler[ChatMessage] =
    new CallbacksHandler[ChatMessage]
  private[rpc] val connectionsListeners: CallbacksHandler[Int] =
    new CallbacksHandler[Int]
  private[rpc] val gameViewListeners: CallbacksHandler[GameState] =
    new CallbacksHandler[GameState]
  private[rpc] val receiveHitsListeners: CallbacksHandler[Seq[Coordinate]] =
    new CallbacksHandler[Seq[Coordinate]]

  def onLogin(
      callback: logInListeners.CallbackType
  ): Registration =
    logInListeners.register(callback)

  def onNewMsg(callback: msgListeners.CallbackType): Registration =
    msgListeners.register(callback)

  def onConnectionsCountChange(
      callback: connectionsListeners.CallbackType
  ): Registration =
    connectionsListeners.register(callback)

  def onGameView(
      callback: gameViewListeners.CallbackType
  ): Registration =
    gameViewListeners.register(callback)

  def onReceiveHits(
      callback: receiveHitsListeners.CallbackType
  ): Registration =
    receiveHitsListeners.register(callback)

}
