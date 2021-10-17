package pt.rmartins.battleships.frontend.services.rpc

import pt.rmartins.battleships.shared.rpc.client.MainClientRPC
import pt.rmartins.battleships.shared.rpc.client.chat.ChatNotificationsRPC
import pt.rmartins.battleships.shared.rpc.client.game.GameNotificationsRPC

class RPCService(notificationsCenter: NotificationsCenter) extends MainClientRPC {

  override def game(): GameNotificationsRPC =
    new GameService(
      notificationsCenter.gameStateListeners,
      notificationsCenter.gameModeListeners,
      notificationsCenter.quitGameListeners
    )

  override val chat: ChatNotificationsRPC =
    new ChatService(
      notificationsCenter.msgListeners
    )

}
