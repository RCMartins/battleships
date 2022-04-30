package pt.rmartins.battleships.frontend.services.rpc

import pt.rmartins.battleships.shared.rpc.client.MainClientRPC
import pt.rmartins.battleships.shared.rpc.client.chat.ChatNotificationsRPC
import pt.rmartins.battleships.shared.rpc.client.game.GameNotificationsRPC

class RPCService(notificationsCenter: NotificationsCenter) extends MainClientRPC {

  override def game(): GameNotificationsRPC =
    new GameService(
      notificationsCenter.sendInviteRequestListeners,
      notificationsCenter.sendInviteResponseListeners,
      notificationsCenter.preGameStateListeners,
      notificationsCenter.preGameConfirmStateListeners,
      notificationsCenter.preGameEnemyRulesPatchListeners,
      notificationsCenter.gamePlayerRequestListeners,
      notificationsCenter.gamePlayerRequestAnswerListeners,
      notificationsCenter.gameStateListeners,
      notificationsCenter.gameModeListeners,
      notificationsCenter.quitGameListeners,
      notificationsCenter.userErrorMessageListeners
    )

  override val chat: ChatNotificationsRPC =
    new ChatService(
      notificationsCenter.msgListeners
    )

}
