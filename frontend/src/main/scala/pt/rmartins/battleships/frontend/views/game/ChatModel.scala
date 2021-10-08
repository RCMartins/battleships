package pt.rmartins.battleships.frontend.views.game

import io.udash._
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.Username

case class ChatModel(
    username: Username,
    msgs: Seq[ChatMessage],
    msgInput: String,
    connectionsCount: Int
)

object ChatModel extends HasModelPropertyCreator[ChatModel] {

  val default: ChatModel =
    ChatModel(Username(""), Seq.empty, "", 0)

}
