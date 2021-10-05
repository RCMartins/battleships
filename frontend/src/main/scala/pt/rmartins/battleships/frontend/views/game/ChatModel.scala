package pt.rmartins.battleships.frontend.views.game

import io.udash._
import pt.rmartins.battleships.shared.model.chat.ChatMessage

case class ChatModel(
    username: String,
    msgs: Seq[ChatMessage],
    msgInput: String,
    connectionsCount: Int
)

object ChatModel extends HasModelPropertyCreator[ChatModel] {

  val default: ChatModel =
    ChatModel("", Seq.empty, "", 0)

}
