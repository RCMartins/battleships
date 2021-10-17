package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.Username

case class PreGameModel(
    username: Username
)

object PreGameModel extends HasModelPropertyCreator[PreGameModel] {

  val default: PreGameModel = PreGameModel(Username(""))

}
