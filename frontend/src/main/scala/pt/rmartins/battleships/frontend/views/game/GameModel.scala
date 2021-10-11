package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.{Attack, BoardMark, Coordinate, Ship}

case class GameModel(
    mousePosition: Option[Coordinate],
    mouseDown: Option[Int],
    selectedShip: Option[Ship],
    turnAttacks: List[Attack],
    turnAttacksSent: Boolean,
    selectedBoardMarkOpt: Option[BoardMark]
)

object GameModel extends HasModelPropertyCreator[GameModel] {

  val default: GameModel =
    GameModel(None, None, None, Nil, turnAttacksSent = false, None)

}