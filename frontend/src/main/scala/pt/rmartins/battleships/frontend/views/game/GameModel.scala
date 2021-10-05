package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.{Attack, BoardMark, Coordinate, Ship}

case class GameModel(
    mousePosition: Option[Coordinate],
    selectedShip: Option[Ship],
    turnAttacks: List[Attack],
    turnAttacksSent: Boolean,
    selectedBoardMarkOpt: Option[BoardMark]
)

object GameModel extends HasModelPropertyCreator[GameModel]
