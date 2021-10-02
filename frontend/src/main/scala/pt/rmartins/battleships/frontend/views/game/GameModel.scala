package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.{Coordinate, GameState, Rotation, Ship}

case class GameModel(
    myGameState: Option[GameState],
    mousePosition: Option[Coordinate],
    selectedShip: Option[Ship]
)

object GameModel extends HasModelPropertyCreator[GameModel]
