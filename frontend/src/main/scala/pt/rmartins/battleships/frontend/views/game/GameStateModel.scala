package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.GameState

case class GameStateModel(
    gameState: Option[GameState]
)

object GameStateModel extends HasModelPropertyCreator[GameStateModel]
