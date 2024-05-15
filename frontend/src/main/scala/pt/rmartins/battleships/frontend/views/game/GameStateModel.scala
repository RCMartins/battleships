package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.GameState

case class GameStateModel(
    gameState: Option[GameState],
    gamePuzzleState: Option[GamePuzzleState]
)

object GameStateModel extends HasModelPropertyCreator[GameStateModel] {

  val Default: GameStateModel =
    GameStateModel(
      gameState = None,
      gamePuzzleState = None
    )

}
