package pt.rmartins.battleships.frontend.views.game

import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils.BoardMarks

case class GamePuzzleState(
    puzzleSolvedCounter: Int,
    puzzleId: PuzzleId,
    boardMarks: BoardMarks,
    playerPuzzle: PlayerPuzzle,
    puzzleSolutionOpt: Option[(PuzzleSolution, Boolean)]
)
