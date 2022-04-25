package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.utils.BoardUtils.BoardMarks

case class PlayerPuzzle(
    boardSize: Coordinate,
    gameFleet: Fleet,
    initialBoardMarks: BoardMarks,
    turnPlayHistory: List[TurnPlay],
    puzzleObjective: PuzzleObjective
)

object PlayerPuzzle extends HasGenCodec[PlayerPuzzle]
