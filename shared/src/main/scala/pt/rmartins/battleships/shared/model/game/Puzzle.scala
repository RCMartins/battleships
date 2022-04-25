package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Puzzle(
    puzzleVersion: Double,
    playerPuzzle: PlayerPuzzle,
    puzzleSolution: PuzzleSolution
)

object Puzzle extends HasGenCodec[Puzzle]
