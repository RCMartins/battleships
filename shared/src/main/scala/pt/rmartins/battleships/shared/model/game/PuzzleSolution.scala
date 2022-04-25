package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait PuzzleSolution

object PuzzleSolution extends HasGenCodec[PuzzleSolution] {

  case class CorrectShipBoardMarksSolution(board: Board) extends PuzzleSolution

  case class CorrectAttacksSolution(solution: List[Attack]) extends PuzzleSolution

}
