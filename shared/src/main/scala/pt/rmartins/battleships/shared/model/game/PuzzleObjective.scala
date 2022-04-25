package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait PuzzleObjective

object PuzzleObjective extends HasGenCodec[PuzzleObjective] {

  case object CorrectShipBoardMarks extends PuzzleObjective

  case class WinInXTurns(maximumTurns: Int) extends PuzzleObjective

}
