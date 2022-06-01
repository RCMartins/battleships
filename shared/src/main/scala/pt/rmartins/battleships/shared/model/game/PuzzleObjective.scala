package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

sealed trait PuzzleObjective

object PuzzleObjective extends HasGenCodec[PuzzleObjective] {

  implicit val puzzleObjectiveEncoder: JsonEncoder[PuzzleObjective] =
    DeriveJsonEncoder.gen[PuzzleObjective]

  implicit val puzzleObjectiveDecoder: JsonDecoder[PuzzleObjective] =
    DeriveJsonDecoder.gen[PuzzleObjective]

  case object CorrectShipBoardMarks extends PuzzleObjective

  case class WinInXTurns(maximumTurns: Int) extends PuzzleObjective

}
