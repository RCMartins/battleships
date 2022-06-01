package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

sealed trait PuzzleSolution

object PuzzleSolution extends HasGenCodec[PuzzleSolution] {

  implicit val puzzleSolutionEncoder: JsonEncoder[PuzzleSolution] =
    DeriveJsonEncoder.gen[PuzzleSolution]

  implicit val puzzleSolutionDecoder: JsonDecoder[PuzzleSolution] =
    DeriveJsonDecoder.gen[PuzzleSolution]

  case class CorrectShipBoardMarksSolution(board: Board) extends PuzzleSolution

  case class CorrectAttacksSolution(solution: List[Attack]) extends PuzzleSolution

}
