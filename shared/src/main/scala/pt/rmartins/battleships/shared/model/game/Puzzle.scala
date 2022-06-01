package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Puzzle(
    puzzleVersion: Double,
    playerPuzzle: PlayerPuzzle,
    puzzleSolution: PuzzleSolution
)

object Puzzle extends HasGenCodec[Puzzle] {

  implicit val puzzleEncoder: JsonEncoder[Puzzle] =
    DeriveJsonEncoder.gen[Puzzle]

  implicit val puzzleDecoder: JsonDecoder[Puzzle] =
    DeriveJsonDecoder.gen[Puzzle]

}
