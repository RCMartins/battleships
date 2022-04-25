package pt.rmartins.battleships.backend.services

import better.files.File
import pt.rmartins.battleships.backend.services.PuzzlesGenerator.PuzzleWithId
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils.BoardMarks
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

import java.util.UUID

object PuzzlesUtils {

  private val puzzlesFolder: File = File("puzzles")

  private implicit val coordinateEncoder: JsonEncoder[Coordinate] =
    DeriveJsonEncoder.gen[Coordinate]
  private implicit val shipIdEncoder: JsonEncoder[ShipId] =
    DeriveJsonEncoder.gen[ShipId]
  private implicit val rotationEncoder: JsonEncoder[Rotation] =
    JsonEncoder[Int].contramap(_.rIndex)
  private implicit val shipEncoder: JsonEncoder[Ship] =
    DeriveJsonEncoder.gen[Ship]
  private implicit val fleetEncoder: JsonEncoder[Fleet] =
    DeriveJsonEncoder.gen[Fleet]
  private implicit val shipInBoardEncoder: JsonEncoder[ShipInBoard] =
    DeriveJsonEncoder.gen[ShipInBoard]
  private implicit val boardEncoder: JsonEncoder[Board] =
    DeriveJsonEncoder.gen[Board]
  private implicit val boardMarkEncoder: JsonEncoder[BoardMark] =
    DeriveJsonEncoder.gen[BoardMark]
  private implicit val turnEncoder: JsonEncoder[Turn] =
    DeriveJsonEncoder.gen[Turn]
  private implicit val attackTypeEncoder: JsonEncoder[AttackType] =
    DeriveJsonEncoder.gen[AttackType]
  private implicit val attackEncoder: JsonEncoder[Attack] =
    DeriveJsonEncoder.gen[Attack]
  private implicit val hitHintEncoder: JsonEncoder[HitHint] =
    DeriveJsonEncoder.gen[HitHint]
  private implicit val turnPlayEncoder: JsonEncoder[TurnPlay] =
    DeriveJsonEncoder.gen[TurnPlay]
  private implicit val puzzleObjectiveEncoder: JsonEncoder[PuzzleObjective] =
    DeriveJsonEncoder.gen[PuzzleObjective]
  private implicit val playerPuzzleEncoder: JsonEncoder[PlayerPuzzle] =
    DeriveJsonEncoder.gen[PlayerPuzzle]
  private implicit val puzzleSolutionEncoder: JsonEncoder[PuzzleSolution] =
    DeriveJsonEncoder.gen[PuzzleSolution]
  private implicit val puzzleEncoder: JsonEncoder[Puzzle] =
    DeriveJsonEncoder.gen[Puzzle]

  private implicit val coordinateDecoder: JsonDecoder[Coordinate] =
    DeriveJsonDecoder.gen[Coordinate]
  private implicit val shipIdDecoder: JsonDecoder[ShipId] =
    DeriveJsonDecoder.gen[ShipId]
  private implicit val rotationDecoder: JsonDecoder[Rotation] =
    JsonDecoder[Int].map(Rotation.getUnsafe)
  private implicit val shipDecoder: JsonDecoder[Ship] =
    DeriveJsonDecoder.gen[Ship]
  private implicit val fleetDecoder: JsonDecoder[Fleet] =
    DeriveJsonDecoder.gen[Fleet]
  private implicit val shipInBoardDecoder: JsonDecoder[ShipInBoard] =
    DeriveJsonDecoder.gen[ShipInBoard]
  private implicit val boardDecoder: JsonDecoder[Board] =
    DeriveJsonDecoder.gen[Board]
  private implicit val boardMarkDecoder: JsonDecoder[BoardMark] =
    DeriveJsonDecoder.gen[BoardMark]
  private implicit val turnDecoder: JsonDecoder[Turn] =
    DeriveJsonDecoder.gen[Turn]
  private implicit val attackTypeDecoder: JsonDecoder[AttackType] =
    DeriveJsonDecoder.gen[AttackType]
  private implicit val attackDecoder: JsonDecoder[Attack] =
    DeriveJsonDecoder.gen[Attack]
  private implicit val hitHintDecoder: JsonDecoder[HitHint] =
    DeriveJsonDecoder.gen[HitHint]
  private implicit val turnPlayDecoder: JsonDecoder[TurnPlay] =
    DeriveJsonDecoder.gen[TurnPlay]
  private implicit val puzzleObjectiveDecoder: JsonDecoder[PuzzleObjective] =
    DeriveJsonDecoder.gen[PuzzleObjective]
  private implicit val playerPuzzleDecoder: JsonDecoder[PlayerPuzzle] =
    DeriveJsonDecoder.gen[PlayerPuzzle]
  private implicit val puzzleSolutionDecoder: JsonDecoder[PuzzleSolution] =
    DeriveJsonDecoder.gen[PuzzleSolution]
  private implicit val puzzleDecoder: JsonDecoder[Puzzle] =
    DeriveJsonDecoder.gen[Puzzle]

  def save(puzzle: Puzzle): PuzzleWithId = {
    val puzzleJsonString: String =
      puzzleEncoder.encodeJson(puzzle, None).toString

    val puzzleId = UUID.randomUUID().toString
    if (!puzzlesFolder.exists)
      puzzlesFolder.createDirectories()
    val puzzleFile = puzzlesFolder / s"$puzzleId.json"
    puzzleFile.write(puzzleJsonString)
    PuzzleWithId(PuzzleId(puzzleId), puzzle)
  }

  def loadAllPuzzles(): List[PuzzleWithId] =
    if (puzzlesFolder.exists)
      puzzlesFolder.children.flatMap(load).toList
    else
      Nil

  def load(puzzleFile: File): Option[PuzzleWithId] =
    puzzleDecoder
      .decodeJson(puzzleFile.contentAsString)
      .toOption
      .map(puzzle => PuzzleWithId(PuzzleId(puzzleFile.nameWithoutExtension), puzzle))

}
