package pt.rmartins.battleships.backend.services

import better.files.File
import pt.rmartins.battleships.backend.services.PuzzlesGenerator.PuzzleWithId
import pt.rmartins.battleships.shared.model.game._

import java.util.UUID

object PuzzlesUtils {

  private val puzzlesFolder: File = File("puzzles")

  def save(puzzle: Puzzle): PuzzleWithId = {
    val puzzleJsonString: String =
      Puzzle.puzzleEncoder.encodeJson(puzzle, None).toString

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
    Puzzle.puzzleDecoder
      .decodeJson(puzzleFile.contentAsString)
      .toOption
      .map(puzzle => PuzzleWithId(PuzzleId(puzzleFile.nameWithoutExtension), puzzle))

}
