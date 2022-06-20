package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Puzzle(
    puzzleVersion: Double,
    playerPuzzle: PlayerPuzzle,
    puzzleSolution: PuzzleSolution
) {

  val value: Int =
    playerPuzzle.puzzleObjective match {
      case PuzzleObjective.CorrectShipBoardMarks =>
        val boardPoints: Int =
          Math.max(0, (playerPuzzle.boardSize.x - 6) * 2)

        val fleetPoints: Int = {
          playerPuzzle.gameFleet.shipsList.map { ship =>
            Math.max(1, ship.piecesSize / 2) +
              Ship.allShipsUniqueRotations(ship.shipId).size
          }.sum
        }

        val turnPlayHistoryMultiplier: Double = {
          val hitHints = playerPuzzle.turnPlayHistory.flatMap(_.hitHints)
          val maxHitCount = playerPuzzle.gameFleet.shipsList.map(_.piecesSize).sum
          val hitCount = hitHints.count(_.isShip)
          val waterCount = hitHints.count(_.isWater)

          if (playerPuzzle.gameFleet.shipsList.sizeIs > hitCount)
            2.0 - Math.min(10, waterCount) * 0.1
          else
            1.0 - hitCount / (maxHitCount * 2.0) - Math.min(10, waterCount) * 0.05
        }

        ((boardPoints + fleetPoints) * turnPlayHistoryMultiplier).toInt
      case PuzzleObjective.WinInXTurns(_) =>
        0
    }

}

object Puzzle extends HasGenCodec[Puzzle] {

  implicit val puzzleEncoder: JsonEncoder[Puzzle] =
    DeriveJsonEncoder.gen[Puzzle]

  implicit val puzzleDecoder: JsonDecoder[Puzzle] =
    DeriveJsonDecoder.gen[Puzzle]

}
