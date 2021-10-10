package pt.rmartins.battleships.backend.services

import pt.rmartins.battleships.shared.model.game.{Board, Coordinate, Rotation, Ship, ShipInGame}

import scala.annotation.tailrec
import scala.util.Random

object BotHelpers {

  def placeShipsAtRandom(
      boardSize: Coordinate,
      shipsToPlace: List[Ship]
  ): Either[Unit, List[ShipInGame]] = {
    def canPlaceInBoard(
        shipsPlacedSoFar: List[ShipInGame],
        shipToPlace: Ship,
        boardCoor: Coordinate
    ): Boolean = {
      val actualPiecePositions = shipToPlace.pieces.map(_ + boardCoor)

      actualPiecePositions.forall(_.isInsideBoard(boardSize)) &&
      !actualPiecePositions
        .exists(coor => shipsPlacedSoFar.exists(_.shipActualPieces.exists(_.distance(coor) <= 1)))
    }

    val possibleCoorList: Seq[(Coordinate, Rotation)] =
      for {
        x <- 0 until boardSize.x
        y <- 0 until boardSize.y
        rotation <- Rotation.all
      } yield (Coordinate(x, y), rotation)

    @tailrec
    def loopPlaceAllShips(
        ships: List[Ship],
        shipsPlacedSoFar: List[ShipInGame]
    ): Either[Unit, List[ShipInGame]] = {
      ships match {
        case Nil =>
          Right(shipsPlacedSoFar)
        case headShip :: nextShips =>
          val result: Option[ShipInGame] =
            Random
              .shuffle(possibleCoorList)
              .map { case (coor, rotation) => (coor, headShip.rotateTo(rotation)) }
              .find { case (coor, shipWithRotation) =>
                canPlaceInBoard(shipsPlacedSoFar, shipWithRotation, coor)
              }
              .map { case (coor, shipWithRotation) => ShipInGame(shipWithRotation, coor) }

          result match {
            case None =>
              Left(())
            case Some(placedShip) =>
              loopPlaceAllShips(nextShips, placedShip :: shipsPlacedSoFar)
          }
      }
    }

    loopPlaceAllShips(shipsToPlace, Nil)

  }

}
