package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

import scala.annotation.tailrec

case class Board(
    boardSize: Coordinate,
    ships: List[ShipInBoard]
) {

  def getSimpleShipsList: List[Ship] =
    ships.map(_.ship)

  def addShip(shipInGame: ShipInBoard): Board =
    copy(ships = shipInGame :: ships)

  def removeLastShip: Board =
    copy(
      ships = ships match {
        case Nil       => Nil
        case _ :: next => next
      }
    )

  def removeShipAt(coor: Coordinate): (Board, Option[ShipInBoard]) = {
    @tailrec
    def loop(
        shipsToCheck: List[ShipInBoard],
        remainingShipsInBoard: List[ShipInBoard]
    ): Option[(List[ShipInBoard], ShipInBoard)] = {
      shipsToCheck match {
        case Nil =>
          None
        case shipInBoard :: nextShipsToCheck if shipInBoard.contains(coor) =>
          Some((remainingShipsInBoard.reverse ++ nextShipsToCheck, shipInBoard))
        case shipInBoard :: nextShipsToCheck =>
          loop(nextShipsToCheck, shipInBoard :: remainingShipsInBoard)
      }
    }

    loop(ships, Nil) match {
      case None =>
        (this, None)
      case Some((remainingShipsInBoard, shipRemoved)) =>
        (copy(ships = remainingShipsInBoard), Some(shipRemoved))
    }
  }

  def resetBoard: Board =
    copy(ships = Nil)

}

object Board extends HasGenCodec[Board]
