package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Board(boardSize: Coordinate, ships: List[ShipInGame], shipsRemaining: Int) {

  def addShip(shipInGame: ShipInGame): Board =
    copy(
      ships = shipInGame :: ships,
      shipsRemaining = shipsRemaining + 1
    )

  def removeLastShip: Board =
    copy(
      ships = ships match {
        case Nil       => Nil
        case _ :: next => next
      },
      shipsRemaining = shipsRemaining - 1
    )

  def resetBoard: Board =
    copy(
      ships = Nil,
      shipsRemaining = 0
    )

}

object Board extends HasGenCodec[Board]
