package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Board(
    boardSize: Coordinate,
    ships: List[ShipInBoard]
) {

  def addShip(shipInGame: ShipInBoard): Board =
    copy(ships = shipInGame :: ships)

  def removeLastShip: Board =
    copy(
      ships = ships match {
        case Nil       => Nil
        case _ :: next => next
      }
    )

  def resetBoard: Board =
    copy(ships = Nil)

}

object Board extends HasGenCodec[Board]
