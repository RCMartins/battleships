package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class ShipInBoard(ship: Ship, position: Coordinate) {

  lazy val shipActualPieces: List[Coordinate] = ship.pieces.map(_ + position)

  def contains(coor: Coordinate): Boolean =
    shipActualPieces.contains(coor)

}

object ShipInBoard extends HasGenCodec[ShipInBoard]
