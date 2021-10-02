package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class ShipInGame(ship: Ship, position: Coordinate) {

  def shipActualPieces: List[Coordinate] = ship.pieces.map(_ + position)

}

object ShipInGame extends HasGenCodec[ShipInGame]
