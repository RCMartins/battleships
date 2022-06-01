package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class ShipInBoard(ship: Ship, position: Coordinate) {

  lazy val shipActualPieces: List[Coordinate] = ship.pieces.map(_ + position)

  def contains(coor: Coordinate): Boolean =
    shipActualPieces.contains(coor)

}

object ShipInBoard extends HasGenCodec[ShipInBoard] {

  implicit val shipInBoardEncoder: JsonEncoder[ShipInBoard] =
    DeriveJsonEncoder.gen[ShipInBoard]

  implicit val shipInBoardDecoder: JsonDecoder[ShipInBoard] =
    DeriveJsonDecoder.gen[ShipInBoard]

}
