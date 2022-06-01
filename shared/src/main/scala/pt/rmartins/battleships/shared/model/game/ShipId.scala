package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class ShipId(id: Int)

object ShipId extends HasGenCodec[ShipId] {

  implicit val shipIdEncoder: JsonEncoder[ShipId] =
    DeriveJsonEncoder.gen[ShipId]

  implicit val shipIdDecoder: JsonDecoder[ShipId] =
    DeriveJsonDecoder.gen[ShipId]

}
