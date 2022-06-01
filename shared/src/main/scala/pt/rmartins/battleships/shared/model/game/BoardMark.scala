package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

sealed trait BoardMark {

  def isPermanent: Boolean = false

  def isWater: Boolean =
    this == BoardMark.Water || this == BoardMark.ManualWater

  def isShip: Boolean =
    this == BoardMark.ShipHit || this == BoardMark.ManualShip

}

object BoardMark extends HasGenCodec[BoardMark] {

  implicit val boardMarkEncoder: JsonEncoder[BoardMark] =
    DeriveJsonEncoder.gen[BoardMark]

  implicit val boardMarkDecoder: JsonDecoder[BoardMark] =
    DeriveJsonDecoder.gen[BoardMark]

  case object Empty extends BoardMark

  case object ManualWater extends BoardMark

  case object ManualShip extends BoardMark

  case object Water extends BoardMark {
    override def isPermanent: Boolean = true
  }

  case object ShipHit extends BoardMark {
    override def isPermanent: Boolean = true
  }

}
