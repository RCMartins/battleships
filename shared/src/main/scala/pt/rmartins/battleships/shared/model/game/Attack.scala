package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

// TODO find a way to have Option[Coordinate] -> Coordinate
case class Attack(attackType: AttackType, coordinateOpt: Option[Coordinate]) {

  def isPlaced: Boolean = coordinateOpt.nonEmpty

}

object Attack extends HasGenCodec[Attack] {

  implicit val attackEncoder: JsonEncoder[Attack] =
    DeriveJsonEncoder.gen[Attack]

  implicit val attackDecoder: JsonDecoder[Attack] =
    DeriveJsonDecoder.gen[Attack]

}
