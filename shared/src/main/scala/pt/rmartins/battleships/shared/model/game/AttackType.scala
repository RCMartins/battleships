package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

sealed trait AttackType

object AttackType extends HasGenCodec[AttackType] {

  implicit val attackTypeEncoder: JsonEncoder[AttackType] =
    DeriveJsonEncoder.gen[AttackType]

  implicit val attackTypeDecoder: JsonDecoder[AttackType] =
    DeriveJsonDecoder.gen[AttackType]

  case object Simple extends AttackType

  case object Radar extends AttackType

  val all: List[AttackType] =
    List(Simple, Radar)

}
