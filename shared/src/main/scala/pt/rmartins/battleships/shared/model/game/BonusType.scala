package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

sealed trait BonusType

object BonusType extends HasGenCodec[BonusType] {

  implicit val bonusTypeEncoder: JsonEncoder[BonusType] =
    DeriveJsonEncoder.gen[BonusType]

  implicit val bonusTypeDecoder: JsonDecoder[BonusType] =
    DeriveJsonDecoder.gen[BonusType]

  case object FirstBlood extends BonusType

  case object DoubleKill extends BonusType

  case object TripleKill extends BonusType

  case object UltraKill extends BonusType

  val AllBonusType: List[BonusType] =
    List(FirstBlood, DoubleKill, TripleKill, UltraKill)

}
