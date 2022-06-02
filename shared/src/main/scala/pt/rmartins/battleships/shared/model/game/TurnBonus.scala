package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class TurnBonus(
    bonusType: BonusType,
    bonusReward: List[BonusReward]
)

object TurnBonus extends HasGenCodec[TurnBonus] {

  implicit val turnBonusEncoder: JsonEncoder[TurnBonus] =
    DeriveJsonEncoder.gen[TurnBonus]

  implicit val turnBonusDecoder: JsonDecoder[TurnBonus] =
    DeriveJsonDecoder.gen[TurnBonus]

}
