package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class GameBonus(
    bonusType: BonusType,
    bonusRewardList: List[BonusReward]
)

object GameBonus extends HasGenCodec[GameBonus] {

  implicit val gameBonusEncoder: JsonEncoder[GameBonus] =
    DeriveJsonEncoder.gen[GameBonus]

  implicit val gameBonusDecoder: JsonDecoder[GameBonus] =
    DeriveJsonDecoder.gen[GameBonus]

}
