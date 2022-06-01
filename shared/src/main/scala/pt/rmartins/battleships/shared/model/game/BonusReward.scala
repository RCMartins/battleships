package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

sealed trait BonusReward {

  def isExtraTurn: Boolean =
    this match {
      case ExtraTurn(_) => true
      case _            => false
    }

}

object BonusReward extends HasGenCodec[BonusReward] {

  implicit val bonusRewardEncoder: JsonEncoder[BonusReward] =
    DeriveJsonEncoder.gen[BonusReward]

  implicit val bonusRewardDecoder: JsonDecoder[BonusReward] =
    DeriveJsonDecoder.gen[BonusReward]

  case class ExtraTurn(attackTypes: List[AttackType]) extends BonusReward

}
