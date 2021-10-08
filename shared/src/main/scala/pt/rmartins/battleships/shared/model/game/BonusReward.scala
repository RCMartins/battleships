package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn

sealed trait BonusReward {

  def isExtraTurn: Boolean =
    this match {
      case ExtraTurn(_) => true
      case _            => false
    }

}

object BonusReward extends HasGenCodec[BonusReward] {

  case class ExtraTurn(attackTypes: List[AttackType]) extends BonusReward

}
