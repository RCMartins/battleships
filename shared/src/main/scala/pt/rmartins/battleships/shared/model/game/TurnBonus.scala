package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class TurnBonus(
    bonusType: BonusType,
    bonusReward: List[BonusReward]
)

object TurnBonus extends HasGenCodec[TurnBonus]
