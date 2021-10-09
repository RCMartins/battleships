package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Rules(
    shipsInThisGame: List[Ship],
    defaultTurnAttackTypes: List[AttackType],
    turnBonuses: List[TurnBonus],
    timeLimit: Option[RuleTimeLimit]
)

object Rules extends HasGenCodec[Rules]
