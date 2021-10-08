package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Rules(
    shipsInThisGame: List[Ship],
    defaultTurnAttackTypes: List[AttackType],
    turnBonuses: List[TurnBonus]
)

object Rules extends HasGenCodec[Rules]
