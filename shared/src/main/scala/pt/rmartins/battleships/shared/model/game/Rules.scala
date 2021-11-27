package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Rules(
    boardSize: Coordinate,
    gameFleet: Fleet,
    defaultTurnAttacks: List[AttackType],
    turnBonuses: List[TurnBonus],
    timeLimit: RuleTimeLimit
)

object Rules extends HasGenCodec[Rules]
