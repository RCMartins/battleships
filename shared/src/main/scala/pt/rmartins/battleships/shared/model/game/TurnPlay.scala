package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class TurnPlay(
    turnNumber: Int,
    turnAttacks: List[Attack],
    hitHints: List[HitHint]
)

object TurnPlay extends HasGenCodec[TurnPlay]
