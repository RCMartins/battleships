package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Rules(
    shipsInThisGame: List[Ship]
)

object Rules extends HasGenCodec[Rules]
