package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class SimplePlayer(
    username: Username,
    isHuman: Boolean,
    boardSize: Coordinate,
    turnPlayHistory: List[TurnPlay]
)

object SimplePlayer extends HasGenCodec[SimplePlayer]
