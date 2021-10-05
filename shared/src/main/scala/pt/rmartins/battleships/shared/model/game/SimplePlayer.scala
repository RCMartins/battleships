package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class SimplePlayer(
    clientId: String,
    username: String,
    boardSize: Coordinate,
    turnPlayHistory: List[TurnPlay]
)

object SimplePlayer extends HasGenCodec[SimplePlayer]
