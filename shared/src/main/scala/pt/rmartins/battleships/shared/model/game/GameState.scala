package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class GameState(
    gameId: GameId,
    me: Player,
    enemy: SimplePlayer,
    gameMode: GameMode
)

object GameState extends HasGenCodec[GameState]
