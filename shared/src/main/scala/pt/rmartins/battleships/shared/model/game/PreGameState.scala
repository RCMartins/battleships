package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class PreGameState(
    gameId: GameId,
    enemyUsername: Username,
    confirmRules: Boolean,
    enemyConfirmedRules: Boolean,
    rules: Rules
)

object PreGameState extends HasGenCodec[PreGameState]
