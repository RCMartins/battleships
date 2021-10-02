package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class GameId(id: String)

object GameId extends HasGenCodec[GameId]
