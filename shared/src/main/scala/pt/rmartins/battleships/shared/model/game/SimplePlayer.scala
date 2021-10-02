package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class SimplePlayer(clientId: String, name: String)

object SimplePlayer extends HasGenCodec[SimplePlayer]
