package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class ShipId(id: Int)

object ShipId extends HasGenCodec[ShipId]
