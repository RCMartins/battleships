package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait PlayerRequestType

object PlayerRequestType extends HasGenCodec[PlayerRequestType] {

  case object EditRules extends PlayerRequestType

}
