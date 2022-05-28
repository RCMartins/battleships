package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait PlayerInviteType

object PlayerInviteType extends HasGenCodec[PlayerInviteType] {

  case object Play extends PlayerInviteType

  case object Rematch extends PlayerInviteType

}
