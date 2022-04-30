package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait UserError

object UserError extends HasGenCodec[UserError] {

  case object InviteItself extends UserError

  case class UsernameNotFound(username: Username) extends UserError

}
