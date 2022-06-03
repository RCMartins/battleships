package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait AuthError

object AuthError extends HasGenCodec[AuthError] {

  case object UnauthorizedException extends AuthError {
    override val toString: String = "Provided token is invalid."
  }

  case object UsernameInvalid extends AuthError {
    override val toString: String = "Provided username is invalid."
  }

  case object UsernameTooLong extends AuthError {
    override val toString: String = s"Provided username is too long."
  }

  case object UserAlreadyExists extends AuthError {
    override val toString: String = "Provided username is already being used."
  }

}
