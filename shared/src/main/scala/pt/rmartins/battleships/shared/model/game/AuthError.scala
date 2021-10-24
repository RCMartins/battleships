package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait AuthError {
  def message: String
}

object AuthError extends HasGenCodec[AuthError] {

  case object UnauthorizedException extends AuthError {
    val message: String = "Provided token is invalid."
  }

  case object UsernameInvalid extends AuthError {
    val message: String = "Provided username is invalid."
  }

  case object UserAlreadyExists extends AuthError {
    val message: String = "Provided username is already being used."
  }

}
