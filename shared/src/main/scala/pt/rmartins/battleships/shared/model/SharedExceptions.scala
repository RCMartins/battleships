package pt.rmartins.battleships.shared.model

import com.avsystem.commons.serialization.HasGenCodec
import io.udash.rpc.serialization.DefaultExceptionCodecRegistry

/** Exception registry providing codecs for custom exceptions. It enables the RPC to transport
  * exceptions from the server to the client as a request result.
  */
class SharedExceptions extends DefaultExceptionCodecRegistry {
  import pt.rmartins.battleships.shared.model.SharedExceptions._

  register(UnauthorizedException.codec)
  register(UsernameInvalid.codec)
  register(UserAlreadyExists.codec)
}

object SharedExceptions {

  case class UnauthorizedException() extends RuntimeException("Provided token is invalid.")
  object UnauthorizedException extends HasGenCodec[UnauthorizedException]

  case class UsernameInvalid() extends RuntimeException("Provided username is invalid.")
  object UsernameInvalid extends HasGenCodec[UsernameInvalid]

  case class UserAlreadyExists() extends RuntimeException("Provided username is already being used")
  object UserAlreadyExists extends HasGenCodec[UserAlreadyExists]

}
