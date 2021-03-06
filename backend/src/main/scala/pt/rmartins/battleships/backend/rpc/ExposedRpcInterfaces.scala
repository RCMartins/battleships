package pt.rmartins.battleships.backend.rpc

import io.udash.i18n.RemoteTranslationRPC
import io.udash.rpc._
import pt.rmartins.battleships.backend.rpc.auth.AuthEndpoint
import pt.rmartins.battleships.backend.rpc.i18n.TranslationsEndpoint
import pt.rmartins.battleships.backend.rpc.secure.SecureEndpoint
import pt.rmartins.battleships.backend.services.DomainServices
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
import pt.rmartins.battleships.shared.rpc.server.MainServerRPC
import pt.rmartins.battleships.shared.rpc.server.open.AuthRPC
import pt.rmartins.battleships.shared.rpc.server.secure.SecureRPC

class ExposedRpcInterfaces(implicit domainServices: DomainServices, clientId: ClientId)
    extends MainServerRPC {
  // required domain services are implicitly passed to the endpoints
  import domainServices._

  private lazy val authEndpoint: AuthRPC = new AuthEndpoint

  // it caches SecureEndpoint for a single UserToken (UserToken change is not an expected behaviour)
  private var secureEndpointCache: Option[(UserToken, SecureEndpoint)] = None

  private def secureEndpoint(implicit ctx: UserContext): SecureRPC = {
    secureEndpointCache match {
      case Some((token, endpoint)) if token == ctx.token =>
        endpoint
      case _ =>
        val endpoint = new SecureEndpoint
        secureEndpointCache = Some((ctx.token, endpoint))
        endpoint
    }
  }

  override def auth(): AuthRPC = authEndpoint

  override def secure(token: UserToken): SecureRPC = {
    authService
      .findUserCtx(token)
      .map(ctx => secureEndpoint(ctx))
      .getOrElse(throw SharedExceptions.UnauthorizedException())
  }

  override def translations(): RemoteTranslationRPC = TranslationsEndpoint
}
