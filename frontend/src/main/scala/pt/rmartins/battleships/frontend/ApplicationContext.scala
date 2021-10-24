package pt.rmartins.battleships.frontend

import pt.rmartins.battleships.frontend.routing._
import pt.rmartins.battleships.frontend.services.rpc.{NotificationsCenter, RPCService}
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.rpc.client.MainClientRPC
import pt.rmartins.battleships.shared.rpc.server.MainServerRPC
import io.udash._
import io.udash.rpc._

object ApplicationContext {
  import scala.concurrent.ExecutionContext.Implicits.global

  private val routingRegistry = new RoutingRegistryDef
  private val viewFactoryRegistry = new StatesToViewFactoryDef

  val application = new Application[RoutingState](
    routingRegistry,
    viewFactoryRegistry,
    new WindowUrlPathChangeProvider
  )

  application.onRoutingFailure {
    case _: SharedExceptions.UnauthorizedException =>
      // automatic redirection to LoginPage
      application.goTo(RoutingLoginPageState)
    case ex =>
      println(ex)
  }

  val notificationsCenter: NotificationsCenter = new NotificationsCenter

  // creates RPC connection to the server
  val serverRpc: MainServerRPC = DefaultServerRPC[MainClientRPC, MainServerRPC](
    new RPCService(notificationsCenter),
    exceptionsRegistry = new SharedExceptions
  )

  val translationsService: TranslationsService = new TranslationsService(serverRpc.translations())
  val userService: UserContextService = new UserContextService(serverRpc)
}
