package pt.rmartins.battleships.backend.services

/** Container for all services used to implicitly pass services to endpoints. */
class DomainServices(implicit
    val authService: AuthService,
    val gameService: GameService,
    val chatService: ChatService,
    val rpcClientsService: RpcClientsService
)
