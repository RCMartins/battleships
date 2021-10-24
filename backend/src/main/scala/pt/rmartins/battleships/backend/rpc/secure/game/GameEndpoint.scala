package pt.rmartins.battleships.backend.rpc.secure.game

import io.udash.auth._
import io.udash.rpc.ClientId
import pt.rmartins.battleships.backend.services.{GameService, RpcClientsService}
import pt.rmartins.battleships.shared.model.auth.UserContext
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC

import scala.concurrent.Future

class GameEndpoint(implicit
    gameService: GameService,
    rpcClientsService: RpcClientsService,
    ctx: UserContext
) extends GameRPC
    with AuthRequires {

  override def sendMsg(gameId: GameId, msgText: String): Future[Unit] =
    gameService.sendMsg(gameId, ctx.username, msgText)

  override def startGameWithBots(rules: Rules): Future[Unit] =
    gameService.startGameWithBots(ctx.username, rules)

  override def startGameWith(otherPlayerUsername: Username, rules: Rules): Future[Unit] =
    gameService.startGame(ctx.username, otherPlayerUsername, rules)

  override def quitCurrentGame(gameId: GameId): Future[Unit] =
    gameService.quitCurrentGame(gameId, ctx.username)

  override def logout(): Future[Unit] =
    gameService.logout(ctx.username)

  override def getAllMessages: Future[Seq[ChatMessage]] =
    gameService.getAllMessages(ctx.username)

  override def rematchGame(gameId: GameId): Future[Unit] =
    gameService.rematchGame(gameId, ctx.username)

  override def confirmShips(
      gameId: GameId,
      shipPositions: List[ShipInBoard]
  ): Future[Unit] =
    gameService.confirmShips(gameId, ctx.username, shipPositions)

  override def cancelShipsPlacement(gameId: GameId): Future[Unit] =
    gameService.cancelShipsPlacement(gameId, ctx.username)

  override def sendTurnAttacks(
      gameId: GameId,
      currentTurn: Turn,
      turnAttacks: List[Attack]
  ): Future[Unit] =
    gameService.sendTurnAttacks(
      gameId,
      ctx.username,
      currentTurn,
      turnAttacks
    )

  override def sendBoardMarks(
      gameId: GameId,
      updatedBoardMarksList: List[(Coordinate, BoardMark)]
  ): Future[Unit] =
    gameService.sendBoardMarks(gameId, ctx.username, updatedBoardMarksList)

}
