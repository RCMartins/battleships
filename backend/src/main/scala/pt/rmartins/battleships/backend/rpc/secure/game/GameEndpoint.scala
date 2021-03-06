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

  override def invitePlayer(
      otherPlayerUsername: Username,
      playerInviteType: PlayerInviteType
  ): Future[Unit] =
    gameService.invitePlayer(ctx.username, otherPlayerUsername, playerInviteType)

  override def sendPlayerInviteAnswer(
      inviterUsername: Username,
      answer: Boolean,
      playerInviteType: PlayerInviteType
  ): Future[Unit] =
    gameService.playerInviteAnswer(ctx.username, inviterUsername, answer, playerInviteType)

  override def startPreGameWithPlayer(otherPlayerUsername: Username, rules: Rules): Future[Unit] =
    gameService.startPreGameWithPlayer(ctx.username, otherPlayerUsername, rules)

  override def confirmRules(gameId: GameId): Future[Unit] =
    gameService.confirmRules(gameId, ctx.username)

  override def cancelRules(gameId: GameId): Future[Unit] =
    gameService.cancelRules(gameId, ctx.username)

  def sendRulesPatch(gameId: GameId, preGameRulesPatch: PreGameRulesPatch): Future[Unit] =
    gameService.sendRulesPatch(gameId, ctx.username, preGameRulesPatch)

  def sendPlayerRequest(gameId: GameId, playerRequestType: PlayerRequestType): Future[Unit] =
    gameService.sendPlayerRequest(gameId, ctx.username, playerRequestType)

  def sendPlayerRequestAnswer(
      gameId: GameId,
      playerRequestType: PlayerRequestType,
      answer: Boolean
  ): Future[Unit] =
    gameService.sendPlayerRequestAnswer(gameId, ctx.username, playerRequestType, answer)

  override def quitCurrentGame(gameId: GameId): Future[Unit] =
    gameService.quitCurrentGame(gameId, ctx.username)

  override def logout(): Future[Unit] =
    gameService.logout(ctx.username)

  override def getAllMessages: Future[Seq[ChatMessage]] =
    gameService.getAllMessages(ctx.username)

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

  override def addToEnemyTimeSeconds(gameId: GameId, secondsToAdd: Int): Future[Unit] =
    gameService.addToEnemyTimeSeconds(gameId, ctx.username, secondsToAdd)

  override def getRandomPuzzle(playerUsername: Username): Future[Option[(PuzzleId, PlayerPuzzle)]] =
    gameService.getRandomPuzzle(playerUsername)

  override def getPuzzleSolution(puzzleId: PuzzleId): Future[Option[PuzzleSolution]] =
    gameService.getPuzzleSolution(puzzleId)

  override def setPuzzleSolved(playerUsername: Username, puzzleId: PuzzleId): Future[Unit] =
    gameService.setPuzzleSolved(playerUsername, puzzleId)

  override def getPuzzlesSolvedCount(playerUsername: Username): Future[Int] =
    gameService.getPuzzlesSolvedCount(playerUsername)

}
