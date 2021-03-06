package pt.rmartins.battleships.shared.rpc.server.game

import io.udash.rpc.DefaultServerRpcCompanion
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game._

import scala.concurrent.Future

trait GameRPC {

  def sendMsg(gameId: GameId, msgText: String): Future[Unit]

  def startGameWithBots(rules: Rules): Future[Unit]

  def invitePlayer(
      otherPlayerUsername: Username,
      playerInviteType: PlayerInviteType
  ): Future[Unit]

  def sendPlayerInviteAnswer(
      inviterUsername: Username,
      answer: Boolean,
      playerInviteType: PlayerInviteType
  ): Future[Unit]

  def startPreGameWithPlayer(otherPlayerUsername: Username, rules: Rules): Future[Unit]

  def confirmRules(gameId: GameId): Future[Unit]

  def cancelRules(gameId: GameId): Future[Unit]

  def sendRulesPatch(gameId: GameId, preGameRulesPatch: PreGameRulesPatch): Future[Unit]

  def sendPlayerRequest(gameId: GameId, playerRequestType: PlayerRequestType): Future[Unit]

  def sendPlayerRequestAnswer(
      gameId: GameId,
      playerRequestType: PlayerRequestType,
      answer: Boolean
  ): Future[Unit]

  def quitCurrentGame(gameId: GameId): Future[Unit]

  def logout(): Future[Unit]

  def getAllMessages: Future[Seq[ChatMessage]]

  def confirmShips(gameId: GameId, shipPositions: List[ShipInBoard]): Future[Unit]

  def cancelShipsPlacement(gameId: GameId): Future[Unit]

  def sendTurnAttacks(gameId: GameId, currentTurn: Turn, turnAttacks: List[Attack]): Future[Unit]

  def sendBoardMarks(gameId: GameId, updatedBoardMarks: List[(Coordinate, BoardMark)]): Future[Unit]

  def addToEnemyTimeSeconds(gameId: GameId, secondsToAdd: Int): Future[Unit]

  def getRandomPuzzle(playerUsername: Username): Future[Option[(PuzzleId, PlayerPuzzle)]]

  def getPuzzleSolution(puzzleId: PuzzleId): Future[Option[PuzzleSolution]]

  def setPuzzleSolved(playerUsername: Username, puzzleId: PuzzleId): Future[Unit]

  def getPuzzlesSolvedCount(playerUsername: Username): Future[Int]

}

object GameRPC extends DefaultServerRpcCompanion[GameRPC]
