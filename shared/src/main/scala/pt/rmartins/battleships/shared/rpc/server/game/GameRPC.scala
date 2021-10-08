package pt.rmartins.battleships.shared.rpc.server.game

import io.udash.rpc.DefaultServerRpcCompanion
import pt.rmartins.battleships.shared.model.game._

import scala.concurrent.Future

trait GameRPC {

  def startGameWith(otherPlayerUsername: Username): Future[Unit]

  def quitCurrentGame(gameId: GameId): Future[Unit]

  def restartGame(gameId: GameId): Future[Unit]

  def confirmShips(gameId: GameId, shipPositions: List[ShipInGame]): Future[Unit]

  def cancelShipsPlacement(gameId: GameId): Future[Unit]

  def sendTurnAttacks(gameId: GameId, currentTurn: Turn, turnAttacks: List[Attack]): Future[Unit]

  def sendBoardMarks(gameId: GameId, updatedBoardMarks: List[(Coordinate, BoardMark)]): Future[Unit]

}

object GameRPC extends DefaultServerRpcCompanion[GameRPC]
