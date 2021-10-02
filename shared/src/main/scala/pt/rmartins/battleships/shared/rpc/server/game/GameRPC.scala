package pt.rmartins.battleships.shared.rpc.server.game

import io.udash.rpc.DefaultServerRpcCompanion
import pt.rmartins.battleships.shared.model.game.{Coordinate, GameId, GameState, ShipInGame}

import scala.concurrent.Future

trait GameRPC {

  def startGameWith(otherPlayer: String): Future[Unit]

  def confirmShips(gameId: GameId, shipPositions: List[ShipInGame]): Future[Unit]

  def sendTurnPlay(hits: Seq[Coordinate]): Future[Boolean]

}

object GameRPC extends DefaultServerRpcCompanion[GameRPC]
