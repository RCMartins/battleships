package pt.rmartins.battleships.backend.services

import com.softwaremill.quicklens.ModifyPimp
import io.udash.rpc.ClientId
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game.HitHint.{ShipHit, Water}
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.Utils.updateVectorUsing

import java.util.UUID
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

class GameService(rpcClientsService: RpcClientsService) {

  private val activeGamesByPlayer: mutable.Map[Username, Game] = mutable.Map.empty
  private val activeGames: mutable.Map[GameId, Game] = mutable.Map.empty

  def updateServerState(game: Game): Unit = {
    activeGamesByPlayer.update(game.player1.username, game)
    activeGamesByPlayer.update(game.player2.username, game)
    activeGames.update(game.gameId, game)
  }

  def removeServerGame(game: Game): Unit = {
    activeGamesByPlayer.remove(game.player1.username)
    activeGamesByPlayer.remove(game.player2.username)
    activeGames.remove(game.gameId)
  }

  case class ServerMyBoard(
      boardSize: Coordinate,
      ships: List[ShipInGame]
  ) {

    lazy val efficientShipCheck: Vector[Vector[Option[ShipInGame]]] =
      ships
        .flatMap(shipInGame => shipInGame.shipActualPieces.map(_ -> shipInGame))
        .foldLeft(Vector.fill(boardSize.x)(Vector.fill(boardSize.y)(Option.empty[ShipInGame]))) {
          case (vectorMatrix, (shipPiece, shipInGame)) =>
            vectorMatrix.updated(
              shipPiece.x,
              vectorMatrix(shipPiece.x).updated(shipPiece.y, Some(shipInGame))
            )
        }

  }

  case class ServerEnemyBoard(
      boardSize: Coordinate,
      boardMarks: Vector[Vector[(Option[Turn], BoardMark)]],
      totalShips: Int,
      shipsLeft: Int
  ) {

    def updateMarks(
        turn: Turn,
        hits: List[(Coordinate, Option[ShipInGame])]
    ): ServerEnemyBoard = {
      val allWater = hits.forall(_._2.isEmpty)
      val allShipHit = hits.forall(_._2.nonEmpty)

      val updatedBoardMarks =
        hits.foldLeft(boardMarks) { case (marks, (coor, _)) =>
          if (allWater)
            updateVectorUsing(marks, coor, _ => (Some(turn), BoardMark.Miss))
          else if (allShipHit)
            updateVectorUsing(marks, coor, _ => (Some(turn), BoardMark.ShipHit))
          else
            updateVectorUsing(marks, coor, { case (_, boardMark) => (Some(turn), boardMark) })
        }
      copy(boardMarks = updatedBoardMarks)
    }

  }

  case class ServerPlayer(
      clientId: ClientId,
      username: Username,
      startedFirst: Boolean,
      myBoard: ServerMyBoard,
      enemyBoard: ServerEnemyBoard,
      turnPlayHistory: List[TurnPlay],
      currentTurnOpt: Option[Turn],
      currentTurnAttackTypes: List[AttackType],
      extraTurnQueue: List[ExtraTurn]
  ) {

    def kills: Int = enemyBoard.totalShips - enemyBoard.shipsLeft

    def toPlayer(game: Game): Player =
      if (game.gameStarted) {
        Player(
          clientId.id,
          username,
          Nil,
          Board(myBoard.boardSize, myBoard.ships),
          enemyBoard.boardMarks,
          turnPlayHistory
        )
      } else {
        Player(
          clientId.id,
          username,
          game.rules.shipsInThisGame,
          Board(myBoard.boardSize, myBoard.ships),
          Vector.empty,
          Nil
        )
      }

  }

  case class Game(
      gameId: GameId,
      boardSize: Coordinate,
      rules: Rules,
      player1: ServerPlayer,
      player2: ServerPlayer,
      playerWhoWonOpt: Option[Username],
      currentTurnPlayer: Option[Username]
  ) {

    val gameStarted: Boolean = currentTurnPlayer.nonEmpty

    val bothPlayers: List[ServerPlayer] = List(player1, player2)

    def getPlayerSafe(playerUsername: Username): Option[ServerPlayer] =
      if (player1.username == playerUsername) Some(player1)
      else if (player2.username == playerUsername) Some(player2)
      else None

    private def getPlayerUnsafe(playerUsername: Username): ServerPlayer =
      if (player1.username == playerUsername) player1 else player2

    def enemyPlayer(player: ServerPlayer): ServerPlayer =
      enemyPlayer(player.username)

    def enemyPlayer(playerUsername: Username): ServerPlayer =
      if (player1.username == playerUsername) player2 else player1

    def toGameStatePlayer1: GameState =
      toGameState(player1, player2)

    def toGameStatePlayer2: GameState =
      toGameState(player2, player1)

    private def toGameState(me: ServerPlayer, enemy: ServerPlayer): GameState = {
      val gameMode: GameMode =
        (me.currentTurnOpt, currentTurnPlayer, playerWhoWonOpt) match {
          case (None, _, _) =>
            PreGameMode(me.myBoard.ships.nonEmpty, enemy.myBoard.ships.nonEmpty)
          case (Some(turn), Some(currentPlayerUsername), None) =>
            InGameMode(
              currentPlayerUsername == me.username,
              turn = turn,
              turnAttackTypes = me.currentTurnAttackTypes
            )
          case (Some(turn), _, Some(playerWhoWon)) =>
            GameOverMode(
              turn,
              me.username == playerWhoWon
            )
          case _ =>
            ???
        }

      GameState(
        gameId,
        rules,
        me.toPlayer(this),
        SimplePlayer(
          enemy.clientId.id,
          enemy.username,
          enemy.enemyBoard.boardSize,
          enemy.turnPlayHistory
        ),
        gameMode
      )
    }

    def placeShips(playerUsername: Username, shipPositions: List[ShipInGame]): Game =
      updatePlayer(getPlayerUnsafe(playerUsername).modify(_.myBoard.ships).setTo(shipPositions))

    def updatePlayer(updatedPlayer: ServerPlayer): Game =
      if (updatedPlayer.username == player1.username)
        copy(player1 = updatedPlayer)
      else
        copy(player2 = updatedPlayer)

    def updatePlayerByUsername(playerUsername: Username, f: ServerPlayer => ServerPlayer): Game =
      getPlayerSafe(playerUsername)
        .map(serverPlayer => updatePlayer(f(serverPlayer)))
        .getOrElse(this)

    def getCurrentTurnPlayer: Option[ServerPlayer] =
      currentTurnPlayer.map(currentUsername =>
        if (currentUsername == player1.username) player1 else player2
      )

  }

  def reload(clientId: ClientId, playerUsername: Username): Future[Unit] = Future {
    activeGamesByPlayer.get(playerUsername).foreach { game =>
      val updatedGame =
        game.updatePlayerByUsername(playerUsername, _.copy(clientId = clientId))

      updateServerState(updatedGame)

      // TODO could this reset any state of enemy? like placed ships or placed targets???
      updateBothGameState(updatedGame)
    }
  }

  def startGame(player1Username: Username, player2Username: Username): Future[Unit] = Future {
    (
      rpcClientsService.getClientIdByUsername(player1Username),
      rpcClientsService.getClientIdByUsername(player2Username)
    ) match {
      case (Some(player1Id), Some(player2Id)) =>
        val shipsThisGame: List[Ship] =
          (
            List.fill(4)(Ship.Submarine) ++
              List.fill(3)(Ship.PatrolBoat2) ++
              List.fill(2)(Ship.PatrolBoat3) ++
              List.fill(1)(Ship.PatrolBoat4) ++
              List.fill(1)(Ship.Carrier)
          )
            .groupBy(_.shipId)
            .toList
            .sortBy { case (id, list) =>
              (-list.head.piecesSize, id)
            }
            .flatMap(_._2)

        val defaultTurnAttackTypes = List.fill(3)(AttackType.Simple)
        val turnBonuses: List[TurnBonus] =
          List(
            TurnBonus(BonusType.FirstBlood, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
            TurnBonus(BonusType.DoubleKill, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
            TurnBonus(BonusType.TripleKill, List(ExtraTurn(List.fill(3)(AttackType.Simple))))
          )

        val boardSize = Coordinate(10, 10)
        val myBoard: ServerMyBoard =
          ServerMyBoard(boardSize, Nil)
        val enemyBoard: ServerEnemyBoard =
          ServerEnemyBoard(
            boardSize,
            Vector.fill(10)(Vector.fill(10)((None, BoardMark.Empty))),
            shipsThisGame.size,
            shipsThisGame.size
          )

        val player1First: Boolean = Random.nextBoolean()

        val player1: ServerPlayer =
          ServerPlayer(
            clientId = player1Id,
            username = player1Username,
            startedFirst = player1First,
            myBoard = myBoard,
            enemyBoard = enemyBoard,
            turnPlayHistory = Nil,
            currentTurnOpt = None,
            currentTurnAttackTypes = Nil,
            extraTurnQueue = Nil
          )
        val player2: ServerPlayer =
          ServerPlayer(
            clientId = player2Id,
            username = player2Username,
            startedFirst = !player1First,
            myBoard = myBoard,
            enemyBoard = enemyBoard,
            turnPlayHistory = Nil,
            currentTurnOpt = None,
            currentTurnAttackTypes = Nil,
            extraTurnQueue = Nil
          )

        val gameId = GameId(UUID.randomUUID().toString)
        val game: Game =
          Game(
            gameId = gameId,
            boardSize = boardSize,
            rules = Rules(
              shipsInThisGame = shipsThisGame,
              defaultTurnAttackTypes = defaultTurnAttackTypes,
              turnBonuses = turnBonuses
            ),
            player1 = player1,
            player2 = player2,
            playerWhoWonOpt = None,
            currentTurnPlayer = None
          )

        updateServerState(game)
        updateBothGameState(game)
      case _ =>
        rpcClientsService.sendMessage(s"Error starting game!")
    }
  }

  def quitCurrentGame(gameId: GameId, playerUsername: Username): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some(game) =>
          removeServerGame(game)
          updateBothQuitGame(game)
      }
    }

  def restartGame(gameId: GameId): Future[Unit] =
    Future {
      activeGames.get(gameId).tap {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some(game) =>
          removeServerGame(game)
          updateBothQuitGame(game)
      }
    }.map {
      case Some(oldGame) =>
        startGame(oldGame.player1.username, oldGame.player2.username)
      case None =>
    }

  def confirmShips(
      gameId: GameId,
      playerUsername: Username,
      shipPositions: List[ShipInGame]
  ): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some(game) if !game.gameStarted =>
          val gameWithShips = game.placeShips(playerUsername, shipPositions)

          val readyToStart =
            gameWithShips.player1.myBoard.ships.nonEmpty &&
              gameWithShips.player2.myBoard.ships.nonEmpty

          val gameWithUpdatedMode =
            if (readyToStart) {
              val playerThatStartsUsername: Username =
                if (gameWithShips.player1.startedFirst)
                  gameWithShips.player1.username
                else
                  gameWithShips.player2.username

              gameWithShips
                .modifyAll(_.player1.currentTurnOpt, _.player2.currentTurnOpt)
                .setTo(Some(Turn(1, None)))
                .modifyAll(_.player1.currentTurnAttackTypes, _.player2.currentTurnAttackTypes)
                .setTo(gameWithShips.rules.defaultTurnAttackTypes)
                .modify(_.currentTurnPlayer)
                .setTo(Some(playerThatStartsUsername))
            } else
              gameWithShips

          updateServerState(gameWithUpdatedMode)
          if (readyToStart)
            updateBothGameState(gameWithUpdatedMode)
          else
            updateBothGameMode(gameWithUpdatedMode)
        case _ =>
          rpcClientsService.sendMessage("Invalid request!")
      }
    }

  def cancelShipsPlacement(gameId: GameId, playerUsername: Username): Future[Unit] =
    Future {
      activeGames.get(gameId).map(game => (game, game.getPlayerSafe(playerUsername))) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some((game, Some(player))) if !game.gameStarted && player.myBoard.ships.nonEmpty =>
          val updatedPlayer = player.modify(_.myBoard.ships).setTo(Nil)
          val updatedGame = game.updatePlayer(updatedPlayer)

          updateServerState(updatedGame)
          updateBothGameMode(updatedGame)
        case _ =>
          rpcClientsService.sendMessage("Invalid request!")
      }
    }

  def sendTurnAttacks(
      gameId: GameId,
      playerUsername: Username,
      currentTurn: Turn,
      turnAttacks: List[Attack]
  ): Future[Unit] = {
    def validateTurnAttacks(me: ServerPlayer): Boolean =
      turnAttacks.zip(me.currentTurnAttackTypes).forall {
        case (Attack(attackType, Some(Coordinate(x, y))), expectedAttackType)
            if attackType == expectedAttackType =>
          val (hitTurnOpt, boardMark) = me.enemyBoard.boardMarks(x)(y)
          hitTurnOpt.isEmpty && !boardMark.isPermanent
        case _ =>
          false
      }

    Future {
      activeGames.get(gameId).map(game => (game, game.getPlayerSafe(playerUsername))) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some((game, Some(player))) if {
              val currentPlayer = game.getCurrentTurnPlayer

              game.playerWhoWonOpt.isEmpty &&
              currentPlayer.exists(_.currentTurnOpt.exists(_ == currentTurn)) &&
              currentPlayer.exists(_.username == player.username) &&
              validateTurnAttacks(player)
            } =>
          val enemy = game.enemyPlayer(player)
          val hits: List[(Coordinate, Option[ShipInGame])] =
            turnAttacks.map(_.coordinateOpt.get).map { case coor @ Coordinate(x, y) =>
              coor -> enemy.myBoard.efficientShipCheck(x)(y)
            }

          val hitHints: List[HitHint] =
            hits
              .groupBy(_._2)
              .toList
              .flatMap {
                case (None, listOfMisses) =>
                  listOfMisses.map(_ => HitHint.Water)
                case (Some(shipInGame), listOfHits) =>
                  val hitsOnTheShip =
                    listOfHits.map(_ => HitHint.ShipHit(shipInGame.ship.shipId, destroyed = false))
                  val shipIsDestroyed =
                    shipInGame.shipActualPieces.forall { case pieceCoor @ Coordinate(x, y) =>
                      listOfHits.exists(_._1 == pieceCoor) ||
                        player.enemyBoard.boardMarks(x)(y)._1.nonEmpty
                    }
                  if (shipIsDestroyed)
                    hitsOnTheShip.head.copy(destroyed = true) :: hitsOnTheShip.tail
                  else
                    hitsOnTheShip
              }
              .sortBy {
                case Water =>
                  Int.MaxValue
                case ShipHit(shipId, destroyed) =>
                  shipId + (if (destroyed) 10000 else 0)
              }

          val turnPlay: TurnPlay =
            TurnPlay(currentTurn, turnAttacks, hitHints)

          val killsThisTurn = hitHints.count(_.isDestroyed)

          val updatedPlayer =
            player
              .modify(_.turnPlayHistory)
              .using(turnPlay :: _)
              .modify(_.enemyBoard)
              .using(_.updateMarks(currentTurn, hits))
              .modify(_.enemyBoard.shipsLeft)
              .using(_ - killsThisTurn)

          val gameOver = updatedPlayer.enemyBoard.shipsLeft == 0
          val updatedGame: Game = game.updatePlayer(updatedPlayer)
          val updatedGameWithNextTurn: Game =
            if (gameOver) {
              rpcClientsService.sendMessage(
                s"Game Over! Player '${updatedPlayer.username.username}' won!"
              )
              updatedGame.modify(_.playerWhoWonOpt).setTo(Some(updatedPlayer.username))
            } else {
              val bonusRewardList: List[BonusReward] =
                rewardExtraTurns(gameBeforeHits = game, turnPlay = turnPlay)
              val extraTurnBonuses: List[ExtraTurn] =
                bonusRewardList.collect { case extraTurn @ ExtraTurn(_) => extraTurn }

              updatedPlayer.extraTurnQueue ++ extraTurnBonuses match {
                case ExtraTurn(attackTypes) :: nextExtraTurns =>
                  val updatedPlayerWithExtraTurns =
                    updatedPlayer
                      .modify(_.currentTurnOpt)
                      .using(_.map { case Turn(currentTurn, extraTurn) =>
                        Turn(currentTurn, extraTurn.map(_ + 1).orElse(Some(1)))
                      })
                      .modify(_.currentTurnAttackTypes)
                      .setTo(attackTypes)
                      .modify(_.extraTurnQueue)
                      .setTo(nextExtraTurns)

                  updatedGame.updatePlayer(updatedPlayerWithExtraTurns)
                case Nil =>
                  val updatedPlayerWithExtraTurns =
                    updatedPlayer
                      .modify(_.currentTurnOpt)
                      .using(_.map { case Turn(currentTurn, _) => Turn(currentTurn + 1, None) })
                      .modify(_.currentTurnAttackTypes)
                      .setTo(updatedGame.rules.defaultTurnAttackTypes)

                  updatedGame
                    .updatePlayer(updatedPlayerWithExtraTurns)
                    .modify(_.currentTurnPlayer)
                    .using(_.map(updatedGame.enemyPlayer(_).username))
              }
            }

          updateServerState(updatedGameWithNextTurn)
          updateBothGameState(updatedGameWithNextTurn)
        case Some((game, _)) =>
          rpcClientsService.sendMessage(
            (
              "Invalid request!",
              game,
              playerUsername,
              currentTurn,
              turnAttacks
            ).toString
          )
      }
    }
  }

  private def rewardExtraTurns(gameBeforeHits: Game, turnPlay: TurnPlay): List[BonusReward] = {
    val killsThisTurn = turnPlay.hitHints.count(_.isDestroyed)

    gameBeforeHits.rules.turnBonuses.flatMap { case TurnBonus(bonusType, bonusReward) =>
      bonusType match {
        case BonusType.FirstBlood
            if killsThisTurn > 0 && gameBeforeHits.bothPlayers.forall(_.kills == 0) =>
          bonusReward
        case BonusType.DoubleKill if killsThisTurn == 2 =>
          bonusReward
        case BonusType.TripleKill if killsThisTurn == 3 =>
          bonusReward
        case _ =>
          Nil
      }
    }
  }

  def sendBoardMarks(
      gameId: GameId,
      playerUsername: Username,
      updatedBoardMarksList: List[(Coordinate, BoardMark)]
  ): Future[Unit] =
    Future {
      activeGames.get(gameId).map(game => (game, game.getPlayerSafe(playerUsername))) match {
        case None =>
          rpcClientsService.sendMessage(s"Error finding game!")
        case Some((game, Some(player))) if game.gameStarted =>
          val updatedBoardMarks: Vector[Vector[(Option[Turn], BoardMark)]] =
            updatedBoardMarksList.foldLeft(player.enemyBoard.boardMarks) {
              case (boardMarks, (coor, newBoardMark)) =>
                updateVectorUsing(
                  boardMarks,
                  coor,
                  {
                    case (turnNumberOpt, mark) if !mark.isPermanent =>
                      (turnNumberOpt, newBoardMark)
                  }
                )
            }

          val updatedPlayer = player.modify(_.enemyBoard.boardMarks).setTo(updatedBoardMarks)
          val updatedGame: Game = game.updatePlayer(updatedPlayer)
          updateServerState(updatedGame)
        case _ =>
          rpcClientsService.sendMessage("Invalid request!")
      }
    }

  private def updateBothGameState(game: Game): Unit = {
    rpcClientsService.sendGameState(
      game.player1.clientId,
      game.toGameStatePlayer1
    )
    rpcClientsService.sendGameState(
      game.player2.clientId,
      game.toGameStatePlayer2
    )
  }

  private def updateBothGameMode(game: Game): Unit = {
    rpcClientsService.sendGameMode(
      game.player1.clientId,
      game.toGameStatePlayer1.gameMode
    )
    rpcClientsService.sendGameMode(
      game.player2.clientId,
      game.toGameStatePlayer2.gameMode
    )
  }

  private def updateBothQuitGame(game: Game): Unit = {
    rpcClientsService.sendQuitGame(game.player1.clientId)
    rpcClientsService.sendQuitGame(game.player2.clientId)
  }

}
