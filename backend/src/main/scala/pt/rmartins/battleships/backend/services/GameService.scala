package pt.rmartins.battleships.backend.services

import com.softwaremill.quicklens.ModifyPimp
import io.udash.rpc.ClientId
import pt.rmartins.battleships.backend.services.GameService.{BotClientId, BotUsername}
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, PlayingMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game.HitHint.{ShipHit, Water}
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils
import pt.rmartins.battleships.shared.model.utils.BoardUtils._

import java.time.{Duration, Instant}
import java.util.{Date, UUID}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

class GameService(rpcClientsService: RpcClientsService) {

  private val activeGamesByPlayer: mutable.Map[Username, Game] = mutable.Map.empty
  private val activeGames: mutable.Map[GameId, Game] = mutable.Map.empty

  @inline
  private def updateActiveGamesByPlayer(player: ServerPlayer, game: Game): Unit = {
    if (player.isHuman)
      activeGamesByPlayer.update(player.username, game)
  }

  private def unsafeUpdateServerState(game: Game): Game = {
    updateActiveGamesByPlayer(game.player1, game)
    updateActiveGamesByPlayer(game.player2, game)
    activeGames.update(game.gameId, game)
    game
  }

  val clock: Runnable = () => {
    val cycleTimeMillis: Long = 200L
    while (true) {
      clock.synchronized {
        val initialInstant = Instant.now()
        activeGames.values.toList.filter(game => game.gameIsActive).foreach { game =>
          val updatedGame = updateGameTime(game, initialInstant)
          unsafeUpdateServerState(updatedGame)

          if (updatedGame.gameIsOver)
            updateBothGameState(updatedGame)
        }
      }
      Thread.sleep(cycleTimeMillis)
    }
  }

  new Thread(clock).start()

  val botUpdater: Runnable = () => {
    val cycleTimeMillis: Long = 500L
    while (true) {
      clock.synchronized {
        val gamesList = activeGames.values.toList

        gamesList
          .filter(game =>
            game.isInPlacingShipsMode && (!game.player1.isHuman || !game.player2.isHuman)
          )
          .foreach { game =>
            def placePlayerShips(player: ServerPlayer): Unit =
              if (!player.isHuman && player.myBoard.ships.isEmpty) {
                // TODO try some times
                //  the fleet should be validated before starting game...
                //  Create Fleet class to hold ship lists and to validate fleets ?????
                //  Also needs board size + any specific data
                BotHelper.placeShipsAtRandom(
                  player.myBoard.boardSize,
                  game.rules.gameFleet.ships
                ) match {
                  case Left(_) =>
                    // Try placing ships in the next botUpdater loop ...
                    println("Failed placing ships... Waiting for next loop...")
                  case Right(placedShips) =>
                    confirmShips(game.gameId, player.username, placedShips)
                }
              }
            placePlayerShips(game.player1)
            placePlayerShips(game.player2)
          }
        gamesList
          .flatMap(game => game.getCurrentTurnPlayer.map((game, _)))
          .filter(!_._2.isHuman)
          .map { case (game, bot) => bot.botHelper.map((game, bot, _)) }
          .foreach {
            case Some((game, bot, botHelper)) =>
              // TODO check if we are 'out' of time for this loop

              bot.currentTurnOpt.foreach { currentTurn =>
                bot.turnPlayHistory.headOption
                  .foreach(lastTurn => botHelper.updateBotBoardMarks(lastTurn))
                val turnAttacks: List[Attack] =
                  botHelper.placeAttacks(bot.currentTurnAttackTypes)
                unsafeSendTurnAttacks(game, bot, currentTurn, turnAttacks)
              }
            case _ =>
          }
      }
      Thread.sleep(cycleTimeMillis)
    }
  }

  new Thread(botUpdater).start()

  def updateGameTime(game: Game, instant: Instant): Game = {
    val updatedGame =
      game.getCurrentTurnPlayer.flatMap(serverPlayer =>
        serverPlayer.timeRemaining.map((serverPlayer, _))
      ) match {
        case Some(
              (
                serverPlayer,
                ServerTimeRemaining(totalTimeRemainingNanos, turnTimeRemainingNanosOpt)
              )
            ) =>
          val betweenNanos: Long =
            game.lastUpdateTimeOpt.map(Duration.between(_, instant).toNanos).getOrElse(0L)

          val updatedServerTimeRemaining: ServerTimeRemaining =
            turnTimeRemainingNanosOpt match {
              case None =>
                val updatedTime: Long = Math.max(0L, totalTimeRemainingNanos - betweenNanos)
                ServerTimeRemaining(updatedTime, turnTimeRemainingNanosOpt)
              case Some(turnTimeRemainingNanos) =>
                if (turnTimeRemainingNanos >= betweenNanos)
                  ServerTimeRemaining(
                    totalTimeRemainingNanos,
                    Some(turnTimeRemainingNanos - betweenNanos)
                  )
                else {
                  val updatedTime: Long =
                    Math.max(0L, totalTimeRemainingNanos + turnTimeRemainingNanos - betweenNanos)
                  ServerTimeRemaining(updatedTime, Some(0))
                }
            }

          val updatedPlayer: ServerPlayer =
            serverPlayer.copy(timeRemaining = Some(updatedServerTimeRemaining))
          val updatedGame: Game =
            game.updatePlayer(updatedPlayer)

          if (updatedServerTimeRemaining.totalTimeRemainingNanos == 0)
            setGameOver(
              updatedGame,
              updatedGame.enemyPlayer(updatedPlayer.username).username
            )
          else
            updatedGame
        case _ =>
          game
      }

    updatedGame.copy(lastUpdateTimeOpt = Some(instant))
  }

  def setGameOver(game: Game, whoWonUsername: Username): Game = {
    sendSystemMessage(game, s"Game Over! Player '$whoWonUsername' won!")
    game.modify(_.playerWhoWonOpt).setTo(Some(whoWonUsername))
  }

  def updateServerState(game: Game, instantNow: Option[Instant] = Some(Instant.now())): Game =
    clock.synchronized {
      val updatedGame = instantNow.map(updateGameTime(game, _)).getOrElse(game)
      unsafeUpdateServerState(updatedGame)
    }

  def removeServerGame(game: Game): Unit =
    clock.synchronized {
      activeGamesByPlayer.remove(game.player1.username)
      activeGamesByPlayer.remove(game.player2.username)
      activeGames.remove(game.gameId)
    }

  case class ServerMyBoard(
      boardSize: Coordinate,
      ships: List[ShipInBoard]
  ) {

    lazy val efficientShipCheck: Vector[Vector[Option[ShipInBoard]]] =
      ships
        .flatMap(shipInGame => shipInGame.shipActualPieces.map(_ -> shipInGame))
        .foldLeft(Vector.fill(boardSize.x)(Vector.fill(boardSize.y)(Option.empty[ShipInBoard]))) {
          case (vectorMatrix, (shipPiece, shipInGame)) =>
            vectorMatrix.updated(
              shipPiece.x,
              vectorMatrix(shipPiece.x).updated(shipPiece.y, Some(shipInGame))
            )
        }

  }

  case class ServerEnemyBoard(
      boardSize: Coordinate,
      boardMarks: BoardMarks,
      totalShips: Int,
      shipsLeft: Int
  ) {

    def updateMarks(
        turn: Turn,
        hits: List[(Coordinate, Option[ShipInBoard])]
    ): ServerEnemyBoard = {
      val allWater = hits.forall(_._2.isEmpty)
      val allShipHit = hits.forall(_._2.nonEmpty)
      val allSubmarineHits =
        allShipHit && hits.forall(_._2.exists(_.ship.shipId == Ship.Submarine.shipId))

      val updatedBoardMarks: BoardMarks =
        hits.foldLeft(boardMarks) { case (marks, (coor, _)) =>
          if (allWater)
            updateBoardMarksUsing(marks, coor, _ => (Some(turn), BoardMark.Water))
          else if (allShipHit)
            updateBoardMarksUsing(marks, coor, _ => (Some(turn), BoardMark.ShipHit))
          else
            updateBoardMarksUsing(marks, coor, { case (_, boardMark) => (Some(turn), boardMark) })
        }

      val updatedBoardMarks2: BoardMarks =
        if (allSubmarineHits)
          hits
            .flatMap(_._1.get8CoorAround)
            .filter(_.isInsideBoard(boardSize))
            .foldLeft(updatedBoardMarks) { case (marks, coor) =>
              updateBoardMarksUsing(
                marks,
                coor,
                { case (turnOpt, _) => (turnOpt, BoardMark.Water) }
              )
            }
        else
          updatedBoardMarks

      copy(boardMarks = updatedBoardMarks2)
    }

    lazy val hasNFreeSpaces: Int =
      boardMarks.map {
        _.count { case (opt, boardMark) => opt.isEmpty && !boardMark.isPermanent }
      }.sum

  }

  case class ServerTimeRemaining(
      totalTimeRemainingNanos: Long,
      turnTimeRemainingNanosOpt: Option[Long]
  )

  case class ServerPlayer(
      clientId: ClientId,
      username: Username,
      startedFirst: Boolean,
      myBoard: ServerMyBoard,
      enemyBoard: ServerEnemyBoard,
      turnPlayHistory: List[TurnPlay],
      currentTurnOpt: Option[Turn],
      currentTurnAttackTypes: List[AttackType],
      extraTurnQueue: List[ExtraTurn],
      timeRemaining: Option[ServerTimeRemaining],
      botHelper: Option[BotHelper]
  ) {

    val isHuman: Boolean = clientId != BotClientId

    def kills: Int = enemyBoard.totalShips - enemyBoard.shipsLeft

    def toPlayer(game: Game): Player =
      if (game.gameStarted)
        Player(
          Nil,
          Board(myBoard.boardSize, myBoard.ships),
          enemyBoard.boardMarks,
          turnPlayHistory
        )
      else
        Player(
          game.rules.gameFleet.ships,
          Board(myBoard.boardSize, myBoard.ships),
          Vector.empty,
          Nil
        )

    def getTimeRemaining: Option[TimeRemaining] =
      timeRemaining.map {
        case ServerTimeRemaining(totalTimeRemainingNanos, turnTimeRemainingNanos) =>
          TimeRemaining(
            (totalTimeRemainingNanos / 1000L / 1000L).toInt,
            turnTimeRemainingNanos.map(turnTime => (turnTime / 1000L / 1000L).toInt)
          )
      }

  }

  case class Game(
      gameId: GameId,
      messages: List[ChatMessage],
      boardSize: Coordinate,
      rules: Rules,
      player1: ServerPlayer,
      player2: ServerPlayer,
      playerWhoWonOpt: Option[Username],
      currentTurnPlayer: Option[Username],
      lastUpdateTimeOpt: Option[Instant]
  ) {

    val isInPlacingShipsMode: Boolean =
      player1.myBoard.ships.isEmpty || player2.myBoard.ships.isEmpty

    val gameStarted: Boolean = currentTurnPlayer.nonEmpty

    val gameIsActive: Boolean = currentTurnPlayer.nonEmpty && playerWhoWonOpt.isEmpty

    val gameIsOver: Boolean = playerWhoWonOpt.nonEmpty

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
        (
          me.currentTurnOpt.flatMap(currentTurn => currentTurnPlayer.map((currentTurn, _))),
          playerWhoWonOpt
        ) match {
          case (None, _) =>
            PreGameMode(me.myBoard.ships.nonEmpty, enemy.myBoard.ships.nonEmpty)
          case (Some((turn, currentPlayerUsername)), None) =>
            PlayingMode(
              currentPlayerUsername == me.username,
              turn = turn,
              turnAttackTypes = me.currentTurnAttackTypes,
              myTimeRemaining = me.getTimeRemaining,
              enemyTimeRemaining = enemy.getTimeRemaining
            )
          case (Some((turn, _)), Some(playerWhoWon)) =>
            GameOverMode(
              turn,
              me.username == playerWhoWon,
              myTimeRemaining = me.getTimeRemaining,
              enemyTimeRemaining = enemy.getTimeRemaining,
              enemyRealBoard = enemy.myBoard.ships
            )
        }

      GameState(
        gameId,
        rules,
        me.toPlayer(this),
        SimplePlayer(
          enemy.username,
          enemy.enemyBoard.boardSize,
          enemy.turnPlayHistory
        ),
        gameMode
      )
    }

    def placeShips(playerUsername: Username, shipPositions: List[ShipInBoard]): Game =
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
      if (gameIsActive)
        currentTurnPlayer.map(currentUsername =>
          if (currentUsername == player1.username) player1 else player2
        )
      else
        None

  }

  def sendMsg(gameId: GameId, playerUsername: Username, msgText: String): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some(game) =>
          val msgTrimmed = msgText.trim
          val msg = ChatMessage(msgTrimmed, playerUsername.username, new Date())
          val updatedGame =
            game.modify(_.messages).using(msg :: _)

          updateServerState(updatedGame)

          sendMessage(game, msg)
      }
    }

  def reload(clientId: ClientId, playerUsername: Username): Future[Unit] = Future {
    activeGamesByPlayer.get(playerUsername).foreach { game =>
      val updatedGame =
        game.updatePlayerByUsername(playerUsername, _.copy(clientId = clientId))

      updateServerState(updatedGame)
      updateBothGameState(updatedGame)
    }
  }

  def startGameWithBots(playerUsername: Username, rules: Rules): Future[Unit] = Future {
    rpcClientsService.getClientIdByUsername(playerUsername) match {
      case Some(playerId) =>
        createNewGame((playerId, playerUsername), None, rules) match {
          case Some(game) =>
            updateServerState(game, None)
            updateBothGameState(game)
          case None =>
            sendSystemMessage(playerUsername, "Error starting game vs bot! (Invalid rules)")
        }
      case _ =>
        sendSystemMessage(playerUsername, "Error starting game vs bot!")
    }
  }

  def startGame(
      player1Username: Username,
      player2Username: Username,
      rules: Rules
  ): Future[Unit] =
    Future {
      (
        rpcClientsService.getClientIdByUsername(player1Username),
        rpcClientsService.getClientIdByUsername(player2Username)
      ) match {
        case (Some(player1Id), Some(player2Id)) =>
          val player2Username = rpcClientsService.authenticatedClients(player2Id).username
          createNewGame(
            (player1Id, player1Username),
            Some((player2Id, player2Username)),
            rules
          ) match {
            case Some(game) =>
              updateServerState(game, None)
              updateBothGameState(game)
            case None =>
              sendSystemMessage(
                player1Username,
                s"Could not find player '$player2Username' (Invalid rules)"
              )
          }
        case _ =>
          sendSystemMessage(player1Username, s"Could not find player '$player2Username'")
      }
    }

  private def createNewGame(
      player1Data: (ClientId, Username),
      player2DataOpt: Option[(ClientId, Username)],
      rules: Rules
  ): Option[Game] = {
    if (rules.gameFleet.ships.isEmpty)
      None
    else {
      val gameId = GameId(UUID.randomUUID().toString)

      val boardSize = rules.boardSize
      val myBoard: ServerMyBoard = ServerMyBoard(boardSize, Nil)
      val enemyBoard: ServerEnemyBoard =
        ServerEnemyBoard(
          boardSize,
          BoardUtils.createEmptyBoardMarks(boardSize),
          rules.gameFleet.shipAmount,
          rules.gameFleet.shipAmount
        )

      val player1First: Boolean = Random.nextBoolean()

      val (player1Id, player1Username) = player1Data
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
          extraTurnQueue = Nil,
          timeRemaining = None,
          botHelper = None
        )
      val (player2Id, player2Username) = player2DataOpt.getOrElse((BotClientId, BotUsername))
      val botHelper: Option[BotHelper] =
        if (player2DataOpt.nonEmpty)
          None
        else
          Some(new BotHelper(gameId, rules, BotHelperLogger.DefaultLogger))
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
          extraTurnQueue = Nil,
          timeRemaining = None,
          botHelper = botHelper
        )

      Some(
        Game(
          gameId = gameId,
          messages = Nil,
          boardSize = boardSize,
          rules = rules,
          player1 = player1,
          player2 = player2,
          playerWhoWonOpt = None,
          currentTurnPlayer = None,
          lastUpdateTimeOpt = None
        )
      )
    }
  }

  def quitCurrentGame(gameId: GameId, playerUsername: Username): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some(game) =>
          removeServerGame(game)
          updateBothQuitGame(game)
      }
    }

  def logout(playerUsername: Username): Future[Unit] =
    Future {
      rpcClientsService
        .getClientIdByUsername(playerUsername)
        .foreach(rpcClientsService.unregisterConnection)
    }

  def getAllMessages(playerUsername: Username): Future[Seq[ChatMessage]] =
    Future {
      activeGamesByPlayer.get(playerUsername) match {
        case None =>
          Seq.empty
        case Some(game) =>
          game.messages.reverse
      }
    }

  def rematchGame(gameId: GameId, playerUsername: Username): Future[Unit] =
    Future {
      activeGames.get(gameId).tap {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some(game) =>
          removeServerGame(game)
          updateBothQuitGame(game)
      }
    }.map {
      case Some(oldGame) =>
        if (!oldGame.player2.isHuman)
          startGameWithBots(oldGame.player1.username, oldGame.rules)
        else
          startGame(oldGame.player1.username, oldGame.player2.username, oldGame.rules)
      case None =>
    }

  def confirmShips(
      gameId: GameId,
      playerUsername: Username,
      shipPositions: List[ShipInBoard]
  ): Future[Unit] =
    Future {
      activeGames.get(gameId) match {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
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

              val serverTimeRemaining: Option[ServerTimeRemaining] =
                gameWithShips.rules.timeLimit.map {
                  case RuleTimeLimit(initialTotalTimeSeconds, additionalTurnTimeSeconds) =>
                    ServerTimeRemaining(
                      initialTotalTimeSeconds.toLong * 1000L * 1000L * 1000L,
                      additionalTurnTimeSeconds.map(_._1 * 1000L * 1000L * 1000L)
                    )
                }

              gameWithShips
                .modifyAll(_.player1.currentTurnOpt, _.player2.currentTurnOpt)
                .setTo(Some(Turn(1, None)))
                .modifyAll(_.player1.currentTurnAttackTypes, _.player2.currentTurnAttackTypes)
                .setTo(gameWithShips.rules.defaultTurnAttackTypes)
                .modifyAll(_.player1.timeRemaining, _.player2.timeRemaining)
                .setTo(serverTimeRemaining)
                .modify(_.currentTurnPlayer)
                .setTo(Some(playerThatStartsUsername))
            } else
              gameWithShips

          val instantNow = Instant.now()
          updateServerState(gameWithUpdatedMode.copy(lastUpdateTimeOpt = Some(instantNow)), None)

          if (readyToStart)
            updateBothGameState(gameWithUpdatedMode)
          else
            updateBothGameMode(gameWithUpdatedMode)
        case _ =>
          sendSystemMessage(playerUsername, "Invalid request!")
      }
    }

  def cancelShipsPlacement(
      gameId: GameId,
      playerUsername: Username
  ): Future[Unit] =
    Future {
      activeGames.get(gameId).map(game => (game, game.getPlayerSafe(playerUsername))) match {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some((game, Some(player))) if !game.gameStarted && player.myBoard.ships.nonEmpty =>
          val updatedPlayer = player.modify(_.myBoard.ships).setTo(Nil)
          val updatedGame = game.updatePlayer(updatedPlayer)

          updateServerState(updatedGame)
          updateBothGameMode(updatedGame)
        case _ =>
          sendSystemMessage(playerUsername, "Invalid request!")
      }
    }

  def sendTurnAttacks(
      gameId: GameId,
      playerUsername: Username,
      currentTurn: Turn,
      turnAttacks: List[Attack]
  ): Future[Unit] = {
    def validateTurnAttacks(me: ServerPlayer): Boolean = {
      val sizeIsValid = {
        val turnAttacksSize = turnAttacks.size
        turnAttacksSize == me.currentTurnAttackTypes.size || {
          me.enemyBoard.hasNFreeSpaces == turnAttacksSize
        }
      }

      sizeIsValid &&
      turnAttacks.zip(me.currentTurnAttackTypes).forall {
        // TODO this does not work property when turnAttacks.size < me.currentTurnAttackTypes.size
        case (Attack(attackType, Some(Coordinate(x, y))), expectedAttackType)
            if attackType == expectedAttackType =>
          val (hitTurnOpt, boardMark) = me.enemyBoard.boardMarks(x)(y)
          hitTurnOpt.isEmpty && !boardMark.isPermanent
        case _ =>
          false
      }
    }

    Future {
      activeGames.get(gameId).map(game => (game, game.getPlayerSafe(playerUsername))) match {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some((game, Some(player))) if {
              game.getCurrentTurnPlayer.exists { currentPlayer =>
                currentPlayer.currentTurnOpt.contains(currentTurn) &&
                currentPlayer.username == player.username
              } &&
              validateTurnAttacks(player)
            } =>
          unsafeSendTurnAttacks(game, player, currentTurn, turnAttacks)
        case Some((game, _)) =>
          sendSystemMessage(
            playerUsername,
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

  private def unsafeSendTurnAttacks(
      game: Game,
      player: ServerPlayer,
      currentTurn: Turn,
      turnAttacks: List[Attack]
  ): Unit = {
    val instantNow = Instant.now()
    val gameWithLastTurnUpdated = updateGameTime(game, instantNow)

    val enemy = gameWithLastTurnUpdated.enemyPlayer(player)
    val hits: List[(Coordinate, Option[ShipInBoard])] =
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
            (true, Int.MaxValue)
          case ShipHit(shipId, destroyed) =>
            (destroyed, shipId.id)
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
    val updatedGameWithPlayer: Game = gameWithLastTurnUpdated.updatePlayer(updatedPlayer)
    val updatedGameWithTurnTime =
      if (gameOver) {
        setGameOver(updatedGameWithPlayer, updatedPlayer.username)
      } else {
        val bonusRewardList: List[BonusReward] =
          rewardExtraTurns(gameBeforeHits = game, turnPlay = turnPlay)
        val extraTurnBonuses: List[ExtraTurn] =
          bonusRewardList.collect { case extraTurn @ ExtraTurn(_) => extraTurn }

        val updatedGameWithNextTurn: Game =
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

              updatedGameWithPlayer.updatePlayer(updatedPlayerWithExtraTurns)
            case Nil =>
              val updatedPlayerWithExtraTurns =
                updatedPlayer
                  .modify(_.currentTurnOpt)
                  .using(_.map { case Turn(currentTurn, _) => Turn(currentTurn + 1, None) })
                  .modify(_.currentTurnAttackTypes)
                  .setTo(updatedGameWithPlayer.rules.defaultTurnAttackTypes)

              updatedGameWithPlayer
                .updatePlayer(updatedPlayerWithExtraTurns)
                .modify(_.currentTurnPlayer)
                .using(_.map(updatedGameWithPlayer.enemyPlayer(_).username))
          }

        updatedGameWithNextTurn.getCurrentTurnPlayer match {
          case None =>
            updatedGameWithNextTurn
          case Some(serverPlayer) =>
            updatedGameWithNextTurn.rules.timeLimit match {
              case Some(RuleTimeLimit(_, Some((additionalTurnTimeSeconds, _)))) =>
                updatedGameWithNextTurn.updatePlayer(
                  serverPlayer.modify(_.timeRemaining).using {
                    _.map { serverTimeRemaining =>
                      serverTimeRemaining.copy(turnTimeRemainingNanosOpt =
                        Some(additionalTurnTimeSeconds * 1000L * 1000L * 1000L)
                      )
                    }
                  }
                )
              case _ =>
                updatedGameWithNextTurn
            }
        }
      }

    val finalGameUpdated = updateGameTime(updatedGameWithTurnTime, Instant.now())
    updateServerState(finalGameUpdated)
    updateBothGameState(finalGameUpdated)
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
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some((game, Some(player))) if game.gameStarted =>
          val updatedBoardMarks: BoardMarks =
            updatedBoardMarksList.foldLeft(player.enemyBoard.boardMarks) {
              case (boardMarks, (coor, newBoardMark)) =>
                updateBoardMarksUsing(
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
          sendSystemMessage(playerUsername, "Invalid request!")
      }
    }

  private def sendSystemMessage(username: Username, message: String): Unit = {
    if (username != BotUsername)
      rpcClientsService
        .getClientIdByUsername(username)
        .foreach(sendSystemMessage(_, message))
  }

  private def sendSystemMessage(clientId: ClientId, message: String): Unit = {
    if (clientId != BotClientId)
      rpcClientsService.sendMessage(clientId, message)
  }

  private def sendSystemMessage(game: Game, message: String): Unit = {
    if (game.player1.isHuman)
      sendSystemMessage(game.player1.clientId, message)
    if (game.player2.isHuman)
      sendSystemMessage(game.player2.clientId, message)
  }

  private def sendMessage(game: Game, chatMessage: ChatMessage): Unit = {
    if (game.player1.isHuman)
      rpcClientsService.sendMessage(game.player1.clientId, chatMessage)
    if (game.player2.isHuman)
      rpcClientsService.sendMessage(game.player2.clientId, chatMessage)
  }

  private def updateBothGameState(game: Game): Unit = {
    if (game.player1.isHuman)
      rpcClientsService.sendGameState(
        game.player1.clientId,
        game.toGameStatePlayer1
      )
    if (game.player2.isHuman)
      rpcClientsService.sendGameState(
        game.player2.clientId,
        game.toGameStatePlayer2
      )
  }

  private def updateBothGameMode(game: Game): Unit = {
    if (game.player1.isHuman)
      rpcClientsService.sendGameMode(
        game.player1.clientId,
        game.toGameStatePlayer1.gameMode
      )
    if (game.player2.isHuman)
      rpcClientsService.sendGameMode(
        game.player2.clientId,
        game.toGameStatePlayer2.gameMode
      )
  }

  private def updateBothQuitGame(game: Game): Unit = {
    if (game.player1.isHuman)
      rpcClientsService.sendQuitGame(game.player1.clientId)
    if (game.player2.isHuman)
      rpcClientsService.sendQuitGame(game.player2.clientId)
  }

}

object GameService {

  val BotClientId: ClientId = ClientId("0")
  val BotUsername: Username = Username("Bot")

}
