package pt.rmartins.battleships.backend.services

import com.softwaremill.quicklens.ModifyPimp
import io.udash.rpc.ClientId
import pt.rmartins.battleships.backend.services.GameService._
import pt.rmartins.battleships.backend.services.PuzzlesGenerator.PuzzleWithId
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.GameMode._
import pt.rmartins.battleships.shared.model.game.HitHint.{ShipHit, Water}
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit.WithRuleTimeLimit
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

  private val activeGamesByPlayer: mutable.Map[Username, GameId] = mutable.Map.empty
  private val activeGames: mutable.Map[GameId, Game] = mutable.Map.empty
  private val activePreGames: mutable.Map[GameId, PreGame] = mutable.Map.empty
  private val acceptedInviteRequests: mutable.Map[ClientId, ClientId] = mutable.Map.empty

  PuzzlesGenerator.initialize()

  private def updatePlayerActiveGames(preGame: PreGame): Unit =
    clock.synchronized {
      activeGamesByPlayer.update(preGame.player1.username, preGame.gameId)
      activeGamesByPlayer.update(preGame.player2.username, preGame.gameId)
    }

  private def updatePlayerActiveGames(game: Game): Unit =
    clock.synchronized {
      if (game.player1.isHuman)
        activeGamesByPlayer.update(game.player1.username, game.gameId)
      if (game.player2.isHuman)
        activeGamesByPlayer.update(game.player2.username, game.gameId)
    }

  private val clock: Runnable = () => {
    val cycleTimeMillis: Long = 200L
    while (true) {
      clock.synchronized {
        val initialInstant = Instant.now()
        activeGames.values.toList.filter(game => game.gameIsActive).foreach { game =>
          val updatedGame = updateGameTime(game, initialInstant)
          activeGames.update(updatedGame.gameId, updatedGame)
          if (updatedGame.gameIsOver)
            sendBothGameState(updatedGame)
        }
      }
      Thread.sleep(cycleTimeMillis)
    }
  }

  new Thread(clock).start()

  private val botUpdater: Runnable = () => {
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
                  game.rules.gameFleet.shipsList
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

  private val backgroundPuzzleGenerator: Runnable = () => {
    val cycleTime = 60000L
    val cycleTimeNoBot = 1000L

    while (PuzzlesGenerator.morePuzzlesNeeded) {
      PuzzlesGenerator.generatePuzzleAndSave(100)
      val botGameRunning =
        clock.synchronized {
          activeGames.values.toList.exists(game =>
            game.gameIsActive && game.bothPlayers.exists(!_.isHuman)
          )
        }
      Thread.sleep(if (botGameRunning) cycleTime else cycleTimeNoBot)
    }
  }

  new Thread(backgroundPuzzleGenerator).start()

  private def updateGameTime(game: Game, instant: Instant): Game = {
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

  private def setGameOver(game: Game, whoWonUsername: Username): Game = {
    if (game.isRealGame)
      sendSystemMessage(game, s"Game Over! Player '$whoWonUsername' won!")
    game.modify(_.playerWhoWonOpt).setTo(Some(whoWonUsername))
  }

  private def updateServerPreGame(preGame: PreGame): Unit =
    clock.synchronized {
      activePreGames.update(preGame.gameId, preGame)
    }

  private def updateServerState(
      game: Game,
      instantNow: Option[Instant] = Some(Instant.now())
  ): Game =
    clock.synchronized {
      val updatedGame = instantNow.map(updateGameTime(game, _)).getOrElse(game)
      activeGames.update(updatedGame.gameId, updatedGame)
      updatedGame
    }

  private def removeServerPreGameOnly(preGame: PreGame): Unit =
    clock.synchronized {
      activePreGames.remove(preGame.gameId)
    }

  private def removeServerPreGameCompletely(preGame: PreGame): Unit =
    clock.synchronized {
      activeGamesByPlayer.remove(preGame.player1.username)
      activeGamesByPlayer.remove(preGame.player2.username)
      activePreGames.remove(preGame.gameId)
    }

  private def removeServerGameOnly(game: Game): Unit =
    clock.synchronized {
      activeGames.remove(game.gameId)
    }

  private def removeServerGameCompletely(game: Game): Unit =
    clock.synchronized {
      activeGamesByPlayer.remove(game.player1.username)
      activeGamesByPlayer.remove(game.player2.username)
      activeGames.remove(game.gameId)
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

  def reload(clientId: ClientId, playerUsername: Username): Future[Unit] =
    Future {
      val gameIdOpt: Option[GameId] =
        activeGamesByPlayer.get(playerUsername)

      gameIdOpt
        .flatMap(activePreGames.get)
        .flatMap(preGame => preGame.getPlayer(playerUsername).map((preGame, _)))
        .foreach { case (preGame, player) =>
          val updatedPreGame: PreGame =
            preGame.updatePlayer(player.copy(clientId = clientId, acceptedRules = false))

          updateServerPreGame(updatedPreGame)
          sendBothPreGameState(updatedPreGame)
        }

      gameIdOpt
        .flatMap(activeGames.get)
        .flatMap(game => game.getPlayerSafe(playerUsername).map((game, _)))
        .foreach { case (game, player) =>
          val updatedGame: Game =
            game.updatePlayer(player.copy(clientId = clientId))

          updateServerState(updatedGame)
          sendBothGameState(updatedGame)
          updatedGame.requestInProgress.foreach { case (playerUsername, playerRequestType) =>
            sendToEnemyPlayerRequest(
              updatedGame.enemyPlayer(playerUsername).clientId,
              playerRequestType
            )
          }
        }
    }

  def startGameWithBots(playerUsername: Username, rules: Rules): Future[Unit] =
    Future {
      rpcClientsService.getClientIdByUsername(playerUsername) match {
        case Some(playerId) =>
          createNewGame(
            GameId(UUID.randomUUID().toString),
            (playerId, playerUsername),
            None,
            rules
          ) match {
            case Some(game) =>
              updatePlayerActiveGames(game)
              updateServerState(game, None)
              sendBothGameState(game)
            case None =>
              sendSystemMessage(playerUsername, "Error starting game vs bot! (Invalid rules)")
          }
        case _ =>
          sendSystemMessage(playerUsername, "Error starting game vs bot!")
      }
    }

  def sendPlayerRequest(
      gameId: GameId,
      playerUsername: Username,
      playerRequestType: PlayerRequestType
  ): Future[Unit] =
    Future {
      activeGames
        .get(gameId)
        .flatMap(game => game.getPlayerSafe(playerUsername).map((game, _))) match {
        case Some((game, player)) =>
          game.requestInProgress match {
            case None =>
              val updatedGame: Game =
                game
                  .modify(_.requestInProgress)
                  .setTo(Some((player.username, playerRequestType)))

              updateServerState(updatedGame, None)
              sendToEnemyPlayerRequest(game.enemyPlayer(player).clientId, playerRequestType)
            case _ =>
              sendSystemMessage(playerUsername, "There is already a request in progress!")
          }
        case _ =>
          sendSystemMessage(playerUsername, "Error sending player request!")
      }
    }

  def sendPlayerRequestAnswer(
      gameId: GameId,
      playerUsername: Username,
      playerRequestType: PlayerRequestType,
      answer: Boolean
  ): Future[Unit] =
    Future {
      activeGames
        .get(gameId)
        .flatMap(game => game.getPlayerSafe(playerUsername).map((game, _))) match {
        case Some((game, player)) =>
          val enemy = game.enemyPlayer(player)
          game.requestInProgress match {
            case Some((currentRequestUsername, currentPlayerRequestType))
                if !game.gameStarted &&
                  currentRequestUsername == enemy.username &&
                  currentPlayerRequestType == playerRequestType =>
              if (answer) {
                val preGame: PreGame =
                  PreGame.fromActiveGame(game)

                removeServerGameOnly(game)
                updateServerPreGame(preGame)
                sendBothPreGameState(preGame)
              } else {
                val updatedGame: Game =
                  game.modify(_.requestInProgress).setTo(None)

                updateServerState(updatedGame, None)
                sendToEnemyPlayerRequestAnswer(enemy.clientId, playerRequestType, answer)
                sendSystemMessage(
                  enemy.username,
                  s"Player '${player.username}' declined your request!"
                )
              }
            case Some((_, _)) =>
              sendSystemMessage(playerUsername, "Error sending player request answer!")
            case None =>
              sendSystemMessage(playerUsername, "There is already a request in progress!")
          }
        case _ =>
          sendSystemMessage(playerUsername, "Error sending player request answer!")
      }
    }

  def invitePlayer(
      player1Username: Username,
      player2Username: Username
  ): Future[Unit] =
    Future {
      (
        rpcClientsService.getClientIdByUsername(player1Username),
        rpcClientsService.getClientIdByUsername(player2Username)
      ) match {
        case (Some(player1Id), Some(player2Id)) if player1Id == player2Id =>
          rpcClientsService.sendUserErrorMessage(player1Id, UserError.InviteItself)
        case (Some(_), Some(player2Id)) =>
          rpcClientsService.sendInviteRequest(player2Id, player1Username)
        case (Some(player1Id), None) =>
          rpcClientsService.sendInviteResponse(
            player1Id,
            player2Username,
            inviteAnswer = false
          )
          rpcClientsService.sendUserErrorMessage(
            player1Id,
            UserError.UsernameNotFound(player2Username)
          )
        case _ =>
      }
    }

  def playerInviteAnswer(
      player1Username: Username,
      player2Username: Username,
      inviteAnswer: Boolean
  ): Future[Unit] =
    Future {
      (
        rpcClientsService.getClientIdByUsername(player1Username),
        rpcClientsService.getClientIdByUsername(player2Username)
      ) match {
        case (Some(player1Id), Some(player2Id)) =>
          if (inviteAnswer)
            acceptedInviteRequests.update(player1Id, player2Id)
          rpcClientsService.sendInviteResponse(player2Id, player1Username, inviteAnswer)
        case _ =>
      }
    }

  def startPreGameWithPlayer(
      player1Username: Username,
      player2Username: Username,
      rules: Rules
  ): Future[Unit] =
    Future {
      (
        rpcClientsService.getClientIdByUsername(player1Username),
        rpcClientsService.getClientIdByUsername(player2Username)
      ) match {
        case (Some(player1Id), Some(player2Id))
            if player1Id != player2Id &&
              acceptedInviteRequests.get(player2Id).contains(player1Id) =>
          val preGame: PreGame =
            createNewPreGame(
              (player1Id, player1Username),
              (player2Id, player2Username),
              rules
            )
          acceptedInviteRequests.remove(player2Id)
          updatePlayerActiveGames(preGame)
          updateServerPreGame(preGame)
          sendBothPreGameState(preGame)
        case (Some(player1Id), None) =>
          rpcClientsService.sendUserErrorMessage(
            player1Id,
            UserError.UsernameNotFound(player2Username)
          )
        case _ =>
          sendSystemMessage(player1Username, "Invalid request!")
      }
    }

  def confirmRules(gameId: GameId, playerUsername: Username): Future[Unit] =
    Future {
      activePreGames
        .get(gameId)
        .flatMap(preGame => preGame.getPlayers(playerUsername).map((preGame, _))) match {
        case Some((preGame, (player1, player2))) if !player1.acceptedRules =>
          if (player2.acceptedRules)
            startGame(preGame)
          else {
            val updatedPreGame: PreGame =
              preGame.updatePlayer(player1.modify(_.acceptedRules).setTo(true))
            updateServerPreGame(updatedPreGame)
            sendPreGameConfirmState(updatedPreGame)
          }
        case _ =>
      }
    }

  def cancelRules(gameId: GameId, playerUsername: Username): Future[Unit] =
    Future {
      activePreGames
        .get(gameId)
        .flatMap(preGame => preGame.getPlayer(playerUsername).map((preGame, _))) match {
        case Some((preGame, player)) if player.acceptedRules =>
          val updatedPreGame: PreGame =
            preGame.updatePlayer(player.modify(_.acceptedRules).setTo(false))
          updateServerPreGame(updatedPreGame)
          sendPreGameConfirmState(updatedPreGame)
        case _ =>
      }
    }

  private def createNewPreGame(
      player1Data: (ClientId, Username),
      player2Data: (ClientId, Username),
      rules: Rules
  ): PreGame = {
    val gameId = GameId(UUID.randomUUID().toString)

    val player1: PreGamePlayer =
      PreGamePlayer(player1Data._1, player1Data._2, acceptedRules = false)

    val player2: PreGamePlayer =
      PreGamePlayer(player2Data._1, player2Data._2, acceptedRules = false)

    PreGame(
      gameId,
      Nil,
      rules,
      player1,
      player2
    )
  }

  def sendRulesPatch(
      gameId: GameId,
      playerUsername: Username,
      preGameRulesPatch: PreGameRulesPatch
  ): Future[Unit] =
    Future {
      activePreGames
        .get(gameId)
        .flatMap(preGame => preGame.getPlayer(playerUsername).map((preGame, _))) match {
        case Some((preGame, player)) if preGameRulesPatch.containsPatch =>
          def check(patchResult: (PreGame, Boolean), simpleRulesPatch: PreGameRulesPatch): Unit = {
            patchResult match {
              case (_, false) =>
              case (updatedPreGame, true) =>
                val finalPreGame: PreGame =
                  updatedPreGame
                    .modify(_.player1.acceptedRules)
                    .setTo(false)
                    .modify(_.player2.acceptedRules)
                    .setTo(false)

                updateServerPreGame(finalPreGame)
                if (
                  updatedPreGame.player1.acceptedRules ||
                  updatedPreGame.player2.acceptedRules
                )
                  sendPreGameConfirmState(finalPreGame)
                sendPreGameRulesPatch(updatedPreGame.getEnemy(player).clientId, simpleRulesPatch)
            }
          }

          preGameRulesPatch match {
            case PreGameRulesPatch(Some(boardSizePatch), _, _, _, _) =>
              check(
                preGame.receiveRulesPatchBoardSize(boardSizePatch),
                PreGameRulesPatch(boardSizePatch = Some(boardSizePatch))
              )
            case PreGameRulesPatch(_, Some((shipId, amountPatch)), _, _, _) =>
              check(
                preGame.receiveRulesPatchFleet(shipId, amountPatch),
                PreGameRulesPatch(gameFleetPatch = Some((shipId, amountPatch)))
              )
            case PreGameRulesPatch(_, _, Some(defaultTurnAttacksPatch), _, _) =>
              check(
                preGame.receiveRulesPatchDefaultAttacks(defaultTurnAttacksPatch),
                PreGameRulesPatch(defaultTurnAttacksPatch = Some(defaultTurnAttacksPatch))
              )
            case PreGameRulesPatch(_, _, _, Some(turnBonusesPatch), _) =>
              check(
                preGame.receiveRulesPatchTurnBonuses(turnBonusesPatch),
                PreGameRulesPatch(turnBonusesPatch = Some(turnBonusesPatch))
              )
            case PreGameRulesPatch(_, _, _, _, Some(timeLimitPatch)) =>
              check(
                preGame.receiveRulesPatchTimeLimit(timeLimitPatch),
                PreGameRulesPatch(timeLimitPatch = Some(timeLimitPatch))
              )
            case _ =>
          }
        case _ =>
      }
    }

  private def startGame(preGame: PreGame): Unit =
    createNewGame(
      preGame.gameId,
      (preGame.player1.clientId, preGame.player1.username),
      Some((preGame.player2.clientId, preGame.player2.username)),
      preGame.rules
    ) match {
      case Some(game) =>
        removeServerPreGameOnly(preGame)
        updateServerState(game, None)
        sendBothGameState(game)
      case None =>
    }

  private def createNewGame(
      gameId: GameId,
      player1Data: (ClientId, Username),
      player2DataOpt: Option[(ClientId, Username)],
      rules: Rules
  ): Option[Game] =
    if (rules.gameFleet.shipAmount == 0)
      None
    else {
      val simplifiedRules: Rules =
        rules.modify(_.gameFleet).using { case Fleet(shipCounterList) =>
          Fleet(shipCounterList.filterNot(_._2._1 == 0))
        }

      val boardSize = simplifiedRules.boardSize
      val myBoard: ServerMyBoard = ServerMyBoard(boardSize, Nil)
      val enemyBoard: ServerEnemyBoard =
        ServerEnemyBoard(
          boardSize,
          BoardUtils.createEmptyBoardMarks(boardSize),
          simplifiedRules.gameFleet.shipAmount,
          simplifiedRules.gameFleet.shipAmount
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
          Some(new BotHelper(gameId, simplifiedRules, BotHelperLogger.DefaultLogger))
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
          rules = simplifiedRules,
          player1 = player1,
          player2 = player2,
          playerWhoWonOpt = None,
          currentTurnPlayer = None,
          lastUpdateTimeOpt = None,
          requestInProgress = None,
          isRealGame = true
        )
      )
    }

  def quitCurrentGame(gameId: GameId, playerUsername: Username): Future[Unit] =
    Future {
      (activePreGames.get(gameId), activeGames.get(gameId)) match {
        case (Some(preGame), _) =>
          removeServerPreGameCompletely(preGame)
          sendBothQuitGame(preGame)
        case (_, Some(game)) =>
          removeServerGameCompletely(game)
          sendBothQuitGame(game)
        case _ =>
          sendSystemMessage(playerUsername, "Error finding game!")
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
      activeGamesByPlayer.get(playerUsername).flatMap(activeGames.get) match {
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
          removeServerGameCompletely(game)
          sendBothQuitGame(game)
      }
    }.map {
      case Some(oldGame) =>
        if (!oldGame.player2.isHuman)
          startGameWithBots(oldGame.player1.username, oldGame.rules)
        else
          startPreGameWithPlayer(
            oldGame.player1.username,
            oldGame.player2.username,
            oldGame.rules
          )
      case None =>
    }

  def confirmShips(
      gameId: GameId,
      playerUsername: Username,
      shipPositions: List[ShipInBoard]
  ): Future[Unit] = {
    def isValidFleet(gameFleet: Fleet, shipPositions: List[ShipInBoard]): Boolean = {
      gameFleet.shipCounterMap.forall { case (shipId, (amount, _)) =>
        shipPositions.count(_.ship.shipId == shipId) == amount
      }
    }

    Future {
      activeGames.get(gameId) match {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some(game) if !game.gameStarted && isValidFleet(game.rules.gameFleet, shipPositions) =>
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
                gameWithShips.rules.timeLimit.toOption.map {
                  case WithRuleTimeLimit(initialTotalTimeSeconds, additionalTurnTimeSeconds) =>
                    ServerTimeRemaining(
                      initialTotalTimeSeconds.toLong * 1000L * 1000L * 1000L,
                      additionalTurnTimeSeconds.map(_._1 * 1000L * 1000L * 1000L)
                    )
                }

              gameWithShips
                .modifyAll(_.player1.currentTurnOpt, _.player2.currentTurnOpt)
                .setTo(Some(Turn(1, None)))
                .modifyAll(_.player1.currentTurnAttackTypes, _.player2.currentTurnAttackTypes)
                .setTo(gameWithShips.rules.defaultTurnAttacks)
                .modifyAll(_.player1.timeRemaining, _.player2.timeRemaining)
                .setTo(serverTimeRemaining)
                .modify(_.currentTurnPlayer)
                .setTo(Some(playerThatStartsUsername))
            } else
              gameWithShips

          val instantNow = Instant.now()
          updateServerState(gameWithUpdatedMode.copy(lastUpdateTimeOpt = Some(instantNow)), None)

          if (readyToStart)
            sendBothGameState(gameWithUpdatedMode)
          else
            sendBothGameMode(gameWithUpdatedMode)
        case _ =>
          sendSystemMessage(playerUsername, "Invalid request!")
      }
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
          sendBothGameMode(updatedGame)
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
          hitTurnOpt.isEmpty && boardMark != BoardMark.Water
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
    if (!gameWithLastTurnUpdated.gameIsOver) {
      val updatedGameWithTurnTime: Game =
        updateGameWithTurnAttacksUnsafe(
          Some(this),
          gameWithLastTurnUpdated,
          player,
          currentTurn,
          turnAttacks
        )

      val finalGameUpdated: Game = updateGameTime(updatedGameWithTurnTime, Instant.now())
      updateServerState(finalGameUpdated)
      sendBothGameState(finalGameUpdated)
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

  def addToEnemyTimeSeconds(
      gameId: GameId,
      playerUsername: Username,
      secondsToAdd: Int
  ): Future[Unit] =
    Future {
      activeGames.get(gameId).map(game => (game, game.getPlayerSafe(playerUsername))) match {
        case None =>
          sendSystemMessage(playerUsername, "Error finding game!")
        case Some((game, Some(player))) if game.gameIsActive =>
          val enemy: ServerPlayer = game.enemyPlayer(player)
          val updatedEnemy: ServerPlayer =
            enemy
              .modify(_.timeRemaining)
              .using(_.map {
                case ServerTimeRemaining(totalTimeRemainingNanos, turnTimeRemainingNanosOpt) =>
                  ServerTimeRemaining(
                    totalTimeRemainingNanos + secondsToAdd * 1000L * 1000L * 1000L,
                    turnTimeRemainingNanosOpt
                  )
              })
          val updatedGame: Game = game.updatePlayer(updatedEnemy)
          updateServerState(updatedGame)
          sendBothGameState(updatedGame)
        case _ =>
          sendSystemMessage(playerUsername, "Invalid request!")
      }
    }

  def getRandomPuzzle(): Future[Option[(PuzzleId, PlayerPuzzle)]] =
    Future.successful(
      PuzzlesGenerator.getRandomPuzzle.map { case PuzzleWithId(puzzleId, puzzle) =>
        println(puzzleId)
        (puzzleId, puzzle.playerPuzzle)
      }
    )

  def getPuzzleSolution(puzzleId: PuzzleId): Future[Option[PuzzleSolution]] =
    Future.successful(
      PuzzlesGenerator.getPuzzle(puzzleId).map { case PuzzleWithId(_, puzzle) =>
        puzzle.puzzleSolution
      }
    )

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

  private def sendBothGameState(game: Game): Unit = {
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

  private def sendBothGameMode(game: Game): Unit = {
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

  private def sendBothQuitGame(preGame: PreGame): Unit = {
    rpcClientsService.sendQuitGame(preGame.player1.clientId)
    rpcClientsService.sendQuitGame(preGame.player2.clientId)
  }

  private def sendBothQuitGame(game: Game): Unit = {
    if (game.player1.isHuman)
      rpcClientsService.sendQuitGame(game.player1.clientId)
    if (game.player2.isHuman)
      rpcClientsService.sendQuitGame(game.player2.clientId)
  }

  private def sendBothPreGameState(preGame: PreGame): Unit = {
    rpcClientsService.sendPreGameState(
      preGame.player1.clientId,
      PreGameState(
        preGame.gameId,
        preGame.player2.username,
        preGame.player1.acceptedRules,
        preGame.player2.acceptedRules,
        preGame.rules
      )
    )
    rpcClientsService.sendPreGameState(
      preGame.player2.clientId,
      PreGameState(
        preGame.gameId,
        preGame.player1.username,
        preGame.player2.acceptedRules,
        preGame.player1.acceptedRules,
        preGame.rules
      )
    )
  }

  private def sendPreGameConfirmState(preGame: PreGame): Unit = {
    rpcClientsService.sendPreGameConfirmStates(
      preGame.player1.clientId,
      preGame.player1.acceptedRules,
      preGame.player2.acceptedRules
    )
    rpcClientsService.sendPreGameConfirmStates(
      preGame.player2.clientId,
      preGame.player2.acceptedRules,
      preGame.player1.acceptedRules
    )
  }

  private def sendPreGameRulesPatch(
      clientId: ClientId,
      preGameRulesPatch: PreGameRulesPatch
  ): Unit = {
    rpcClientsService.sendPreGameRulesPatch(
      clientId,
      preGameRulesPatch
    )
  }

  private def sendToEnemyPlayerRequest(
      enemyClientId: ClientId,
      playerRequestType: PlayerRequestType
  ): Unit = {
    rpcClientsService.sendPlayerRequest(
      enemyClientId,
      playerRequestType
    )
  }

  private def sendToEnemyPlayerRequestAnswer(
      enemyClientId: ClientId,
      playerRequestType: PlayerRequestType,
      answer: Boolean
  ): Unit = {
    rpcClientsService.sendPlayerRequestAnswer(
      enemyClientId,
      playerRequestType,
      answer
    )
  }

}

object GameService {

  val BotClientId: ClientId = ClientId("0")
  val BotUsername: Username = Username("Bot")

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

    def updateMarksSimple(newForcedBoardMarks: List[(Coordinate, BoardMark)]): ServerEnemyBoard = {
      val updatedBoardMarks: BoardMarks =
        newForcedBoardMarks.foldLeft(boardMarks) { case (marks, (coor, boardMark)) =>
          updateBoardMarksUsing(marks, coor, { case (turnOpt, _) => (turnOpt, boardMark) })
        }

      copy(boardMarks = updatedBoardMarks)
    }

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
        _.count { case (opt, boardMark) => opt.isEmpty && boardMark != BoardMark.Water }
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
          Board(myBoard.boardSize, myBoard.ships),
          enemyBoard.boardMarks,
          turnPlayHistory
        )
      else
        Player(
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

  case class PreGamePlayer(
      clientId: ClientId,
      username: Username,
      acceptedRules: Boolean
  )

  case class PreGame(
      gameId: GameId,
      messages: List[ChatMessage], // TODO ?
      rules: Rules,
      player1: PreGamePlayer,
      player2: PreGamePlayer
  ) {

    def getPlayer(playerUsername: Username): Option[PreGamePlayer] =
      if (player1.username == playerUsername) Some(player1)
      else if (player2.username == playerUsername) Some(player2)
      else None

    def getEnemy(player: PreGamePlayer): PreGamePlayer =
      if (player.clientId == player1.clientId) player2 else player1

    def getPlayers(playerUsername: Username): Option[(PreGamePlayer, PreGamePlayer)] =
      getPlayer(playerUsername).map(player => (player, getEnemy(player)))

    def updatePlayer(updatedPlayer: PreGamePlayer): PreGame =
      if (updatedPlayer.username == player1.username)
        copy(player1 = updatedPlayer)
      else
        copy(player2 = updatedPlayer)

    def receiveRulesPatchBoardSize(
        boardSizePatch: Coordinate
    ): (PreGame, Boolean) =
      rules.boardSize match {
        case currentBoardSize if currentBoardSize == boardSizePatch =>
          (this, false)
        case _ =>
          (this.modify(_.rules.boardSize).setTo(boardSizePatch), true)
      }

    def receiveRulesPatchFleet(
        shipId: ShipId,
        amountPatch: Int
    ): (PreGame, Boolean) =
      rules.gameFleet.shipCounterMap.get(shipId) match {
        case None if 0 == amountPatch =>
          (this, false)
        case Some((currentAmount, _)) if currentAmount == amountPatch =>
          (this, false)
        case None =>
          val updatedFleet: Fleet =
            Fleet(rules.gameFleet.updateCounter(shipId, amountPatch, Rotation.Rotation0))
          (this.modify(_.rules.gameFleet).setTo(updatedFleet), true)
        case Some((_, rotation)) =>
          val updatedFleet: Fleet =
            Fleet(rules.gameFleet.updateCounter(shipId, amountPatch, rotation))
          (this.modify(_.rules.gameFleet).setTo(updatedFleet), true)
      }

    def receiveRulesPatchDefaultAttacks(
        defaultTurnAttacksPatch: List[AttackType]
    ): (PreGame, Boolean) =
      rules.defaultTurnAttacks match {
        case currentDefaultTurnAttacks if currentDefaultTurnAttacks == defaultTurnAttacksPatch =>
          (this, false)
        case _ =>
          (this.modify(_.rules.defaultTurnAttacks).setTo(defaultTurnAttacksPatch), true)
      }

    def receiveRulesPatchTurnBonuses(
        turnBonusesPatch: List[TurnBonus]
    ): (PreGame, Boolean) =
      rules.turnBonuses match {
        case currentTurnBonuses if currentTurnBonuses == turnBonusesPatch =>
          (this, false)
        case _ =>
          (this.modify(_.rules.turnBonuses).setTo(turnBonusesPatch), true)
      }

    def receiveRulesPatchTimeLimit(
        timeLimitPatch: RuleTimeLimit
    ): (PreGame, Boolean) =
      rules.timeLimit match {
        case currentTimeLimit if currentTimeLimit == timeLimitPatch =>
          (this, false)
        case _ =>
          (this.modify(_.rules.timeLimit).setTo(timeLimitPatch), true)
      }

  }

  object PreGame {
    def fromActiveGame(game: Game): PreGame =
      PreGame(
        gameId = game.gameId,
        messages = Nil, // TODO add this?
        rules = game.rules,
        player1 =
          PreGamePlayer(game.player1.clientId, game.player1.username, acceptedRules = false),
        player2 = PreGamePlayer(game.player2.clientId, game.player2.username, acceptedRules = false)
      )
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
      lastUpdateTimeOpt: Option[Instant],
      requestInProgress: Option[(Username, PlayerRequestType)],
      isRealGame: Boolean
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
            PlacingShipsMode(me.myBoard.ships.nonEmpty, enemy.myBoard.ships.nonEmpty)
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
          enemy.isHuman,
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

  private[services] def updateGameWithTurnAttacksUnsafe(
      gameServiceOpt: Option[GameService],
      game: Game,
      player: ServerPlayer,
      currentTurn: Turn,
      turnAttacks: List[Attack]
  ): Game = {
    val enemy = game.enemyPlayer(player)
    val hits: List[(Coordinate, Option[ShipInBoard])] =
      turnAttacks
        .filter(_.attackType == AttackType.Simple)
        .map(_.coordinateOpt.get)
        .map { case coor @ Coordinate(x, y) =>
          coor -> enemy.myBoard.efficientShipCheck(x)(y)
        }

    val radarShots: List[(Coordinate, BoardMark)] =
      turnAttacks.flatMap {
        case Attack(AttackType.Radar, Some(coor @ Coordinate(x, y))) =>
          val boardMark: BoardMark =
            enemy.myBoard.efficientShipCheck(x)(y) match {
              case Some(_) => BoardMark.ShipHit
              case None    => BoardMark.Water
            }
          Some(coor -> boardMark)
        case _ =>
          None
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
        .using(_.updateMarksSimple(radarShots))
        .modify(_.enemyBoard)
        .using(_.updateMarks(currentTurn, hits))
        .modify(_.enemyBoard.shipsLeft)
        .using(_ - killsThisTurn)

    val gameOver = updatedPlayer.enemyBoard.shipsLeft == 0
    val updatedGameWithPlayer: Game = game.updatePlayer(updatedPlayer)
    val updatedGameWithTurnTime: Game =
      if (gameOver)
        gameServiceOpt match {
          case None =>
            simpleSetGameOver(updatedGameWithPlayer, updatedPlayer.username)
          case Some(gameService) =>
            gameService.setGameOver(updatedGameWithPlayer, updatedPlayer.username)
        }
      else {
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
                  .setTo(updatedGameWithPlayer.rules.defaultTurnAttacks)

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
              case WithRuleTimeLimit(_, Some((additionalTurnTimeSeconds, _))) =>
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

    updatedGameWithTurnTime
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

  private def simpleSetGameOver(game: Game, whoWonUsername: Username): Game = {
    game.modify(_.playerWhoWonOpt).setTo(Some(whoWonUsername))
  }

}
