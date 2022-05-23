package pt.rmartins.battleships.backend.services

import com.softwaremill.quicklens.ModifyPimp
import io.udash.rpc.ClientId
import org.scalamock.matchers.ArgCapture.CaptureOne
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import pt.rmartins.battleships.shared.model.game._

import scala.concurrent.Future

class GameServiceTest extends AsyncWordSpec with Matchers with AsyncMockFactory {

  "sendMsg" should {

    "Save an error message if the gameId is not valid" in {
      val playerUsername = Username("player1")
      val clientId = ClientId("abc")
      val gameId = GameId("game123")
      val errorMsg = "Error finding game!"

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(playerUsername)
        .returning(Some(clientId))
      (clientMock
        .sendMessage(_: ClientId, _: String))
        .expects(clientId, errorMsg)
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.sendMsg(gameId, playerUsername, errorMsg)
        chatMessages <- service.getAllMessages(playerUsername)
      } yield {
        chatMessages.size should be(0)
      }
    }

    "Save an message if gameId is valid" ignore {
      val playerUsername = Username("player1")
      val clientId = ClientId("abc")
      val gameId = GameId("game123")
      val msgText = "Hello"

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(playerUsername)
        .returning(Some(clientId))
      val service = new GameService(clientMock)

      for {
        _ <- service.sendMsg(gameId, playerUsername, msgText)
        chatMessages <- service.getAllMessages(playerUsername)

      } yield {
        chatMessages.size should be(1)

        chatMessages.head.author should be("player1")
        chatMessages.head.text should be("Hello")
      }
    }

  }

  "invitePlayer" should {

    "return an error if the player is inviting himself" in {
      val player1Username = Username("player1")
      val player1Id = ClientId("id1")

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
        .twice()
      (clientMock.sendUserErrorMessage _)
        .expects(player1Id, UserError.InviteItself)
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.invitePlayer(player1Username, player1Username)
      } yield {
        succeed
      }
    }

    "send an invite request from player1 to player2" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")
      val player2Id = ClientId("id2")

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
      (clientMock.getClientIdByUsername _)
        .expects(player2Username)
        .returning(Some(player2Id))
      (clientMock.sendInviteRequest _)
        .expects(player2Id, player1Username)
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.invitePlayer(player1Username, player2Username)
      } yield {
        succeed
      }
    }

    "return an error if the player2 doesn't exist" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
      (clientMock.getClientIdByUsername _)
        .expects(player2Username)
        .returning(None)
      (clientMock.sendInviteResponse _)
        .expects(player1Id, player2Username, false)
        .returning(())
      (clientMock.sendUserErrorMessage _)
        .expects(player1Id, UserError.UsernameNotFound(player2Username))
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.invitePlayer(player1Username, player2Username)
      } yield {
        succeed
      }
    }

  }

  "playerInviteAnswer" should {

    "send an invite response from player1 to player2" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")
      val player2Id = ClientId("id2")
      val inviteAnswer = true

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
      (clientMock.getClientIdByUsername _)
        .expects(player2Username)
        .returning(Some(player2Id))
      (clientMock.sendInviteResponse _)
        .expects(player2Id, player1Username, inviteAnswer)
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.playerInviteAnswer(player1Username, player2Username, inviteAnswer)
      } yield {
        succeed
      }
    }

  }

  "startPreGameWithPlayer" should {

    "return an error if the player1 == player2" in {
      val player1Username = Username("player1")
      val player1Id = ClientId("id1")
      val errorMessage = "Invalid request!"

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
        .repeated(3)
      (clientMock
        .sendMessage(_: ClientId, _: String))
        .expects(player1Id, errorMessage)
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.startPreGameWithPlayer(
          player1Username,
          player1Username,
          BattleshipsMocks.default10v10Rules
        )
      } yield {
        succeed
      }
    }

    "return an error if the player2 didn't accepted request" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")
      val player2Id = ClientId("id2")
      val errorMessage = "Invalid request!"

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
        .twice()
      (clientMock.getClientIdByUsername _)
        .expects(player2Username)
        .returning(Some(player2Id))
      (clientMock
        .sendMessage(_: ClientId, _: String))
        .expects(player1Id, errorMessage)
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.startPreGameWithPlayer(
          player1Username,
          player2Username,
          BattleshipsMocks.default10v10Rules
        )
      } yield {
        succeed
      }
    }

    "start a game with player2" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")
      val player2Id = ClientId("id2")
      val rules = BattleshipsMocks.default10v10Rules

      val preGameState1 = CaptureOne[PreGameState]()
      val preGameState2 = CaptureOne[PreGameState]()

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
        .twice()
      (clientMock.getClientIdByUsername _)
        .expects(player2Username)
        .returning(Some(player2Id))
        .twice()
      (clientMock.sendInviteResponse _)
        .expects(player1Id, player2Username, true)
        .returning(())
      (clientMock.sendPreGameState _)
        .expects(player1Id, capture(preGameState1))
        .returning(())
      (clientMock.sendPreGameState _)
        .expects(player2Id, capture(preGameState2))
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.playerInviteAnswer(
          player2Username,
          player1Username,
          inviteAnswer = true
        )
        _ <- service.startPreGameWithPlayer(
          player1Username,
          player2Username,
          rules
        )
      } yield {
        preGameState1.value should be(
          PreGameState(
            preGameState1.value.gameId,
            player2Username,
            confirmRules = false,
            enemyConfirmedRules = false,
            rules
          )
        )
        preGameState2.value should be(
          PreGameState(
            preGameState2.value.gameId,
            player1Username,
            confirmRules = false,
            enemyConfirmedRules = false,
            rules
          )
        )
      }
    }

    "return an error if the player2 doesn't exist" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")

      val clientMock = mock[RpcClientsService]
      (clientMock.getClientIdByUsername _)
        .expects(player1Username)
        .returning(Some(player1Id))
      (clientMock.getClientIdByUsername _)
        .expects(player2Username)
        .returning(None)
      (clientMock.sendUserErrorMessage _)
        .expects(player1Id, UserError.UsernameNotFound(player2Username))
        .returning(())
      val service = new GameService(clientMock)

      for {
        _ <- service.startPreGameWithPlayer(
          player1Username,
          player2Username,
          BattleshipsMocks.default10v10Rules
        )
      } yield {
        succeed
      }
    }

  }

  "confirmRules" should {

    "send confirm message to player2 when player1 confirms rules" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")
      val player2Id = ClientId("id2")

      val (clientMock, service, runStartPreGameWithPlayer) =
        prepareStartPreGameWithPlayerMock(
          player1Username,
          player1Id,
          player2Username,
          player2Id,
          BattleshipsMocks.default10v10Rules
        )

      (clientMock.sendPreGameConfirmStates _)
        .expects(player1Id, true, false)
        .returning(())
      (clientMock.sendPreGameConfirmStates _)
        .expects(player2Id, false, true)
        .returning(())

      for {
        gameId <- runStartPreGameWithPlayer()
        _ <- service.confirmRules(gameId, player1Username)
      } yield {
        succeed
      }
    }

    "start game player1 confirms rules (and player2 already had confirmed rules)" in {
      val player1Username = Username("player1")
      val player2Username = Username("player2")
      val player1Id = ClientId("id1")
      val player2Id = ClientId("id2")
      val rules = BattleshipsMocks.default10v10Rules

      val (clientMock, service, runStartPreGameWithPlayer) =
        prepareStartPreGameWithPlayerMock(
          player1Username,
          player1Id,
          player2Username,
          player2Id,
          rules
        )

      (clientMock.sendPreGameConfirmStates _)
        .expects(player1Id, true, false)
        .returning(())
      (clientMock.sendPreGameConfirmStates _)
        .expects(player2Id, false, true)
        .returning(())

      val gameState1 = CaptureOne[GameState]()
      val gameState2 = CaptureOne[GameState]()

      (clientMock.sendGameState _)
        .expects(player1Id, capture(gameState1))
        .returning(())
      (clientMock.sendGameState _)
        .expects(player2Id, capture(gameState2))
        .returning(())

      for {
        gameId <- runStartPreGameWithPlayer()
        _ <- service.confirmRules(gameId, player1Username)
        _ <- service.confirmRules(gameId, player2Username)
      } yield {
        val simplifiedRules: Rules =
          rules.modify(_.gameFleet).using { case Fleet(shipCounterList) =>
            Fleet(shipCounterList.filterNot(_._2._1 == 0))
          }

        val board: Board =
          Board(boardSize = rules.boardSize, ships = Nil)
        val player1: Player =
          Player(
            myBoard = board,
            enemyBoardMarks = Vector.empty,
            turnPlayHistory = Nil
          )
        val player2: Player =
          Player(
            myBoard = board,
            enemyBoardMarks = Vector.empty,
            turnPlayHistory = Nil
          )
        val simplePlayer1: SimplePlayer =
          SimplePlayer(
            username = player1Username,
            isHuman = true,
            boardSize = rules.boardSize,
            turnPlayHistory = Nil
          )
        val simplePlayer2: SimplePlayer =
          SimplePlayer(
            username = player2Username,
            isHuman = true,
            boardSize = rules.boardSize,
            turnPlayHistory = Nil
          )

        gameState1.value should be(
          GameState(
            gameId = gameId,
            rules = simplifiedRules,
            me = player1,
            enemy = simplePlayer2,
            gameMode = GameMode.PlacingShipsMode(iPlacedShips = false, enemyPlacedShips = false)
          )
        )
        gameState2.value should be(
          GameState(
            gameId = gameId,
            rules = simplifiedRules,
            me = player2,
            enemy = simplePlayer1,
            gameMode = GameMode.PlacingShipsMode(iPlacedShips = false, enemyPlacedShips = false)
          )
        )
      }
    }

  }

  private def prepareStartPreGameWithPlayerMock(
      player1Username: Username,
      player1Id: ClientId,
      player2Username: Username,
      player2Id: ClientId,
      rules: Rules
  ): (RpcClientsService, GameService, () => Future[GameId]) = {
    val preGameState1 = CaptureOne[PreGameState]()
    val preGameState2 = CaptureOne[PreGameState]()

    val clientMock = mock[RpcClientsService]
    (clientMock.getClientIdByUsername _)
      .expects(player1Username)
      .returning(Some(player1Id))
      .twice()
    (clientMock.getClientIdByUsername _)
      .expects(player2Username)
      .returning(Some(player2Id))
      .twice()
    (clientMock.sendInviteResponse _)
      .expects(player1Id, player2Username, true)
      .returning(())
    (clientMock.sendPreGameState _)
      .expects(player1Id, capture(preGameState1))
      .returning(())
    (clientMock.sendPreGameState _)
      .expects(player2Id, capture(preGameState2))
      .returning(())
    val service = new GameService(clientMock)

    (
      clientMock,
      service,
      () =>
        for {
          _ <- service.playerInviteAnswer(
            player2Username,
            player1Username,
            inviteAnswer = true
          )
          _ <- service.startPreGameWithPlayer(
            player1Username,
            player2Username,
            rules
          )
        } yield preGameState1.value.gameId
    )
  }

}
