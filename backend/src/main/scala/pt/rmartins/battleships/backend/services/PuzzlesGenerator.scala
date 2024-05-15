package pt.rmartins.battleships.backend.services

import io.udash.rpc.ClientId
import pt.rmartins.battleships.backend.services.BotHelper.BotBoardMark
import pt.rmartins.battleships.backend.services.BotHelperLogger.DefaultLogger
import pt.rmartins.battleships.backend.services.GameService._
import pt.rmartins.battleships.shared.model.game.PuzzleObjective._
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit.WithoutRuleTimeLimit
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils

import scala.annotation.tailrec
import scala.util.Random

object PuzzlesGenerator {

  private val PuzzleVersion: Double = 1.0

  private val smallShips: List[Ship] =
    Ship.allShipsList.filter(_.piecesSize <= 5)

  case class PuzzleWithId(puzzleId: PuzzleId, puzzle: Puzzle)

  private val PuzzlesMaximumAmount = 1000
  private var puzzlesVector: Vector[PuzzleWithId] = Vector.empty
  private var puzzlesMap: Map[PuzzleId, PuzzleWithId] = Map.empty

  def morePuzzlesNeeded: Boolean = puzzlesVector.size < PuzzlesMaximumAmount

  private def nextInt(min: Int, max: Int): Int = Random.nextInt(max - min + 1) + min

  def main(args: Array[String]): Unit = {
    val initialTime = System.currentTimeMillis()
    for (_ <- 1 to 50)
      generatePuzzleAndSave(500000L) match {
        case None =>
          println("No puzzle found :(")
        case Some(PuzzleWithId(_, puzzle)) =>
          println(s"Found a puzzle in ${System.currentTimeMillis() - initialTime}ms")
          println(puzzle.playerPuzzle.gameFleet.shipsList.map(_.name))
          val boardSize = puzzle.playerPuzzle.boardSize
          puzzle.puzzleSolution match {
            case PuzzleSolution.CorrectShipBoardMarksSolution(board) =>
              DefaultLogger.logBotBoardMarks(
                boardSize,
                board.ships
                  .flatMap(ship => ship.shipActualPieces.map((ship.ship.shipId, _)))
                  .foldLeft(BotHelper.createEmptyBotBoardMarks(boardSize)) {
                    case (botBoardMarks, (shipId, coordinate)) =>
                      BotHelper.forceSetBoardMark(
                        botBoardMarks,
                        coordinate,
                        BotBoardMark.ShipExclusive(Set(shipId))
                      )
                  }
              )
            case PuzzleSolution.CorrectAttacksSolution(solution) => ???
          }
          println(
            puzzle.playerPuzzle.turnPlayHistory.mkString("\n")
          )
      }
  }

  def initialize(): Unit =
    puzzlesVector.synchronized {
      puzzlesVector = PuzzlesUtils.loadAllPuzzles().toVector
      puzzlesMap = puzzlesVector.map(PuzzleWithId => PuzzleWithId.puzzleId -> PuzzleWithId).toMap
    }

  @tailrec
  def generatePuzzleAndSave(maxMilliseconds: Long): Option[PuzzleWithId] =
    if (maxMilliseconds > 0 && morePuzzlesNeeded) {
      val initialTime = System.currentTimeMillis()
      createNewPuzzle(CorrectShipBoardMarks) match {
        case Some(puzzle) =>
          val puzzleWithId: PuzzleWithId = PuzzlesUtils.save(puzzle)
          puzzlesVector.synchronized {
            puzzlesVector = puzzlesVector :+ puzzleWithId
            puzzlesMap = puzzlesMap + (puzzleWithId.puzzleId -> puzzleWithId)
          }
          Some(puzzleWithId)
        case None =>
          val elapsed = System.currentTimeMillis() - initialTime
          generatePuzzleAndSave(maxMilliseconds - elapsed)
      }
    } else
      None

  def getRandomPuzzle(puzzlesSolvedSet: Set[PuzzleId]): Option[PuzzleWithId] =
    puzzlesVector.synchronized {
      val puzzlesNotSolved: Vector[PuzzleWithId] =
        puzzlesVector.filterNot(puzzle => puzzlesSolvedSet(puzzle.puzzleId))

      if (puzzlesNotSolved.isEmpty)
        None
      else {
        Some(puzzlesNotSolved(Random.nextInt(puzzlesNotSolved.size)))
      }
    }

  def getPuzzle(puzzleId: PuzzleId): Option[PuzzleWithId] =
    puzzlesMap.get(puzzleId)

  private def createNewPuzzle(puzzleObjective: PuzzleObjective): Option[Puzzle] = {
    puzzleObjective match {
      case CorrectShipBoardMarks =>
        val boardSize: Coordinate = Coordinate.square(nextInt(6, 8))
        BotHelper.placeShipsAtRandom(
          boardSize,
          Random.shuffle(smallShips).take(3)
        ) match {
          case None =>
            None
          case Some(shipsInPlace) =>
            makeSomePlays(Board(boardSize, shipsInPlace))
        }
      case WinInXTurns(maximumTurns) =>
        ???
    }
  }

  private def simpleGame(board: Board, defaultTurnAttacks: Int): (Game, BotHelper) = {
    val rules: Rules =
      Rules(
        boardSize = board.boardSize,
        gameFleet = Fleet.fromShipsList(board.getSimpleShipsList),
        defaultTurnAttacks = List.fill(defaultTurnAttacks)(AttackType.Simple),
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )

    val botHelper: BotHelper =
      new BotHelper(
        GameId(""),
        rules,
        BotHelperLogger.EmptyLogger
      )

    val enemyBoard: ServerEnemyBoard =
      ServerEnemyBoard(
        boardSize = board.boardSize,
        BoardUtils.createEmptyBoardMarks(board.boardSize),
        rules.gameFleet.shipsAmount,
        rules.gameFleet.shipsAmount
      )

    val botPlayer: ServerPlayer =
      ServerPlayer(
        clientId = BotClientId,
        username = BotUsername,
        startedFirst = true,
        myBoard = ServerMyBoard(board.boardSize, board.ships),
        enemyBoard = enemyBoard,
        turnPlayHistory = Nil,
        currentTurnOpt = None,
        currentTurnAttackTypes = Nil,
        extraTurnQueue = Nil,
        timeRemaining = None,
        botHelper = Some(botHelper)
      )

    val dummyPlayer: ServerPlayer =
      ServerPlayer(
        clientId = ClientId("1"),
        username = BotUsername,
        startedFirst = false,
        myBoard = ServerMyBoard(board.boardSize, board.ships),
        enemyBoard = enemyBoard,
        turnPlayHistory = Nil,
        currentTurnOpt = None,
        currentTurnAttackTypes = Nil,
        extraTurnQueue = Nil,
        timeRemaining = None,
        botHelper = Some(
          new BotHelper(
            GameId(""),
            rules,
            BotHelperLogger.EmptyLogger
          )
        )
      )

    (
      Game(
        gameId = GameId(""),
        messages = Nil,
        boardSize = board.boardSize,
        rules = rules,
        player1 = botPlayer,
        player2 = dummyPlayer,
        playerWhoWonOpt = None,
        currentTurnPlayer = None,
        lastUpdateTimeOpt = None,
        requestInProgress = None,
        isRealGame = false
      ),
      botHelper
    )
  }

  private def makeSomePlays(board: Board): Option[Puzzle] = {
    val (game, botHelper) = simpleGame(board, 3)

    val maxTurns: Int = 4

    @tailrec
    def attacksXTypes(game: Game, turns: Int): Game = {
      val attacks = botHelper.placeAttacks(botHelper.rules.defaultTurnAttacks)
      val updateGame: Game =
        GameService.updateGameWithTurnAttacksUnsafe(
          None,
          game,
          game.player1,
          Turn(turns, None),
          attacks
        )
      if (isGoodPuzzle(updateGame, botHelper))
        game
      else {
        botHelper.updateBotBoardMarks(updateGame.player1.turnPlayHistory.head)
        if (turns == maxTurns || isGoodPuzzle(updateGame, botHelper))
          updateGame
        else
          attacksXTypes(updateGame, turns + 1)
      }
    }

    val finalGame = attacksXTypes(game, 1)

    if (isGoodPuzzle(finalGame, botHelper))
      Some(
        Puzzle(
          puzzleVersion = PuzzleVersion,
          playerPuzzle = PlayerPuzzle(
            boardSize = board.boardSize,
            gameFleet = Fleet.fromShipsList(
              board.getSimpleShipsList.map(_.copy(rotation = Rotation.Rotation0))
            ),
            initialBoardMarks = finalGame.player1.enemyBoard.boardMarks,
            turnPlayHistory = finalGame.player1.turnPlayHistory,
            puzzleObjective = CorrectShipBoardMarks
          ),
          puzzleSolution = PuzzleSolution.CorrectShipBoardMarksSolution(
            Board(finalGame.player1.myBoard.boardSize, finalGame.player1.myBoard.ships)
          )
        )
      )
    else
      None
  }

  private def isGoodPuzzle(
      game: Game,
      botHelper: BotHelper
  ): Boolean = {
    val botBoardMarks = botHelper.getBotBoardMarks
    val coorsWithoutShips: Set[Coordinate] =
      Coordinate.fill(game.player1.myBoard.boardSize) --
        game.player1.myBoard.ships.flatMap(_.shipActualPieces).toSet
    game.player1.myBoard.ships.forall { shipInBoard =>
      shipInBoard.shipActualPieces.forall { coor =>
        BotHelper.getMark(botBoardMarks, coor) match {
          case BotBoardMark.ShipExclusive(shipIds) if shipIds == Set(shipInBoard.ship.shipId) =>
            true
          case _ =>
            false
        }
      }
    } &&
    coorsWithoutShips.forall { coor =>
      BotHelper.getMark(botBoardMarks, coor) match {
        case BotBoardMark.Empty => true
        case BotBoardMark.Water => true
        case _                  => false
      }
    }
  }

}
