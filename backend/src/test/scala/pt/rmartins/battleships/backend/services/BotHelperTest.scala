package pt.rmartins.battleships.backend.services

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Assertion, Inspectors}
import pt.rmartins.battleships.shared.model.game.AttackType._
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit.WithoutRuleTimeLimit
import pt.rmartins.battleships.shared.model.game.Ship._
import pt.rmartins.battleships.shared.model.game._

import java.util.UUID
import scala.annotation.tailrec

class BotHelperTest extends AnyWordSpec with Matchers with MockFactory with Inspectors {

  private val turnAttackTypes2: List[AttackType] = List.fill(2)(Simple)
  private val turnAttackTypes3: List[AttackType] = List.fill(3)(Simple)
  private val turnAttackTypes4: List[AttackType] = List.fill(4)(Simple)

  "placeAttacks - single ships" should {

    "shoot only one missile and ignore the 2 radars" in {
      val rules = Rules(
        boardSize = Coordinate(3, 3),
        gameFleet = Fleet.fromShips(List(Ranger -> 1)),
        defaultTurnAttacks = List(Simple, Radar, Radar),
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val result = placeAttacks(botHelper, Nil)

      result.map(_.attackType) should be(List(Simple))
    }

    "find Ranger ship with only 1 possible position" in {
      val rules = Rules(
        boardSize = Coordinate(3, 3),
        gameFleet = Fleet.fromShips(List(Ranger -> 1)),
        defaultTurnAttacks = turnAttackTypes2,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (2, 0), (1, 1)),
          hitTurn(2, hits = List((Ranger, 2, 2)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((0, 2), (1, 2)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(result, rules.defaultTurnAttacks.size, expectedCoordinates)
    }

    "find AircraftCarrier ship with only 1 possible position" in {
      val rules = Rules(
        boardSize = Coordinate(5, 5),
        gameFleet = Fleet.fromShips(List(AircraftCarrier -> 1)),
        defaultTurnAttacks = turnAttackTypes3,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (2, 0), (2, 2), (0, 4)),
          hitTurn(2, hits = List((AircraftCarrier, 0, 1)), water = List((0, 0), (0, 2)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((2, 1), (1, 1), (1, 3), (1, 2)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(result, rules.defaultTurnAttacks.size, expectedCoordinates)
    }

    "find Conqueror ship with only 1 possible position (2 turns match)" in {
      val rules = Rules(
        boardSize = Coordinate(6, 6),
        gameFleet = Fleet.fromShips(List(Conqueror -> 1)),
        defaultTurnAttacks = turnAttackTypes2,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          hitTurn(1, hits = List((Conqueror, 3, 0)), water = List((0, 2), (5, 5))),
          hitTurn(2, hits = List((Conqueror, 3, 3)), water = List((2, 4), (1, 5)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((3, 1), (3, 2)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(result, rules.defaultTurnAttacks.size, expectedCoordinates)
    }

    "find 2 ships at once (Ranger & Conqueror) #1" in {
      val rules = Rules(
        boardSize = Coordinate(6, 6),
        gameFleet = Fleet.fromShips(List(Ranger -> 1, Conqueror -> 1)),
        defaultTurnAttacks = turnAttackTypes3,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          hitTurn(1, hits = List((Ranger, 0, 0), (Ranger, 0, 1))),
          hitTurn(2, hits = List((Conqueror, 3, 0), (Conqueror, 3, 1)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((0, 2), (3, 2), (3, 3)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(result, rules.defaultTurnAttacks.size, expectedCoordinates)
    }

    "find correct position of Conqueror #1" in {
      val rules = Rules(
        boardSize = Coordinate(4, 4),
        gameFleet = Fleet.fromShips(List(Skeeter -> 1, Conqueror -> 1)),
        defaultTurnAttacks = turnAttackTypes3,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          hitTurn(1, hits = List((Skeeter, 0, 1))),
          hitTurn(2, hits = List((Conqueror, 3, 1)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinatesSure: Set[Coordinate] =
        Set((3, 0), (3, 2), (3, 3)).map { case (x, y) => Coordinate(x, y) }

      val expectedCoordinatesOther: Set[Coordinate] =
        Set((0, 0), (1, 1), (0, 2)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinatesSeq(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinatesSure,
        expectedCoordinatesSure,
        expectedCoordinatesOther
      )
    }

    "find correct position of Conqueror #2" in {
      val rules = Rules(
        boardSize = Coordinate(5, 5),
        gameFleet = Fleet.fromShips(List(Skeeter -> 1, Conqueror -> 1)),
        defaultTurnAttacks = turnAttackTypes4,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (0, 1), (1, 3), (3, 4)),
          hitTurn(2, hits = List((Skeeter, 1, 4)), water = List((4, 1))),
          hitTurn(3, hits = List((Conqueror, 3, 1)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((3, 0), (3, 2), (3, 3), (0, 4)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(result, rules.defaultTurnAttacks.size, expectedCoordinates)
    }

    "find AircraftCarrier ship with few remaining possible spots" in {
      val rules = Rules(
        boardSize = Coordinate(4, 4),
        gameFleet = Fleet.fromShips(List(AircraftCarrier -> 1)),
        defaultTurnAttacks = turnAttackTypes3,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (0, 0), (2, 0), (3, 0), (2, 2), (0, 3))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinatesSure: Set[Coordinate] =
        Set((1, 1), (2, 1), (1, 2)).map { case (x, y) => Coordinate(x, y) }

      val expectedCoordinatesOther: Set[Coordinate] =
        Set((1, 0), (0, 1), (3, 1), (1, 3)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinatesSeq(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinatesSure,
        expectedCoordinatesSure,
        expectedCoordinatesOther
      )
    }

    "find correct position of TorpedoBoat #1" in {
      val rules = Rules(
        boardSize = Coordinate(8, 8),
        gameFleet = Fleet.fromShips(List(AircraftCarrier -> 1, TorpedoBoat -> 1)),
        defaultTurnAttacks = turnAttackTypes2,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (4, 0), (4, 1)),
          totalMiss(2, (5, 2), (5, 3), (7, 3)),
          hitTurn(
            3,
            hits = List((AircraftCarrier, 6, 6), (TorpedoBoat, 7, 1)),
            water = List((4, 3))
          ),
          hitTurn(
            4,
            hits = List((AircraftCarrier, 5, 6), (TorpedoBoat, 5, 1), (TorpedoBoat, 6, 2))
          ),
          hitTurn(
            5,
            hits = List((AircraftCarrier, 6, 5), (AircraftCarrier, 6, 7), (TorpedoBoat, 5, 0))
          )
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((4, 6), (7, 0)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(result, rules.defaultTurnAttacks.size, expectedCoordinates)
    }

    "find correct position of Cruiser #1" in {
      val rules = Rules(
        boardSize = Coordinate(7, 7),
        gameFleet = Fleet.fromShips(List(Ranger -> 1, Cruiser -> 1)),
        defaultTurnAttacks = turnAttackTypes2,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (3, 1), (1, 3), (5, 0), (6, 1)),
          hitTurn(2, hits = List((Ranger, 1, 1)), water = List((6, 0))),
          hitTurn(3, hits = List((Cruiser, 4, 4)), water = List((2, 0))),
          hitTurn(4, hits = List((Cruiser, 5, 5)), water = List((0, 2)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates1: Set[Coordinate] =
        Set((3, 3), (6, 6)).map { case (x, y) => Coordinate(x, y) }

      val expectedCoordinates2: Set[Coordinate] =
        Set((1, 0), (0, 1), (2, 1), (1, 2)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinatesSeq(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates1,
        expectedCoordinates2
      )
    }

    "find correct position of Conqueror #3" in {
      val rules = Rules(
        boardSize = Coordinate(7, 7),
        gameFleet = Fleet.fromShips(List(Skeeter -> 1, Ranger -> 1, Conqueror -> 1)),
        defaultTurnAttacks = turnAttackTypes3,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (5, 1), (6, 2), (1, 1), (1, 3), (2, 6)),
          hitTurn(2, hits = List((Skeeter, 1, 2), (Ranger, 1, 6))),
          hitTurn(3, hits = List((Conqueror, 5, 2)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinatesSure: Set[Coordinate] =
        Set((5, 3), (5, 4), (5, 5)).map { case (x, y) => Coordinate(x, y) }

      val expectedCoordinatesOther: Set[Coordinate] =
        Set((0, 2), (1, 2), (2, 2), (3, 2), (1, 4), (1, 5), (0, 6))
          .map { case (x, y) => Coordinate(x, y) }

      containsCoordinatesSeq(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinatesSure,
        expectedCoordinatesOther,
        expectedCoordinatesOther
      )
    }

    "Send a triple kill" in {
      val rules = Rules(
        boardSize = Coordinate(8, 8),
        gameFleet = Fleet.fromShips(
          List(Submarine, Skeeter, Ranger, Conqueror, Cruiser, AircraftCarrier).map(_ -> 1)
        ),
        defaultTurnAttacks = turnAttackTypes3,
        gameBonuses = List(
          GameBonus(
            BonusType.TripleKill,
            List(BonusReward.ExtraTurn(List.fill(3)(AttackType.Simple)))
          )
        ),
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (1, 1)),
          hitTurn(2, hits = List((Skeeter, 0, 0), (Ranger, 7, 0), (Cruiser, 4, 0))),
          hitTurn(3, hits = List((Ranger, 7, 2), (Cruiser, 2, 2)), water = List((1, 0))),
          hitTurn(
            4,
            hits = List((AircraftCarrier, 3, 5), (AircraftCarrier, 4, 7), (AircraftCarrier, 5, 5))
          )
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((0, 1), (3, 1), (7, 1)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates
      )
    }

    "find correct position of all ships #1" in {
      val rules = Rules(
        boardSize = Coordinate(5, 5),
        gameFleet = Fleet.fromShips(List(Submarine, Skeeter, Ranger).map(_ -> 1)),
        defaultTurnAttacks = turnAttackTypes3,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (1, 0), (3, 0), (0, 1), (2, 2)),
          turnPlay(
            2,
            None,
            List((0, 0), (2, 0), (4, 0)),
            List((Submarine, true), (Skeeter, false), (Ranger, false))
          )
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((2, 1), (4, 1), (4, 2)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates
      )
    }

    "find correct position of LongShip ship #1" in {
      val rules = Rules(
        boardSize = Coordinate(6, 6),
        gameFleet = Fleet.fromShips(List(LongShip -> 1, TorpedoBoat -> 1)),
        defaultTurnAttacks = List.fill(5)(Simple),
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          hitTurn(1, hits = List((LongShip, 0, 0), (TorpedoBoat, 2, 4)), water = List((4, 2))),
          hitTurn(2, hits = List((TorpedoBoat, 3, 4)), water = List((3, 3))),
          hitTurn(3, hits = List((TorpedoBoat, 2, 2))),
          hitTurn(4, hits = List((TorpedoBoat, 1, 3)), water = List((4, 1))),
          turnPlay(5, None, List((3, 2)), List((TorpedoBoat, true)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((1, 0), (2, 0), (3, 0), (4, 0), (5, 0)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates
      )
    }

    "fill remaining empty positions #1" in {
      val rules = Rules(
        boardSize = Coordinate(6, 3),
        gameFleet = Fleet.fromShips(List(LongShip -> 1)),
        defaultTurnAttacks = List.fill(11)(Simple),
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (0, 1), (1, 1)),
          hitTurn(2, hits = List((LongShip, 0, 2)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] = {
        val exclude: Set[(Int, Int)] = Set((0, 1), (0, 2))
        (for (x <- 0 until 6; y <- List(0, 2)) yield (x, y))
          .filterNot(exclude)
          .map { case (x, y) => Coordinate(x, y) }
          .toSet
      }

      containsCoordinates(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates
      )
    }

    "fill remaining empty positions #2" in {
      val rules = Rules(
        boardSize = Coordinate(6, 3),
        gameFleet = Fleet.fromShips(List(LongShip -> 1)),
        defaultTurnAttacks = List.fill(15)(Simple),
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (0, 1), (1, 1)),
          hitTurn(2, hits = List((LongShip, 0, 2)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] = {
        val exclude: Set[(Int, Int)] = Set((0, 1), (0, 2), (1, 1))
        (for (x <- 0 until 6; y <- 0 until 3) yield (x, y))
          .filterNot(exclude)
          .map { case (x, y) => Coordinate(x, y) }
          .toSet
      }

      containsCoordinates(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates
      )
    }

  }

  "placeAttacks - multiple ships" ignore {

    "find correct position of both Conqueror ships #1" in {
      val rules = Rules(
        boardSize = Coordinate(4, 4),
        gameFleet = Fleet.fromShips(List(Conqueror -> 2)),
        defaultTurnAttacks = List.fill(6)(Simple),
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (0, 1), (2, 1), (1, 2), (3, 2)),
          hitTurn(2, hits = List((Conqueror, 1, 3), (Conqueror, 3, 3)), water = List((3, 1)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((0, 0), (0, 1), (0, 2), (0, 3), (0, 3), (2, 3)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates
      )
    }

    "find correct position of both Ranger ships #1" in {
      val rules = Rules(
        boardSize = Coordinate(4, 4),
        gameFleet = Fleet.fromShips(List(Ranger -> 2)),
        defaultTurnAttacks = turnAttackTypes4,
        gameBonuses = Nil,
        timeLimit = WithoutRuleTimeLimit
      )
      val botHelper = createBotHelper(rules)

      val turnHistory =
        List(
          totalMiss(1, (1, 0), (0, 1), (1, 2), (3, 2)),
          hitTurn(2, hits = List((Ranger, 3, 1), (Ranger, 2, 3)), water = List((0, 3)))
        )

      val result = placeAttacks(botHelper, turnHistory)

      val expectedCoordinates: Set[Coordinate] =
        Set((1, 1), (2, 1), (1, 3), (3, 3)).map { case (x, y) => Coordinate(x, y) }

      containsCoordinates(
        result,
        rules.defaultTurnAttacks.size,
        expectedCoordinates
      )
    }

  }

  private def containsCoordinates(
      result: List[Attack],
      size: Int,
      expectedCoordinates: Set[Coordinate]
  ): Assertion = {
    containsCoordinatesSeq(
      result,
      size,
      Seq.fill(size)(expectedCoordinates): _*
    )
  }

  private def containsCoordinatesSeq(
      result: List[Attack],
      size: Int,
      expectedCoordinatesSeq: Set[Coordinate]*
  ): Assertion = {
    val attacksCoordinatesList = result.flatMap(_.coordinateOpt)
    val attacksCoordinatesSet = attacksCoordinatesList.toSet

    attacksCoordinatesList.size should be(size)
    attacksCoordinatesSet.size should be(size)

    @tailrec
    def findAllExpected(
        coorList: List[Coordinate],
        coordinatesSeqOfSets: Seq[Set[Coordinate]]
    ): Assertion =
      coorList match {
        case Nil =>
          succeed
        case coor :: next =>
          coordinatesSeqOfSets.zipWithIndex.find(_._1(coor)) match {
            case None =>
              fail(s"$coorList not found in $coordinatesSeqOfSets")
            case Some((_, index)) =>
              findAllExpected(
                next,
                coordinatesSeqOfSets.take(index) ++ coordinatesSeqOfSets.drop(index + 1)
              )
          }
      }

    findAllExpected(attacksCoordinatesList, expectedCoordinatesSeq)
  }

  private def totalMiss(turnNumber: Int, missCoordinates: (Int, Int)*): TurnPlay = {
    missCoordinates should not be empty
    TurnPlay(
      Turn(turnNumber, None),
      missCoordinates.map { case (x, y) => Attack(Simple, Some(Coordinate(x, y))) }.toList,
      missCoordinates.map(_ => HitHint.Water).toList
    )
  }

  private def hitTurn(
      turnNumber: Int,
      hits: List[(Ship, Int, Int)],
      water: List[(Int, Int)] = Nil
  ): TurnPlay =
    TurnPlay(
      Turn(turnNumber, None),
      hits.map { case (_, x, y) => Attack(AttackType.Simple, Some(Coordinate(x, y))) } ++
        water.map { case (x, y) => Attack(AttackType.Simple, Some(Coordinate(x, y))) },
      hits.map { case (ship, _, _) => HitHint.ShipHit(ship.shipId, destroyed = false) } ++
        water.map(_ => HitHint.Water)
    )

  private def turnPlay(
      turnNumber: Int,
      extraTurn: Option[Int],
      coor: List[(Int, Int)],
      ships: List[(Ship, Boolean)]
  ): TurnPlay = {
    coor should not be empty
    coor.sizeIs >= ships.size should be(true)
    TurnPlay(
      Turn(turnNumber, extraTurn),
      coor.map { case (x, y) => Attack(AttackType.Simple, Some(Coordinate(x, y))) },
      ships.map { case (ship, destroyed) => HitHint.ShipHit(ship.shipId, destroyed = destroyed) } ++
        List.fill(coor.size - ships.size)(HitHint.Water)
    )
  }

  private def placeAttacks(botHelper: BotHelper, turnHistory: List[TurnPlay]) = {
    turnHistory match {
      case Nil =>
        botHelper.placeAttacks(botHelper.rules.defaultTurnAttacks)
      case _ =>
        turnHistory.map { turnPlay =>
          botHelper.updateBotBoardMarks(turnPlay)
          botHelper.placeAttacks(botHelper.rules.defaultTurnAttacks)
        }.last
    }
  }

  private def createBotHelper(rules: Rules): BotHelper =
    new BotHelper(
      gameId = GameId(UUID.randomUUID().toString),
      rules = rules,
      logger = BotHelperLogger.DefaultLogger
    )

}
