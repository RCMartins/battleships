package pt.rmartins.battleships.backend.services

import pt.rmartins.battleships.backend.services.BotHelper.BotBoardMark._
import pt.rmartins.battleships.backend.services.BotHelper._
import pt.rmartins.battleships.shared.model.game._

import scala.annotation.tailrec
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

class BotHelper(gameId: GameId, val rules: Rules, val logger: BotHelperLogger) {

  import logger._

  private val boardSize: Coordinate = rules.boardSize

  private val allShipGuessers: Map[ShipId, ShipGuesser] = {
    val allShipIds: List[ShipId] = rules.gameFleet.shipCounterList.map(_._1)
    allShipIds.map(shipId => shipId -> new ShipGuesser(rules, shipId, this)).toMap
  }

  private var cachedTurnPlays: List[TurnPlay] =
    Nil

  def getCachedTurnPlays: List[TurnPlay] =
    cachedTurnPlays

  private var cachedBotBoardMarks: BotBoardMarks =
    createEmptyBotBoardMarks(boardSize)

  def getBotBoardMarks: BotBoardMarks = cachedBotBoardMarks

  private val standardTripleKill: Boolean =
    rules.gameBonuses.exists(turnBonus =>
      turnBonus.bonusType == BonusType.TripleKill &&
        turnBonus.bonusRewardList.contains(BonusReward.ExtraTurn(List.fill(3)(AttackType.Simple)))
    )

  def updateBotBoardMarks(turnPlay: TurnPlay): Unit = {
    cachedTurnPlays = turnPlay :: cachedTurnPlays
    allShipGuessers.foreach(_._2.updateTurnPlay(turnPlay))

    val TurnPlay(turn, turnAttacks, hitHints) = turnPlay
    val coordinates = turnAttacks.flatMap(_.coordinateOpt).toSet
    val shipIds = hitHints.flatMap(_.shipIdOpt).toSet

    val allWater = hitHints.forall(_.isWater)
    val allShipHit = hitHints.forall(_.isShip)
    val allSubmarineHits =
      allShipHit && hitHints.forall(_.shipIdDestroyedOpt.contains(Ship.Submarine.shipId))

    // Update board marks with direct turnPlay data
    val updatedBotBoardMarks: BotBoardMarks =
      coordinates.foldLeft(cachedBotBoardMarks) { case (botBoardMarks, coor) =>
        if (allWater)
          updateBoardMarksUsing(botBoardMarks, coor, _ => (Some(turn), Water))
        else if (allShipHit)
          updateBoardMarksUsing(
            botBoardMarks,
            coor,
            {
              case (_, ShipExclusive(currentShipIds)) =>
                (Some(turn), ShipExclusive(currentShipIds.filter(shipIds)))
              case (_, ShipOrWater(currentShipIds)) =>
                (Some(turn), ShipExclusive(currentShipIds.filter(shipIds)))
              case _ =>
                (Some(turn), ShipExclusive(shipIds))
            }
          )
        else
          updateBoardMarksUsing(
            botBoardMarks,
            coor,
            {
              case (_, Water) =>
                (Some(turn), Water)
              case (_, Empty) =>
                (Some(turn), ShipOrWater(shipIds))
              case (_, ShipOrWater(currentShipIds)) =>
                (Some(turn), shipOrWater(currentShipIds.filter(shipIds)))
              case (_, ShipExclusive(currentShipIds)) =>
                (Some(turn), ShipExclusive(currentShipIds.filter(shipIds)))
            }
          )
      }

    // Update board marks with all submarineHits or allShipHit
    val updatedBotBoardMarks2: BotBoardMarks =
      if (allSubmarineHits)
        turnAttacks
          .flatMap(_.coordinateOpt.map(_.get8CoorAround).getOrElse(Nil))
          .filter(_.isInsideBoard(rules.boardSize))
          .foldLeft(updatedBotBoardMarks) { case (marks, coor) =>
            updateBoardMarksUsing(
              marks,
              coor,
              { case (turnOpt, _) => (turnOpt, Water) }
            )
          }
      else if (allShipHit) {
        (coordinates.flatMap(_.get8CoorAround) -- coordinates)
          .filter(_.isInsideBoard(boardSize))
          .foldLeft(updatedBotBoardMarks) { case (upBoardMarks, coor) =>
            getMark(upBoardMarks, coor) match {
              case Empty =>
                forceSetBoardMark(upBoardMarks, coor, ShipOrWater(shipIds))
              case Water =>
                upBoardMarks
              case ShipOrWater(currentShipIds) =>
                forceSetBoardMark(upBoardMarks, coor, shipOrWater(currentShipIds.filter(shipIds)))
              case ShipExclusive(currentShipIds) =>
                forceSetBoardMark(upBoardMarks, coor, ShipExclusive(currentShipIds.filter(shipIds)))
            }
          }
      } else
        updatedBotBoardMarks

    // Update board marks with destroyed ships positions (with only 1 possibility)
    val updatedBotBoardMarks3: BotBoardMarks =
      hitHints.flatMap(_.shipIdDestroyedOpt).foldLeft(updatedBotBoardMarks2) {
        case (botBoardMarks, destroyedShipId) =>
          val shipGuesser = allShipGuessers(destroyedShipId)
          val shipGuesses = shipGuesser.checkPossiblePositions(botBoardMarks)
          val possibleShipPositionsSet: Set[List[List[Coordinate]]] =
            shipGuesser.possibleShipPositions(botBoardMarks, shipGuesses)

          if (possibleShipPositionsSet.sizeIs != 1)
            botBoardMarks
          else {
            val shipPos: Set[Coordinate] =
              possibleShipPositionsSet.flatten.flatten
            val water: Set[Coordinate] =
              (shipPos.flatMap(_.get8CoorAround) -- shipPos).filter(_.isInsideBoard(boardSize))
            val outsideTurnCoordinates: Set[Coordinate] =
              shipGuesser.getTurnsWithShip.flatMap { case TurnPlay(_, turnAttacks, _) =>
                turnAttacks.flatMap(_.coordinateOpt)
              }.toSet -- shipPos -- water
            val updates: Set[(Coordinate, BotBoardMark)] =
              shipPos.map(_ -> BotBoardMark.ShipExclusive(Set(destroyedShipId))) ++
                water.map(_ -> (BotBoardMark.Water: BotBoardMark)) ++
                outsideTurnCoordinates.flatMap { coor =>
                  getMark(botBoardMarks, coor) match {
                    case ShipOrWater(shipIds) =>
                      Some(coor -> shipOrWater(shipIds - destroyedShipId))
                    case ShipExclusive(shipIds) =>
                      Some(coor -> ShipExclusive(shipIds - destroyedShipId))
                    case _ =>
                      None
                  }
                }

            updates.foldLeft(botBoardMarks) { case (upBoardMarks, (coor, botBoardMark)) =>
              forceSetBoardMark(upBoardMarks, coor, botBoardMark)
            }
          }
      }

    logLine("printBotBoard1")
    logBotBoardMarks(boardSize, updatedBotBoardMarks3)

    cachedBotBoardMarks = updatedBotBoardMarks3
  }

  def placeAttacks(
      currentTurnAttackTypes: List[AttackType],
      randomSeed: Option[Long] = None
  ): List[Attack] = {
    val turnHistory = cachedTurnPlays
    val botBoardMarks = cachedBotBoardMarks

    val filteredCurrentTurnAttackTypes: List[AttackType] =
      currentTurnAttackTypes.filter(_ == AttackType.Simple)

    logBotGame(gameId = gameId, rules = rules, turnHistory = turnHistory)

    logLine("+" * 80)
    logLine("+" * 29 + "  " + turnHistory.map(_.turn).maxByOption(_.currentTurn) + "  " + "+" * 29)
    logLine("+" * 80)

    randomSeed.foreach(value => logLine(s"Using randomSeed = $value"))
    val random: Random = randomSeed.map(new Random(_)).getOrElse(Random)

    val (updatedBotBoardMarks, attackList) =
      smarterPlaceAttacks(botBoardMarks, filteredCurrentTurnAttackTypes, random)

    val validAttacks: List[Attack] =
      attackList.filter {
        case Attack(_, Some(coor)) =>
          getSquare(botBoardMarks, coor) match {
            case (None, botBoardMark) if botBoardMark != Water => true
            case _                                             => false
          }
        case _ =>
          false
      }

    cachedBotBoardMarks = updatedBotBoardMarks
    if (validAttacks.isEmpty) {
      logLine(s"smarterPlaceAttacks returned an invalid attack list: $attackList")
      shotAllRandom(botBoardMarks, filteredCurrentTurnAttackTypes, ignorePositions = Set.empty)
    } else if (
      validAttacks.map(_.attackType).groupBy(identity) ==
        filteredCurrentTurnAttackTypes.groupBy(identity)
    ) {
      validAttacks
    } else {
      validAttacks ++
        shotAllRandom(
          botBoardMarks,
          filteredCurrentTurnAttackTypes.take(
            filteredCurrentTurnAttackTypes.size - validAttacks.size
          ),
          ignorePositions = validAttacks.flatMap(_.coordinateOpt).toSet
        )
    }
  }

  private def smarterPlaceAttacks(
      initialBoardMarks: BotBoardMarks,
      currentTurnAttackTypes: List[AttackType],
      random: Random
  ): (BotBoardMarks, List[Attack]) = {

    val (boardMarksUpdated, sureAttacks, otherAttacks) = {
      val initialShipGuessers: List[ShipGuesser] =
        allShipGuessers.values.toList

      @tailrec
      def calcAllResults(
          boardMarks: BotBoardMarks,
          boardWasUpdated: Boolean,
          shipGuessers: List[ShipGuesser],
          resultsSoFar: List[(Set[Coordinate], Set[Coordinate])]
      ): (Option[BotBoardMarks], List[Set[Coordinate]], List[Set[Coordinate]]) =
        shipGuessers match {
          case Nil =>
            val (shotsList1, shotsList2) = resultsSoFar.unzip
            (Some(boardMarks).filter(_ => boardWasUpdated), shotsList1, shotsList2)
          case headGuesser :: nextGuessers =>
            logLine("-" * 50)
            logLine(s"${Ship.shipsNamesMap(headGuesser.shipId)}:")
            headGuesser.getBestShots(boardMarks, currentTurnAttackTypes) match {
              case (Some(updatedBoardMarks), shotsList1, shotsList2) if resultsSoFar.isEmpty =>
                calcAllResults(
                  boardMarks = updatedBoardMarks,
                  boardWasUpdated = true,
                  shipGuessers = nextGuessers,
                  resultsSoFar = (shotsList1, shotsList2) :: resultsSoFar
                )
              case (Some(updatedBoardMarks), _, _) =>
                calcAllResults(
                  boardMarks = updatedBoardMarks,
                  boardWasUpdated = true,
                  shipGuessers = initialShipGuessers,
                  resultsSoFar = Nil
                )
              case (None, shotsList1, shotsList2) =>
                calcAllResults(
                  boardMarks = boardMarks,
                  boardWasUpdated = boardWasUpdated,
                  shipGuessers = nextGuessers,
                  resultsSoFar = (shotsList1, shotsList2) :: resultsSoFar
                )
            }
        }

      calcAllResults(
        boardMarks = initialBoardMarks,
        boardWasUpdated = false,
        shipGuessers = initialShipGuessers,
        resultsSoFar = Nil
      )
    }

    boardMarksUpdated.foreach { boardMarks =>
      logLine("printBotBoard4")
      logBotBoardMarks(boardSize, boardMarks)
    }

    val attacks: List[Attack] = {
      val maximumShots = currentTurnAttackTypes.size

      @tailrec
      def getShots(possibleCoor: List[Coordinate], shotsSoFar: Set[Coordinate]): Set[Coordinate] =
        possibleCoor match {
          case Nil =>
            shotsSoFar
          case _ if shotsSoFar.size == maximumShots =>
            shotsSoFar
          case headCoor :: next =>
            getShots(next, shotsSoFar + headCoor)
        }

      val zipped = sureAttacks.zip(otherAttacks)
      val shuffledSure = random.shuffle(sureAttacks.flatten.distinct)
      val shuffledOther = random.shuffle(otherAttacks.flatten.distinct)

      val shotsOpt: Option[Iterable[Coordinate]] =
        if (maximumShots == 1) {
          zipped.find { case (sure, tent) => sure.isEmpty && tent.nonEmpty } match {
            case None =>
              Some((shuffledOther ++ shuffledSure).take(1))
            case Some((_, tent)) =>
              Some(tent.take(1))
          }
        } else if (maximumShots == 2) {
          if (shuffledSure.nonEmpty && shuffledOther.nonEmpty)
            Some(shuffledSure.take(1) ++ shuffledOther.take(1))
          else if (shuffledOther.sizeIs >= 2)
            otherAttacks.filter(_.nonEmpty) match {
              case List(_) if shuffledSure.sizeIs >= 2 =>
                Some(shuffledOther.take(2))
              case one :: two :: _ =>
                val oneDistinctCoors = one -- two
                val twoDistinctCoors = two -- one
                if (oneDistinctCoors.nonEmpty && twoDistinctCoors.nonEmpty)
                  Some(oneDistinctCoors.take(1) ++ twoDistinctCoors.take(1))
                else
                  Some(one.take(1) ++ two.take(1))
              case _ =>
                None
            }
          else
            None
        } else if (maximumShots == 3) {
          if (standardTripleKill && sureAttacks.count(_.sizeIs == 1) >= 3) {
            Some(sureAttacks.filter(_.sizeIs == 1).take(3).flatten)
          } else if (shuffledSure.nonEmpty && shuffledOther.nonEmpty) {
            otherAttacks.filter(_.nonEmpty) match {
              case List(_) if shuffledSure.sizeIs >= 2 =>
                Some(shuffledSure.take(2) ++ shuffledOther.take(1))
              case one :: two :: _ =>
                val oneDistinctCoors = one -- two
                val twoDistinctCoors = two -- one
                if (oneDistinctCoors.nonEmpty && twoDistinctCoors.nonEmpty)
                  Some(shuffledSure.take(1) ++ oneDistinctCoors.take(1) ++ twoDistinctCoors.take(1))
                else
                  Some(shuffledSure.take(1) ++ one.take(1) ++ two.take(1))
              case _ =>
                Some(shuffledSure.take(1) ++ shuffledOther.take(2))
            }

          } else if (shuffledSure.nonEmpty)
            Some(shuffledSure.take(3))
          else
            None
        } else {
          None
        }

      val finalShots =
        shotsOpt match {
          case Some(shots) if shots.toSet.sizeIs == maximumShots =>
            shots.toList
          case _ =>
            shuffledSure ++ shuffledOther
        }

      getShots(finalShots, Set.empty)
        .map(coor => Attack(AttackType.Simple, Some(coor)))
        .toList
    }
    (boardMarksUpdated.getOrElse(initialBoardMarks), attacks)
  }

  private def shotAllRandom(
      boardMarks: BotBoardMarks,
      currentTurnAttackTypes: List[AttackType],
      ignorePositions: Set[Coordinate]
  ): List[Attack] = {
    def possibleCoorList(excludeWater: Boolean): Seq[Coordinate] =
      for {
        x <- 0 until boardSize.x
        y <- 0 until boardSize.y
        if {
          val (turnOpt, botBoardMark) = boardMarks(x)(y)
          turnOpt.isEmpty && (!excludeWater || botBoardMark != Water)
        }
      } yield Coordinate(x, y)

    val standardRandomShots: Seq[Attack] =
      Random
        .shuffle(possibleCoorList(excludeWater = true))
        .filterNot(ignorePositions)
        .zip(currentTurnAttackTypes)
        .map { case (coor, attackType) => Attack(attackType, Some(coor)) }

    if (currentTurnAttackTypes.sizeIs == standardRandomShots.size)
      standardRandomShots.toList
    else {
      val standardRandomShotsSet: Set[Coordinate] =
        standardRandomShots.flatMap(_.coordinateOpt).toSet

      val remainingTurnAttackTypes = currentTurnAttackTypes.drop(standardRandomShots.size)

      standardRandomShots.toList ++
        Random
          .shuffle(possibleCoorList(excludeWater = false))
          .filterNot(ignorePositions)
          .filterNot(standardRandomShotsSet)
          .take(remainingTurnAttackTypes.size)
          .zip(remainingTurnAttackTypes)
          .map { case (coor, attackType) => Attack(attackType, Some(coor)) }
          .toList
    }
  }

}

object BotHelper {

  sealed trait ShipGuess

  case class SingleShipGuess(positions: List[Coordinate]) extends ShipGuess

  case class MultipleShipGuess(positions: List[List[Coordinate]]) extends ShipGuess

  sealed trait BotBoardMark {

    def isShipOrWater: Boolean = this match {
      case ShipOrWater(_) => true
      case _              => false
    }

    def isExclusive: Boolean = this match {
      case ShipExclusive(_) => true
      case _                => false
    }

  }

  object BotBoardMark {

    case object Empty extends BotBoardMark

    case object Water extends BotBoardMark

    case class ShipOrWater(shipIds: Set[ShipId]) extends BotBoardMark

    case class ShipExclusive(shipIds: Set[ShipId]) extends BotBoardMark

    def shipOrWater(shipIds: Set[ShipId]): BotBoardMark =
      if (shipIds.isEmpty) Water else ShipOrWater(shipIds)

  }

  type BotBoardMarks = Vector[Vector[(Option[Turn], BotBoardMark)]]

  def createEmptyBotBoardMarks(boardSize: Coordinate): BotBoardMarks =
    Vector.fill(boardSize.x)(Vector.fill(boardSize.y)((None, Empty)))

  def getSquare(
      botBoardMarks: BotBoardMarks,
      coordinate: Coordinate
  ): (Option[Turn], BotBoardMark) =
    botBoardMarks(coordinate.x)(coordinate.y)

  def getTurn(botBoardMarks: BotBoardMarks, coordinate: Coordinate): Option[Turn] =
    botBoardMarks(coordinate.x)(coordinate.y)._1

  def getMark(botBoardMarks: BotBoardMarks, coordinate: Coordinate): BotBoardMark =
    botBoardMarks(coordinate.x)(coordinate.y)._2

  def forceSetBoardMark(
      botBoardMarks: BotBoardMarks,
      coordinate: Coordinate,
      botBoardMark: BotBoardMark
  ): BotBoardMarks =
    updateBoardMarksUsing(
      botBoardMarks,
      coordinate,
      { case (turnOpt, _) => (turnOpt, botBoardMark) }
    )

  private def updateBoardMarksUsing(
      botBoardMarks: BotBoardMarks,
      coordinate: Coordinate,
      f: PartialFunction[(Option[Turn], BotBoardMark), (Option[Turn], BotBoardMark)]
  ): BotBoardMarks = {
    val vectorX: Vector[(Option[Turn], BotBoardMark)] = botBoardMarks(coordinate.x)
    val current = vectorX(coordinate.y)
    botBoardMarks.updated(
      coordinate.x,
      vectorX.updated(
        coordinate.y,
        f.applyOrElse(current, (_: (Option[Turn], BotBoardMark)) => current)
      )
    )
  }

  def placeShipsAtRandom(
      boardSize: Coordinate,
      shipsToPlace: List[Ship],
      randomSeed: Option[Long] = None
  ): Option[List[ShipInBoard]] = {
    def canPlaceInBoard(
        shipsPlacedSoFar: List[ShipInBoard],
        shipToPlace: Ship,
        boardCoor: Coordinate
    ): Boolean = {
      val actualPiecePositions = shipToPlace.pieces.map(_ + boardCoor)

      actualPiecePositions.forall(_.isInsideBoard(boardSize)) &&
      !actualPiecePositions
        .exists(coor => shipsPlacedSoFar.exists(_.shipActualPieces.exists(_.distance(coor) <= 1)))
    }

    val possibleCoorList: Seq[(Coordinate, Rotation)] =
      for {
        x <- 0 until boardSize.x
        y <- 0 until boardSize.y
        rotation <- Rotation.all
      } yield (Coordinate(x, y), rotation)

    val random: Random = randomSeed.map(new Random(_)).getOrElse(Random)

    @tailrec
    def loopPlaceAllShips(
        ships: List[Ship],
        shipsPlacedSoFar: List[ShipInBoard]
    ): Option[List[ShipInBoard]] = {
      ships match {
        case Nil =>
          Some(shipsPlacedSoFar)
        case headShip :: nextShips =>
          val result: Option[ShipInBoard] =
            LazyList
              .from(random.shuffle(possibleCoorList))
              .map { case (coor, rotation) => (coor, headShip.rotateTo(rotation)) }
              .find { case (coor, shipWithRotation) =>
                canPlaceInBoard(shipsPlacedSoFar, shipWithRotation, coor)
              }
              .map { case (coor, shipWithRotation) => ShipInBoard(shipWithRotation, coor) }

          result match {
            case None =>
              None
            case Some(placedShip) =>
              loopPlaceAllShips(nextShips, placedShip :: shipsPlacedSoFar)
          }
      }
    }

    loopPlaceAllShips(shipsToPlace.sortBy(_.shipBiggestToSmallestOrder), Nil)
  }

}
