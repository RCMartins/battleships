package pt.rmartins.battleships.backend.services

import pt.rmartins.battleships.backend.services.BotHelper.BotBoardMark._
import pt.rmartins.battleships.backend.services.BotHelper._
import pt.rmartins.battleships.shared.model.game._

import scala.annotation.tailrec
import scala.util.Random

class BotHelper(gameId: GameId, val rules: Rules, logger: BotHelperLogger) {

  import logger._

  private val MaxShipSamples = 1000

  private val boardSize: Coordinate = rules.boardSize

  private val allShipGuessers: Map[ShipId, ShipGuesser] = {
    val allShipIds = rules.gameFleet.shipsCounter.keys.toList
    allShipIds.map(shipId => shipId -> new ShipGuesser(shipId)).toMap
  }

  private var cachedTurnPlays: List[TurnPlay] =
    Nil
  private var cachedBotBoardMarks: BotBoardMarks =
    Vector.fill(boardSize.x)(Vector.fill(boardSize.y)((None, Empty)))

  private val standardTripleKill: Boolean =
    rules.turnBonuses.exists(turnBonus =>
      turnBonus.bonusType == BonusType.TripleKill &&
        turnBonus.bonusReward.contains(BonusReward.ExtraTurn(List.fill(3)(AttackType.Simple)))
    )

  private def getSquare(
      botBoardMarks: BotBoardMarks,
      coordinate: Coordinate
  ): (Option[Turn], BotBoardMark) =
    botBoardMarks(coordinate.x)(coordinate.y)

  private def getTurn(botBoardMarks: BotBoardMarks, coordinate: Coordinate): Option[Turn] =
    botBoardMarks(coordinate.x)(coordinate.y)._1

  private def getMark(botBoardMarks: BotBoardMarks, coordinate: Coordinate): BotBoardMark =
    botBoardMarks(coordinate.x)(coordinate.y)._2

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

  private def forceSetBoardMark(
      botBoardMarks: BotBoardMarks,
      coordinate: Coordinate,
      botBoardMark: BotBoardMark
  ): BotBoardMarks =
    updateBoardMarksUsing(
      botBoardMarks,
      coordinate,
      { case (turnOpt, _) => (turnOpt, botBoardMark) }
    )

  private def forceSetBoardMarkUpdated(
      botBoardMarks: BotBoardMarks,
      coordinate: Coordinate,
      botBoardMark: BotBoardMark
  ): (BotBoardMarks, Boolean) = {
    val vectorX: Vector[(Option[Turn], BotBoardMark)] = botBoardMarks(coordinate.x)
    val (turnOpt, currentMark) = vectorX(coordinate.y)
    if (currentMark == botBoardMark)
      (botBoardMarks, false)
    else
      (
        botBoardMarks.updated(coordinate.x, vectorX.updated(coordinate.y, (turnOpt, botBoardMark))),
        true
      )
  }

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

    logLine("printBotBoard1")
    logBotBoardMarks(boardSize, updatedBotBoardMarks2)

    //TODO check destroyed ships to update positions + water

    cachedBotBoardMarks = updatedBotBoardMarks2
  }

  private class ShipGuesser(val shipId: ShipId) {

    private val shipsCounter: Int = rules.gameFleet.shipsCounter(shipId)
    private val uniqueRotations = Ship.allShipsUniqueRotations(shipId)

    private var turnsWithShip: List[TurnPlay] = Nil

    def updateTurnPlay(turnPlay: TurnPlay): Unit =
      if (turnPlay.hitHints.exists(_.shipIdOpt.contains(shipId)))
        turnsWithShip = turnPlay :: turnsWithShip

    def baseShipsHit(boardMarks: BotBoardMarks): Option[List[ShipGuess]] = {
      val destroyedCount: Int =
        turnsWithShip.map { case TurnPlay(_, _, hitHints) =>
          hitHints.count(_.shipIdDestroyedOpt.contains(shipId))
        }.sum
      val aliveShips = shipsCounter - destroyedCount

      if (aliveShips == 0)
        None
      else {
        val possiblePositionsList: List[(List[HitHint], List[Coordinate])] =
          turnsWithShip
            .map { case TurnPlay(_, turnAttacks, hitHints) =>
              (
                hitHints,
                turnAttacks
                  .filter(_.attackType == AttackType.Simple)
                  .flatMap(_.coordinateOpt)
                  .filter { coor =>
                    getMark(boardMarks, coor) match {
                      case Empty                  => true
                      case Water                  => false
                      case ShipOrWater(shipIds)   => shipIds.contains(shipId)
                      case ShipExclusive(shipIds) => shipIds.contains(shipId)
                    }
                  }
              )
            }
            .filter(_._2.nonEmpty)

        Some(
          if (shipsCounter == 1) {
            @tailrec
            def filterLoop(
                remainingPositionsList: List[(List[HitHint], List[Coordinate])],
                currentPossibilitiesOpt: Option[List[List[Coordinate]]]
            ): List[List[Coordinate]] =
              remainingPositionsList match {
                case Nil =>
//                  logLine("currentPossibilitiesOpt:")
//                  logLine(currentPossibilitiesOpt.getOrElse(Nil).mkString("\n"))
                  currentPossibilitiesOpt.getOrElse(Nil)
                case (hitHints, coordinates) :: next =>
                  val amount = hitHints.flatMap(_.shipIdOpt).count(_ == shipId)
                  val comb = Utils.combinations(coordinates, amount = amount)
//                  logLine(s"comb ($coordinates -> $amount:")
//                  logLine(comb.mkString("\n"))
//                  logLine("+" * 50)
                  currentPossibilitiesOpt match {
                    case None =>
                      filterLoop(next, Some(comb))
                    case Some(currentPossibilities) =>
                      filterLoop(
                        next,
                        Some(
                          currentPossibilities.flatMap { list =>
                            comb.map(list ++ _)
                          }
                        )
                      )
                  }
              }

            if (possiblePositionsList.nonEmpty) {
              logLine(s"possiblePositionsList: ${possiblePositionsList.size}")
              logLine(possiblePositionsList.mkString("\n"))
            }

            filterLoop(possiblePositionsList, None).map(ShipGuess)
          } else
            possiblePositionsList.flatMap(_._2).map(coor => ShipGuess(List(coor)))
        )
      }
    }

    private val getBlindShipPositions: (BotBoardMarks, Int) => LazyList[(Ship, Coordinate)] = {
      var cachePositions: LazyList[(Ship, Coordinate)] =
        LazyList.from(
          Random.shuffle(
            for {
              rotation <- Rotation.all
              ship = Ship.getShip(shipId, rotation)
              x <- 0 to boardSize.x - ship.size.x
              y <- 0 to boardSize.y - ship.size.y
            } yield (ship, Coordinate(x, y))
          )
        )

      (boardMarks: BotBoardMarks, amount: Int) => {
        cachePositions
          .filter { case (ship, coordinate) =>
            shipIsPossible(shipId, ship.pieces.map(_ + coordinate), boardMarks, turnsWithShip)
          }
          .take(amount)
      }
    }

    /* Option[BotBoardMarks] -> Some(BotBoardMarks) or None if there was no update
     * List[Coordinate]   -> known 100% shots
     * List[Coordinate]   -> tentative shots
     */
    def getBestShots(
        botBoardMarks: BotBoardMarks,
        currentTurnAttackTypes: List[AttackType]
    ): (Option[BotBoardMarks], Set[Coordinate], Set[Coordinate]) = {

      def getSimplePositions(guessCoor: Coordinate): List[List[Coordinate]] =
        uniqueRotations.flatMap { ship =>
          ship.pieces.flatMap { shipPiece =>
            val diffDist = guessCoor - shipPiece
            List(ship.pieces.map(_ + diffDist))
              .filter(shipIsPossible(shipId, _, botBoardMarks, cachedTurnPlays))
          }
        }

      def processShipGuesses(
          shipGuesses: List[ShipGuess]
      ): (Option[BotBoardMarks], Set[Coordinate], Set[Coordinate]) = {
        val possibleShipPositions: Set[List[Coordinate]] =
          shipGuesses.flatMap {
            case ShipGuess(headPosition :: other) =>
              val otherPositionsSet = other.toSet
              getSimplePositions(headPosition).filter { shipPieces =>
                val shipPiecesSet = shipPieces.toSet
                otherPositionsSet.forall(shipPiecesSet)
              }
            case _ => // impossible...
              Nil
          }.toSet

        if (possibleShipPositions.isEmpty) {
          logLine("Bug! possibleShipPositions is empty!")
          logLine("shipGuesses:")
          logLine(shipGuesses.mkString("\n"))
          (None, Set.empty, Set.empty)
        } else
          processShipPositions(possibleShipPositions, allShipPossiblePositions = true)
      }

      def processShipPositions(
          possibleShipPositions: Set[List[Coordinate]],
          allShipPossiblePositions: Boolean
      ): (Option[BotBoardMarks], Set[Coordinate], Set[Coordinate]) = {
        logLine(s"possibleShipPositions: ${possibleShipPositions.size}")
        if (possibleShipPositions.size <= 15)
          logLine(possibleShipPositions.mkString("\n"))

        val allKnownWater = {
          val waterListList: List[Set[Coordinate]] =
            possibleShipPositions.toList
              .map(guessPositions =>
                guessPositions.flatMap(_.get8CoorAround).toSet -- guessPositions.toSet
              )

          val all = waterListList.flatten.distinct
          all.filter { possibleWaterCoor =>
            waterListList.forall(_(possibleWaterCoor)) &&
            possibleWaterCoor.isInsideBoard(boardSize)
          }
        }

        val newWaterFound: List[Coordinate] =
          if (allShipPossiblePositions)
            allKnownWater.filter(coor => getMark(botBoardMarks, coor) != Water)
          else
            Nil

        if (newWaterFound.nonEmpty) {
          logLine("All new known water coordinates:")
          logLine(newWaterFound.mkString("\n"))
        }

        val updatedBoardMarks: BotBoardMarks =
          newWaterFound.foldLeft(botBoardMarks) { case (upBoardMarks, coor) =>
            forceSetBoardMark(upBoardMarks, coor, Water)
          }

        val maximumAttacksNecessary = currentTurnAttackTypes.size

        val coordinatesInAllShipPositions: Set[Coordinate] =
          possibleShipPositions.flatten
            .filter(coor =>
              getSquare(updatedBoardMarks, coor) match {
                case (_, botBoardMark) if botBoardMark != Water => true
                case _                                          => false
              }
            )

        val (sureAttacks, otherAttacks) = {
          val all: Set[Coordinate] =
            coordinatesInAllShipPositions
              .filter(getTurn(updatedBoardMarks, _).isEmpty)

          val coordinatesInAllGuesses: Set[Coordinate] =
            all
              .filter(coor => possibleShipPositions.forall(_.contains(coor)))
              .take(maximumAttacksNecessary)

          (
            coordinatesInAllGuesses,
            (all -- coordinatesInAllGuesses).take(maximumAttacksNecessary)
          )
        }

        val updateShipCoordinates: Set[(Coordinate, BotBoardMark)] = {
          val sureMarks =
            sureAttacks ++
              (if (possibleShipPositions.sizeIs == 1) possibleShipPositions.flatten else Set.empty)

          val aroundMarks =
            (sureMarks.flatMap(_.get8CoorAround) -- sureMarks).filter(_.isInsideBoard(boardSize))

          sureMarks.map(_ -> (ShipExclusive(Set(shipId)): BotBoardMark)) ++
            aroundMarks.map(_ -> (ShipOrWater(Set(shipId)): BotBoardMark))
        }

        if (updateShipCoordinates.nonEmpty) {
          val exclusive = updateShipCoordinates.filter(_._2.isExclusive)
          val shipOrWater = updateShipCoordinates.filter(_._2.isShipOrWater)
          if (exclusive.nonEmpty) {
            logLine("updateShipCoordinates Exclusive:")
            logLine(
              exclusive
                .map { case (coor, mark) =>
                  s"  $coor-$mark  ${getTurn(updatedBoardMarks, coor).nonEmpty}"
                }
                .mkString("\n")
            )
          }
          if (shipOrWater.nonEmpty) {
            logLine("updateShipCoordinates ShipOrWater:")
            logLine(
              shipOrWater
                .map { case (coor, mark) =>
                  s"  $coor-$mark  ${getTurn(updatedBoardMarks, coor).nonEmpty}"
                }
                .mkString("\n")
            )
          }
        }

        val (updatedBotBoardMarks2, marksUpdated2) =
          updateShipCoordinates
            .filter(_._1.isInsideBoard(boardSize))
            .foldLeft((updatedBoardMarks, false)) { case ((upBoardMarks, updated), (coor, mark)) =>
              val markOpt: Option[BotBoardMark] =
                (getMark(upBoardMarks, coor), mark) match {
                  case (Empty, _) | (_, ShipExclusive(_)) =>
                    Some(mark)
                  case (ShipOrWater(currentShipIds), ShipOrWater(markShipIds)) =>
                    Some(shipOrWater(currentShipIds.filter(markShipIds)))
                  case (ShipExclusive(currentShipIds), ShipOrWater(markShipIds)) =>
                    Some(ShipExclusive(currentShipIds.filter(markShipIds)))
                  case (_, _) =>
                    None
                }

              markOpt match {
                case Some(mark) =>
                  val (finalBoardMarks, updated2) =
                    forceSetBoardMarkUpdated(upBoardMarks, coor, mark)
                  (finalBoardMarks, updated || updated2)
                case None =>
                  (upBoardMarks, updated)
              }
            }

        if (newWaterFound.nonEmpty || marksUpdated2) {
          logLine("printBotBoard2")
          logBotBoardMarks(boardSize, updatedBotBoardMarks2)
        }

        val (updatedBotBoardMarks3, marksUpdated3) =
          if (allShipPossiblePositions)
            turnsWithShip.foldLeft((updatedBotBoardMarks2, false)) {
              case ((upBoardMarks, updated), TurnPlay(_, turnAttacks, hitHints)) =>
                def findAllWater(
                    coordinates: Set[Coordinate],
                    shipIds: List[ShipId]
                ): Set[Coordinate] = {
                  shipIds match {
                    case Nil =>
                      coordinates
                    case shipId :: next =>
                      coordinates
                        .map { coor =>
                          getMark(upBoardMarks, coor) match {
                            case ShipExclusive(markShipIds) if markShipIds == Set(shipId) =>
                              findAllWater(coordinates - coor, next)
                            case _ =>
                              Set.empty[Coordinate]
                          }
                        }
                        .find(_.nonEmpty)
                        .getOrElse(Set.empty[Coordinate])
                  }
                }

                if (hitHints.exists(_.isWater)) {
                  val result =
                    findAllWater(
                      turnAttacks.flatMap(_.coordinateOpt).toSet,
                      hitHints.flatMap(_.shipIdOpt)
                    ).filter(getMark(upBoardMarks, _) match {
                      case Water => false
                      case _     => true
                    })

                  if (result.nonEmpty)
                    (
                      result.foldLeft(upBoardMarks) { case (upBoardMarks2, coor) =>
                        forceSetBoardMark(upBoardMarks2, coor, Water)
                      },
                      true
                    )
                  else
                    (upBoardMarks, updated)
                } else
                  (upBoardMarks, updated)
            }
          else
            (updatedBotBoardMarks2, false)

        if (marksUpdated3) {
          logLine("printBotBoard3")
          logBotBoardMarks(boardSize, updatedBotBoardMarks3)
        }

        val finalMarksUpdated = newWaterFound.nonEmpty || marksUpdated2 || marksUpdated3
        (
          Some(updatedBotBoardMarks3).filter(_ => finalMarksUpdated),
          sureAttacks,
          otherAttacks
        )
      }

      baseShipsHit(botBoardMarks) match {
        case None =>
          (None, Set.empty, Set.empty)
        case Some(Nil) =>
          val possiblePositions = getBlindShipPositions(botBoardMarks, MaxShipSamples)

          val possibleShipPositions: Set[List[Coordinate]] =
            possiblePositions.map { case (ship, coor) =>
              ship.pieces.map(_ + coor)
            }.toSet

          processShipPositions(
            possibleShipPositions,
            allShipPossiblePositions = possiblePositions.sizeIs < MaxShipSamples
          )
        case Some(shipGuesses) =>
          processShipGuesses(shipGuesses)
      }
    }

  }

  def placeAttacks(
      currentTurnAttackTypes: List[AttackType],
      randomSeed: Option[Long] = None
  ): List[Attack] = {
    val turnHistory = cachedTurnPlays
    val botBoardMarks = cachedBotBoardMarks

    logBotGame(gameId = gameId, rules = rules, turnHistory = turnHistory)

    logLine("+" * 80)
    logLine("+" * 29 + "  " + turnHistory.map(_.turn).maxByOption(_.currentTurn) + "  " + "+" * 29)
    logLine("+" * 80)

    randomSeed.foreach(value => logLine(s"Using randomSeed = $value"))
    val random: Random = randomSeed.map(new Random(_)).getOrElse(Random)

    val (updatedBotBoardMarks, attackList) =
      smarterPlaceAttacks(botBoardMarks, currentTurnAttackTypes, random)

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
      shotAllRandom(botBoardMarks, currentTurnAttackTypes)
    } else if (validAttacks.sizeIs == currentTurnAttackTypes.size) {
      validAttacks
    } else {
      validAttacks ++
        shotAllRandom(
          botBoardMarks,
          currentTurnAttackTypes.take(currentTurnAttackTypes.size - validAttacks.size)
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
            logLine(s"${Ship.shipNames(headGuesser.shipId)}:")
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

  private def shipIsPossible(
      shipId: ShipId,
      shipPieces: List[Coordinate],
      botBoardMarks: BotBoardMarks,
      fullTurnHistory: List[TurnPlay]
  ): Boolean =
    shipPieces.forall { coor =>
      coor.isInsideBoard(boardSize) && {
        getSquare(botBoardMarks, coor) match {
          case (_, Empty)                  => true
          case (_, Water)                  => false
          case (_, ShipOrWater(shipIds))   => shipIds.contains(shipId)
          case (_, ShipExclusive(shipIds)) => shipIds.contains(shipId)
        }
      }
    } && {
      val turnsCounter: Map[Turn, Int] =
        shipPieces.flatMap(coor => getTurn(botBoardMarks, coor)).groupBy(identity).map {
          case (turn, list) => (turn, list.size)
        }

      //TODO create fullTurnHistoryMap to make this more efficient
      fullTurnHistory.forall { case TurnPlay(turn, _, hitHints) =>
        val hitShipIds = hitHints.flatMap(_.shipIdOpt)
        val thisTurnShipCounter = hitShipIds.count(_ == shipId)

        turnsCounter.get(turn) match {
          case None =>
            0 == thisTurnShipCounter
          case Some(shipTurnCount) =>
            shipTurnCount == thisTurnShipCounter
        }
      }
    }

  private def shotAllRandom(
      boardMarks: BotBoardMarks,
      currentTurnAttackTypes: List[AttackType]
  ): List[Attack] = {
    val possibleCoorList: Seq[Coordinate] =
      for {
        x <- 0 until boardSize.x
        y <- 0 until boardSize.y
        if {
          val (turnOpt, botBoardMark) = boardMarks(x)(y)
          turnOpt.isEmpty && botBoardMark != Water
        }
      } yield Coordinate(x, y)

    LazyList
      .from(Random.shuffle(possibleCoorList))
      .take(currentTurnAttackTypes.size)
      .zip(currentTurnAttackTypes)
      .map { case (coor, attackType) => Attack(attackType, Some(coor)) }
      .toList
  }

}

object BotHelper {

  case class ShipGuess(positions: List[Coordinate])

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

  def placeShipsAtRandom(
      boardSize: Coordinate,
      shipsToPlace: List[Ship],
      randomSeed: Option[Long] = None
  ): Either[Unit, List[ShipInBoard]] = {
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
    ): Either[Unit, List[ShipInBoard]] = {
      ships match {
        case Nil =>
          Right(shipsPlacedSoFar)
        case headShip :: nextShips =>
          val result: Option[ShipInBoard] =
            random
              .shuffle(possibleCoorList)
              .map { case (coor, rotation) => (coor, headShip.rotateTo(rotation)) }
              .find { case (coor, shipWithRotation) =>
                canPlaceInBoard(shipsPlacedSoFar, shipWithRotation, coor)
              }
              .map { case (coor, shipWithRotation) => ShipInBoard(shipWithRotation, coor) }

          result match {
            case None =>
              Left(())
            case Some(placedShip) =>
              loopPlaceAllShips(nextShips, placedShip :: shipsPlacedSoFar)
          }
      }
    }

    loopPlaceAllShips(shipsToPlace, Nil)
  }

}
