package pt.rmartins.battleships.backend.services

import pt.rmartins.battleships.backend.services.BotHelper.BotBoardMark._
import pt.rmartins.battleships.backend.services.BotHelper._
import pt.rmartins.battleships.backend.services.ShipGuesser.MaxShipSamples
import pt.rmartins.battleships.shared.model.game._

import scala.annotation.tailrec
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps

case class ShipGuesser(rules: Rules, shipId: ShipId, botHelper: BotHelper) {

  import botHelper.logger._

  private val boardSize: Coordinate = rules.boardSize

  private val shipsCounter: Int =
    rules.gameFleet.shipCounterMap.get(shipId).map(_._1).getOrElse(0)

  private val uniqueRotations = Ship.allShipsUniqueRotations(shipId)

  private var turnsWithShip: List[TurnPlay] = Nil

  def getTurnsWithShip: List[TurnPlay] = turnsWithShip

  def updateTurnPlay(turnPlay: TurnPlay): Unit =
    if (turnPlay.hitHints.exists(_.shipIdOpt.contains(shipId)))
      turnsWithShip = turnPlay :: turnsWithShip

  def checkPossiblePositions(boardMarks: BotBoardMarks): Option[List[ShipGuess]] = {
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
        if (possiblePositionsList.nonEmpty) {
          logLine(s"possiblePositionsList: ${possiblePositionsList.size}")
          logLine(possiblePositionsList.mkString("\n"))
        }

        oneShipFilterLoop(possiblePositionsList, None).map(SingleShipGuess)
      } else {
        @tailrec
        def filterLoop(
            remainingPositionsList: List[(List[HitHint], List[Coordinate])],
            currentPossibilitiesByAmount: List[(Int, List[List[List[Coordinate]]])]
        ): List[List[List[Coordinate]]] =
          remainingPositionsList match {
            case Nil =>
              currentPossibilitiesByAmount.flatMap(_._2)
            case (hitHints, coordinates) :: next =>
              val totalAmount: Int = hitHints.flatMap(_.shipIdOpt).count(_ == shipId)
              val maxShips: Int =
                currentPossibilitiesByAmount
                  .maxByOption(_._1)
                  .map(_._1)
                  .getOrElse(0) + totalAmount

              /*
               * [List of different alternatives of [List of ships of [List of coordinates]]]
               */
              def shipsLoop(
                  nShips: Int
              ): Option[List[List[List[Coordinate]]]] = {
//                val amount: Int =
//                    if (nShips == 1)
//                      totalAmount
//                    else
//                  Math.min(totalAmount, nShips)

                if (nShips == 1) {
                  val comb: List[List[Coordinate]] =
                    Utils.combinations(coordinates, amount = totalAmount)
                  currentPossibilitiesByAmount.find(_._1 == 1) match {
                    case None =>
                      Some(comb.map(List(_))).tap(println)
                    case Some((_, currentPossibilities)) =>
                      Some(
                        currentPossibilities.map(
                          _.flatMap(list => comb.map(list ++ _))
                        )
                      )
                  }
                } else if (nShips == 2) {
                  val comb: List[List[List[Coordinate]]] =
                    Utils
                      .combinationGroups(coordinates, List(1, 1))
                      .map(_.map(_.toList).toList)
                      .toList

                  currentPossibilitiesByAmount.find(_._1 == 2) match {
                    case None =>
                      Some(comb).tap(println)
                    case Some((_, currentPossibilities)) =>
                      ???
                  }
                } else
                  ???
              }

              val updatedPossibilitiesByAmount: List[(Int, List[List[List[Coordinate]]])] =
                (1 to maxShips).toList.flatMap { nShips =>
                  shipsLoop(nShips).map((nShips, _)).tap(println)
                }

              filterLoop(next, updatedPossibilitiesByAmount)
          }

        if (possiblePositionsList.nonEmpty) {
          logLine(s"possiblePositionsList: ${possiblePositionsList.size}")
          logLine(possiblePositionsList.mkString("\n"))
        }

        filterLoop(possiblePositionsList, List.empty)
          .map {
            case List(oneList) => SingleShipGuess(oneList)
            case list          => MultipleShipGuess(list)
          }
          .tap(println)
      }
    )
  }

  def baseShipsHit(boardMarks: BotBoardMarks): Option[List[ShipGuess]] = {
    val destroyedCount: Int =
      turnsWithShip.map { case TurnPlay(_, _, hitHints) =>
        hitHints.count(_.shipIdDestroyedOpt.contains(shipId))
      }.sum
    val aliveShips = shipsCounter - destroyedCount

    if (aliveShips == 0)
      None
    else
      checkPossiblePositions(boardMarks)
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

  def possibleShipPositions(
      botBoardMarks: BotBoardMarks,
      shipGuesses: List[ShipGuess]
  ): Set[List[List[Coordinate]]] = {
    def getSimplePositions(guessCoor: Coordinate): List[List[Coordinate]] =
      uniqueRotations.flatMap { ship =>
        ship.pieces.flatMap { shipPiece =>
          val diffDist = guessCoor - shipPiece
          List(ship.pieces.map(_ + diffDist))
            .filter(shipIsPossible(shipId, _, botBoardMarks, botHelper.getCachedTurnPlays))
        }
      }

    shipGuesses.flatMap {
      case SingleShipGuess(headPosition :: other) =>
        val otherPositionsSet = other.toSet
        getSimplePositions(headPosition)
          .filter { shipPieces =>
            val shipPiecesSet = shipPieces.toSet
            otherPositionsSet.forall(shipPiecesSet)
          }
          .map(List(_))
      case SingleShipGuess(Nil) =>
        Nil
      case MultipleShipGuess(initialShips) =>
//        def loop(shipsPositions: List[List[Coordinate]]): List[List[Coordinate]] = {
//          shipsPositions match {
//            case Nil =>
//              ???
//            case head :: next =>
//              ???
//          }
//        }
//
//        loop(initialShips)

        Nil
    }.toSet
  }

  /* Option[BotBoardMarks] -> Some(BotBoardMarks) or None if there was no update
   * List[Coordinate]   -> known 100% shots
   * List[Coordinate]   -> tentative shots
   */
  def getBestShots(
      botBoardMarks: BotBoardMarks,
      currentTurnAttackTypes: List[AttackType]
  ): (Option[BotBoardMarks], Set[Coordinate], Set[Coordinate]) = {

    def processShipGuesses(
        shipGuesses: List[ShipGuess]
    ): (Option[BotBoardMarks], Set[Coordinate], Set[Coordinate]) = {
      val possibleShipPositionsSet: Set[List[List[Coordinate]]] =
        possibleShipPositions(botBoardMarks, shipGuesses)

      if (possibleShipPositionsSet.isEmpty) {
        logLine("Bug! possibleShipPositions is empty!")
        logLine("shipGuesses:")
        logLine(shipGuesses.mkString("\n"))
        (None, Set.empty, Set.empty)
      } else
        processShipPositions(possibleShipPositionsSet, allShipPossiblePositions = true)
    }

    def processShipPositions(
        possibleShipPositions: Set[List[List[Coordinate]]],
        allShipPossiblePositions: Boolean
    ): (Option[BotBoardMarks], Set[Coordinate], Set[Coordinate]) = {
      logLine(s"possibleShipPositions: ${possibleShipPositions.size}")
      if (possibleShipPositions.size <= 15)
        logLine(possibleShipPositions.mkString("\n"))

      val possibleShipPositionsFlatten: Set[Coordinate] =
        possibleShipPositions.flatten.flatten

      val allKnownWater = {
        val waterListList: List[Set[Coordinate]] =
          possibleShipPositions.toList.flatten.map(guessPositions =>
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
        possibleShipPositionsFlatten.filter(coor =>
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
            (if (possibleShipPositions.sizeIs == 1) possibleShipPositionsFlatten else Set.empty)

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

        val possibleShipPositions: Set[List[List[Coordinate]]] =
          possiblePositions
            .map { case (ship, coor) =>
              ship.pieces.map(_ + coor)
            }
            .map(List(_))
            .toSet

        processShipPositions(
          possibleShipPositions,
          allShipPossiblePositions = possiblePositions.sizeIs < MaxShipSamples
        )
      case Some(shipGuesses) =>
        processShipGuesses(shipGuesses)
    }
  }

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

      // TODO create fullTurnHistoryMap to make this more efficient
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

  @tailrec
  final def oneShipFilterLoop(
      remainingPositionsList: List[(List[HitHint], List[Coordinate])],
      currentPossibilitiesOpt: Option[List[List[Coordinate]]]
  ): List[List[Coordinate]] =
    remainingPositionsList match {
      case Nil =>
        currentPossibilitiesOpt.getOrElse(Nil)
      case (hitHints, coordinates) :: next =>
        val amount = hitHints.flatMap(_.shipIdOpt).count(_ == shipId)
        val comb = Utils.combinations(coordinates, amount = amount)
        currentPossibilitiesOpt match {
          case None =>
            oneShipFilterLoop(next, Some(comb))
          case Some(currentPossibilities) =>
            oneShipFilterLoop(
              next,
              Some(
                currentPossibilities.flatMap { list =>
                  comb.map(list ++ _)
                }
              )
            )
        }
    }

}

object ShipGuesser {

  private val MaxShipSamples = 1000

}
