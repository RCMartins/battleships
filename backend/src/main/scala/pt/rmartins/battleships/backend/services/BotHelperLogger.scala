package pt.rmartins.battleships.backend.services

import better.files.File
import pt.rmartins.battleships.backend.services.BotHelper.BotBoardMark._
import pt.rmartins.battleships.backend.services.BotHelper.BotBoardMarks
import pt.rmartins.battleships.shared.model.game._

trait BotHelperLogger {

  def logLine(any: => Any): Unit

  def logBotBoardMarks(boardSize: Coordinate, botBoardMarks: BotBoardMarks): Unit

  def logBotGame(gameId: GameId, rules: Rules, turnHistory: List[TurnPlay]): Unit

}

object BotHelperLogger {

  object DefaultLogger extends BotHelperLogger {
    private val logFolder = File("log")

    def logLine(any: => Any): Unit =
      println(any)

    def logBotBoardMarks(boardSize: Coordinate, botBoardMarks: BotBoardMarks): Unit = {
      def idToChar(shipIds: Set[ShipId]): Set[String] =
        shipIds.map(id => (if (id.id < 10) '0' + id.id else 'A' + (id.id - 10)).toChar.toString)

      def printShipIds(ids: Set[String]): String = {
        val sorted = ids.toSeq.sorted
        if (sorted.size == 1)
          "   " + sorted.mkString(",") + "   "
        else if (sorted.size == 2)
          "  " + sorted.mkString(",") + "  "
        else if (sorted.size == 3)
          " " + sorted.mkString(",") + " "
        else
          sorted.mkString(",")
      }

      for (y <- 0 until boardSize.y) {
        val strList =
          for (x <- 0 until boardSize.x) yield {
            botBoardMarks(x)(y) match {
              case (_, Empty)                  => "       "
              case (_, Water)                  => "   .   "
              case (_, ShipOrWater(shipIds))   => printShipIds(idToChar(shipIds) + "W")
              case (_, ShipExclusive(shipIds)) => printShipIds(idToChar(shipIds))
            }
          }
        if (y == 0)
          println(strList.map("-" * _.length).mkString("-", "---", "-"))
        println(strList.mkString("|", " | ", "|"))
        if (y < boardSize.y - 1)
          println(strList.map("-" * _.length).mkString("|", "-|-", "|"))
        else
          println(strList.map("-" * _.length).mkString("-", "---", "-"))
      }

      //    for (y <- 0 until boardSize.y) {
      //      val strList =
      //        for (x <- 0 until boardSize.x) yield {
      //          botBoardMarks(x)(y) match {
      //            case (_, Empty)                  => "       "
      //            case (_, Water)                  => "   .   "
      //            case (_, ShipOrWater(shipIds))   => printShipIds(shipIds.map(_.id.toString) + "W")
      //            case (_, ShipExclusive(shipIds)) => printShipIds(shipIds.map(_.id.toString))
      //          }
      //        }
      //      if (y == 0)
      //        println(strList.map("\u2501" * _.length).mkString("\u250F", "\u2533", "\u2513"))
      //      println(strList.mkString("\u2503", "\u2503", "\u2503"))
      //      if (y < boardSize.y - 1)
      //        println(strList.map("\u2501" * _.length).mkString("\u2523", "\u254B", "\u252B"))
      //      else
      //        println(strList.map("\u2501" * _.length).mkString("\u2517", "\u253B", "\u251B"))
      //    }
    }

    def logBotGame(gameId: GameId, rules: Rules, turnHistory: List[TurnPlay]): Unit = {
      val sb = new StringBuilder()

      def coorToString(coor: Coordinate): String = s"(${coor.x}, ${coor.y})"

      if (turnHistory.nonEmpty)
        sb.append(
          turnHistory.reverse
            .map { case TurnPlay(turn, turnAttacks, hitHints) =>
              List(
                turn.currentTurn.toString,
                turn.extraTurn.toString,
                turnAttacks
                  .flatMap(_.coordinateOpt)
                  .map(coorToString)
                  .mkString("List(", ", ", ")"), {
                  val hits: List[String] =
                    hitHints.collect { case HitHint.ShipHit(shipId, destroyed) =>
                      s"(${Ship.shipNames(shipId)}, $destroyed)"
                    }
                  if (hits.isEmpty) "Nil" else hits.mkString("List(", ", ", ")")
                }
              ).mkString("turnPlay(", ", ", ")")
            }
            .map("  " + _)
            .mkString("List(\n", ",\n", "\n)")
        )

      val file = logFolder / s"${gameId.id}.log"

      if (file.exists) {
        file.append("\n\n\n")
        file.append(sb.result())
      } else {
        //gameFleet = Fleet.fromShips(List(Skeeter, Ranger, Conqueror)),
        file.append(s"boardSize = ${rules.boardSize.toCodeString}\n\n")
        file.append(
          s"gameFleet = Fleet.fromShips(List(${rules.gameFleet.ships.map(ship => Ship.shipNames(ship.shipId)).mkString(", ")}))\n\n"
        )
        file.append(sb.result())
      }
    }
  }

}
