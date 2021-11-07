package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Turn(
    currentTurn: Int,
    extraTurn: Option[Int]
) {

  def >(otherTurn: Turn): Boolean =
    if (currentTurn == otherTurn.currentTurn)
      (extraTurn, otherTurn.extraTurn) match {
        case (Some(_), None) =>
          true
        case (Some(extraTurnNumber1), Some(extraTurnNumber2))
            if extraTurnNumber1 > extraTurnNumber2 =>
          true
        case _ =>
          false
      }
    else
      currentTurn > otherTurn.currentTurn

  def toTurnString: String =
    s"$currentTurn${extraTurn.map(extraTurnNumber => ('a' + extraTurnNumber - 1).toChar).getOrElse("")}"

}

object Turn extends HasGenCodec[Turn]
