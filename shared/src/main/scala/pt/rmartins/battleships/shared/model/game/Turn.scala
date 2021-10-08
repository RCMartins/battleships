package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Turn(
    currentTurn: Int,
    extraTurn: Option[Int]
) {

  def toTurnString: String =
    s"$currentTurn${extraTurn.map(extraTurnNumber => ('a' + extraTurnNumber - 1).toChar).getOrElse("")}"

}

object Turn extends HasGenCodec[Turn]
