package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait GameMode {

  def isPreGame: Boolean = false

  def isInGame: Boolean = false

}

object GameMode extends HasGenCodec[GameMode] {

  case class PreGameMode(shipsToPlace: List[Ship]) extends GameMode {
    override def isPreGame: Boolean = true
  }

  case class InGameMode(firstPlayerUsername: String, halfTurns: Int) extends GameMode {
    override def isInGame: Boolean = true
  }

}
