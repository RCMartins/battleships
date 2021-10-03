package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait GameMode {

  def isPreGame: Boolean = false

  def isInGame: Boolean = false

}

object GameMode extends HasGenCodec[GameMode] {

  case class PreGameMode(
      shipsToPlace: List[Ship],
      mePlacedShips: Boolean,
      enemyPlacedShips: Boolean
  ) extends GameMode {
    override def isPreGame: Boolean = true
  }

  case class InGameMode(isFirstPlayer: Boolean, halfTurns: Int, turnAttacks: List[Attack])
      extends GameMode {

    override def isInGame: Boolean = true

    def isMyTurn: Boolean = if (isFirstPlayer) halfTurns % 2 == 1 else halfTurns % 2 == 0

  }

}
