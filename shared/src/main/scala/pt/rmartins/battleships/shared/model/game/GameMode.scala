package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait GameMode {

  def isPreGame: Boolean = false

  def isInGame: Boolean = false

  def isEndGame: Boolean = false

}

object GameMode extends HasGenCodec[GameMode] {

  case class PreGameMode(
      shipsToPlace: List[Ship],
      mePlacedShips: Boolean,
      enemyPlacedShips: Boolean
  ) extends GameMode {

    override def isPreGame: Boolean = true

  }

  case class InGameMode(isFirstPlayer: Boolean, halfTurns: Int, turnAttackTypes: List[AttackType])
      extends GameMode {

    override def isInGame: Boolean = true

    def isMyTurn: Boolean = if (isFirstPlayer) halfTurns % 2 == 1 else halfTurns % 2 == 0

  }

  case class GameOverMode(halfTurns: Int, youWon: Boolean) extends GameMode {

    override def isEndGame: Boolean = true

  }

}
