package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait GameMode {

  def isPreGame: Boolean = false

  def isPlaying: Boolean = false

  def isEndGame: Boolean = false

}

object GameMode extends HasGenCodec[GameMode] {

  case class PreGameMode(
      iPlacedShips: Boolean,
      enemyPlacedShips: Boolean
  ) extends GameMode {

    override def isPreGame: Boolean = true

  }

  case class PlayingMode(
      isMyTurn: Boolean,
      turn: Turn,
      turnAttackTypes: List[AttackType],
      myTimeRemaining: Option[TimeRemaining],
      enemyTimeRemaining: Option[TimeRemaining]
  ) extends GameMode {

    override def isPlaying: Boolean = true

  }

  case class GameOverMode(
      turn: Turn,
      youWon: Boolean,
      myTimeRemaining: Option[TimeRemaining],
      enemyTimeRemaining: Option[TimeRemaining],
      enemyRealBoard: List[ShipInBoard]
  ) extends GameMode {

    override def isEndGame: Boolean = true

  }

}
