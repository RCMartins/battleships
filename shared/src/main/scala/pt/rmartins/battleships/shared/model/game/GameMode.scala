package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait GameMode {

  def isPlacingShips: Boolean = false

  def isPlaying: Boolean = false

  def isEndGame: Boolean = false

}

object GameMode extends HasGenCodec[GameMode] {

  case class PlacingShipsMode(
      iPlacedShips: Boolean,
      enemyPlacedShips: Boolean
  ) extends GameMode {

    override def isPlacingShips: Boolean = true

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
