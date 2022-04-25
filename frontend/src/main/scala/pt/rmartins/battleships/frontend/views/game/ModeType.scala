package pt.rmartins.battleships.frontend.views.game

sealed trait ModeType

object ModeType {

  case object PlacingGameModeType extends ModeType

  case object PlayingModeType extends ModeType

  case object GameOverModeType extends ModeType

}
