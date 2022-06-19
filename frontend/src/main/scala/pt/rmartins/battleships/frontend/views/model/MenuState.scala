package pt.rmartins.battleships.frontend.views.model

sealed trait MenuState

object MenuState {

  case object PlayingVsBots extends MenuState

  case object PlayingVsPlayer extends MenuState

}
