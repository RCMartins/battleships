package pt.rmartins.battleships.frontend.views.model

sealed trait MenuState

object MenuState {

  // TODO Nothing selected ???

  case object PlayingVsBots extends MenuState

  case object PlayingVsPlayer extends MenuState

  case object PlayingPuzzles extends MenuState

}
