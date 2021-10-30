package pt.rmartins.battleships.frontend.views.game

sealed trait ErrorModalType

object ErrorModalType {

  case object SmallBoardError extends ErrorModalType

  case object EmptyFleetError extends ErrorModalType

}
