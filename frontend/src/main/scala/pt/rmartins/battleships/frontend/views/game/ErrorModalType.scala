package pt.rmartins.battleships.frontend.views.game

import pt.rmartins.battleships.shared.model.game.Username

sealed trait ErrorModalType

object ErrorModalType {

  case object SmallBoardError extends ErrorModalType

  case object EmptyFleetError extends ErrorModalType

  case object InviteItselfError extends ErrorModalType

  case class UsernameNotFound(username: Username) extends ErrorModalType

}
