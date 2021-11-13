package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.frontend.views.game.BoardView.InGameMarkSelector
import pt.rmartins.battleships.shared.model.game._

case class GameModel(
    mousePosition: Option[Coordinate],
    mouseDown: Option[Int],
    selectedShip: Option[Ship],
    turnAttacks: List[Attack],
    turnAttacksQueuedStatus: AttacksQueuedStatus,
    selectedInGameMarkOpt: Option[InGameMarkSelector],
    timeRemaining: Option[(TimeRemaining, TimeRemaining)],
    lineDashOffset: Int
)

object GameModel extends HasModelPropertyCreator[GameModel] {

  val default: GameModel =
    GameModel(
      mousePosition = None,
      mouseDown = None,
      selectedShip = None,
      turnAttacks = Nil,
      turnAttacksQueuedStatus = AttacksQueuedStatus.NotSet,
      selectedInGameMarkOpt = None,
      timeRemaining = None,
      lineDashOffset = 0
    )

}
