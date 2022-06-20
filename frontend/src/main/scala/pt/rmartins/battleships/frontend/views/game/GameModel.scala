package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.frontend.views.game.BoardView.GameAction
import pt.rmartins.battleships.frontend.views.model.AttacksQueuedStatus
import pt.rmartins.battleships.shared.model.game._

case class GameModel(
    shipsLeftToPlace: List[Ship],
    mousePosition: Option[Coordinate],
    mouseDown: Option[Int],
    selectedShip: Option[Ship],
    turnAttacks: List[Attack],
    turnAttacksQueuedStatus: AttacksQueuedStatus,
    selectedAction: GameAction,
    timeRemaining: Option[(TimeRemaining, TimeRemaining)],
    lineDashOffset: Int,
    marksPlacedHistory: List[Set[(Coordinate, BoardMark, BoardMark)]]
)

object GameModel extends HasModelPropertyCreator[GameModel] {

  val Default: GameModel =
    GameModel(
      shipsLeftToPlace = Nil,
      mousePosition = None,
      mouseDown = None,
      selectedShip = None,
      turnAttacks = Nil,
      turnAttacksQueuedStatus = AttacksQueuedStatus.NotSet,
      selectedAction = GameAction.ShotSelector,
      timeRemaining = None,
      lineDashOffset = 0,
      marksPlacedHistory = Nil
    )

}
