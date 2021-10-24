package pt.rmartins.battleships.frontend.views.game

import io.udash.{HasModelPropertyCreator, Property}
import pt.rmartins.battleships.shared.model.game._

case class PreGameModel(
    enemyUsername: Username,
    boardSize: Coordinate,
    shipCounter: Map[ShipId, Property[Int]],
    previewBoardOpt: Option[(Board, Int)], // TODO change int to double 0.0 to 1.0,
    timeLimit: Option[RuleTimeLimit]
)

object PreGameModel extends HasModelPropertyCreator[PreGameModel] {

  val MaxPreviewTries: Int = 100
  val MinPreviewTriesPerc: Double = 0.15
  val WarningPreviewTriesPerc: Double = 0.75
  val MinPreviewTries: Int = Math.ceil(MinPreviewTriesPerc * MaxPreviewTries).toInt

  val default: PreGameModel =
    PreGameModel(
      enemyUsername = Username(""),
      boardSize = Coordinate(1, 1),
      shipCounter = Ship.allShipsList.map(_.shipId -> Property(0)).toMap,
      previewBoardOpt = None,
      timeLimit = None
    )

}
