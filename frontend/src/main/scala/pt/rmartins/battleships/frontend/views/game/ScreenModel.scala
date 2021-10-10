package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import org.scalajs.dom.html.Span
import pt.rmartins.battleships.shared.model.game.Coordinate

case class ScreenModel(
    canvasSize: Coordinate,
    selectedTab: String,
    missilesPopupMillisOpt: Option[Int],
    extraTurnPopup: Option[Int],
    extraTurnText: Option[Span]
)

object ScreenModel extends HasModelPropertyCreator[ScreenModel] {

  val chatTab: String = "chat"
  val myMovesTab: String = "my-moves"
  val enemyMovesTab: String = "enemy-moves"

  val default: ScreenModel =
    ScreenModel(
      canvasSize = BoardView.CanvasSize,
      selectedTab = myMovesTab,
      missilesPopupMillisOpt = None,
      extraTurnPopup = None,
      extraTurnText = None
    )

}
