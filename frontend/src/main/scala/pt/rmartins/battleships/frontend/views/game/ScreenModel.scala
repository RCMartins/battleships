package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import org.scalajs.dom.html.Span
import pt.rmartins.battleships.shared.model.game.{Coordinate, Turn}
import scalatags.JsDom.all._

case class ScreenModel(
    canvasSize: Coordinate,
    selectedTab: String,
    lastSeenMessagesChat: Int,
    lastSeenMessagesMyMoves: Int,
    lastSeenMessagesEnemyMoves: Int,
    missilesPopupMillisOpt: Option[Int],
    extraTurnPopup: Option[Int],
    extraTurnText: Span,
    hideMyBoard: Boolean,
    revealEnemyBoard: Boolean,
    showErrorModal: Boolean,
    myBoardTitle: Span,
    enemyBoardTitle: Span,
    realEnemyBoardTitle: Span,
    previewBoardTitle: Span,
    screenResized: Unit,
    showMissesMoves: Boolean,
    showDisabledMoves: Boolean,
    disabledMovesSet: Set[Turn]
)

object ScreenModel extends HasModelPropertyCreator[ScreenModel] {

  val chatTab: String = "chat"
  val myMovesTab: String = "my-moves"
  val enemyMovesTab: String = "enemy-moves"

  val default: ScreenModel =
    ScreenModel(
      canvasSize = BoardView.CanvasSize,
      selectedTab = myMovesTab,
      lastSeenMessagesChat = 0,
      lastSeenMessagesMyMoves = 0,
      lastSeenMessagesEnemyMoves = 0,
      missilesPopupMillisOpt = None,
      extraTurnPopup = None,
      extraTurnText = span.render,
      hideMyBoard = false,
      revealEnemyBoard = false,
      showErrorModal = false,
      myBoardTitle = span.render,
      enemyBoardTitle = span.render,
      realEnemyBoardTitle = span.render,
      previewBoardTitle = span.render,
      screenResized = (),
      showMissesMoves = true,
      showDisabledMoves = true,
      disabledMovesSet = Set.empty
    )

  def resetScreenModel(model: ScreenModel): ScreenModel = {
    model.copy(
      selectedTab = myMovesTab,
      lastSeenMessagesChat = 0,
      lastSeenMessagesMyMoves = 0,
      lastSeenMessagesEnemyMoves = 0,
      missilesPopupMillisOpt = None,
      extraTurnPopup = None,
      hideMyBoard = false,
      revealEnemyBoard = false,
      showErrorModal = false,
      disabledMovesSet = Set.empty
    )
  }

}
