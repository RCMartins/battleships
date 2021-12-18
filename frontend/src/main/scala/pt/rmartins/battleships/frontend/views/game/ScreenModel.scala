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
    hideMyBoard: Boolean,
    revealEnemyBoard: Boolean,
    errorModalType: Option[ErrorModalType],
    screenResized: Unit,
    newTurn: Unit,
    showMissesMoves: Boolean,
    showDisabledMoves: Boolean,
    disabledMovesSet: Set[Turn],
    hoverMove: Option[Turn],
    tick: Int,
    receiveEditRequest: Option[Unit]
)

object ScreenModel extends HasModelPropertyCreator[ScreenModel] {

  val chatTab: String = "chat"
  val myMovesTab: String = "my-moves"
  val enemyMovesTab: String = "enemy-moves"

  val default: ScreenModel =
    ScreenModel(
      canvasSize = BoardView.CanvasSize,
      selectedTab = chatTab,
      lastSeenMessagesChat = 0,
      lastSeenMessagesMyMoves = 0,
      lastSeenMessagesEnemyMoves = 0,
      missilesPopupMillisOpt = None,
      extraTurnPopup = None,
      hideMyBoard = false,
      revealEnemyBoard = false,
      errorModalType = None,
      screenResized = (),
      newTurn = (),
      showMissesMoves = true,
      showDisabledMoves = true,
      disabledMovesSet = Set.empty,
      hoverMove = None,
      tick = 0,
      receiveEditRequest = None
    )

  def resetScreenModel(model: ScreenModel): ScreenModel = {
    model.copy(
      selectedTab = chatTab,
      lastSeenMessagesChat = 0,
      lastSeenMessagesMyMoves = 0,
      lastSeenMessagesEnemyMoves = 0,
      missilesPopupMillisOpt = None,
      extraTurnPopup = None,
      hideMyBoard = false,
      revealEnemyBoard = false,
      errorModalType = None,
      disabledMovesSet = Set.empty,
      hoverMove = None,
      tick = 0,
      receiveEditRequest = None
    )
  }

}
