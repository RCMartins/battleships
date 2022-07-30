package pt.rmartins.battleships.frontend.views.game

import io.udash.{HasModelPropertyCreator, ModelProperty}
import pt.rmartins.battleships.frontend.views.model.ErrorModalType
import pt.rmartins.battleships.shared.model.game.{Coordinate, Turn}

case class ScreenModel(
    mainBoardCanvasSize: Coordinate,
    smallBoardCanvasSize: Coordinate,
    selectedTab: String,
    lastSeenMessagesChat: Int,
    lastSeenMessagesMyMoves: Int,
    lastSeenMessagesEnemyMoves: Int,
    hideMyBoard: Boolean,
    revealEnemyBoard: Boolean,
    errorModalType: Option[ErrorModalType],
    newTurn: Unit,
    showAllMoves: Boolean,
    showMissesMoves: Boolean,
    showDisabledMoves: Boolean,
    disabledMovesSet: Set[Turn],
    hoverMove: Option[Turn],
    tick: Int,
    receiveEditRequest: Option[Unit],
    namedRuleNameBefore: Option[String],
    namedRuleName: String
)

object ScreenModel extends HasModelPropertyCreator[ScreenModel] {

  val chatTab: String = "chat"
  val myMovesTab: String = "my-moves"
  val enemyMovesTab: String = "enemy-moves"

  val Default: ScreenModel =
    resetScreenModel(
      ScreenModel(
        mainBoardCanvasSize = Coordinate.square(1),
        smallBoardCanvasSize = Coordinate.square(1),
        selectedTab = chatTab,
        lastSeenMessagesChat = 0,
        lastSeenMessagesMyMoves = 0,
        lastSeenMessagesEnemyMoves = 0,
        hideMyBoard = false,
        revealEnemyBoard = false,
        errorModalType = None,
        newTurn = (),
        showAllMoves = true,
        showMissesMoves = true,
        showDisabledMoves = true,
        disabledMovesSet = Set.empty,
        hoverMove = None,
        tick = 0,
        receiveEditRequest = None,
        namedRuleNameBefore = None,
        namedRuleName = ""
      )
    )

  def resetScreenModel(model: ScreenModel): ScreenModel = {
    model.copy(
      mainBoardCanvasSize = Coordinate.square(300),
      smallBoardCanvasSize = Coordinate.square(100),
      selectedTab = chatTab,
      lastSeenMessagesChat = 0,
      lastSeenMessagesMyMoves = 0,
      lastSeenMessagesEnemyMoves = 0,
      hideMyBoard = false,
      revealEnemyBoard = false,
      errorModalType = None,
      disabledMovesSet = Set.empty,
      hoverMove = None,
      tick = 0,
      receiveEditRequest = None,
      namedRuleNameBefore = None,
      namedRuleName = ""
    )
  }

}
