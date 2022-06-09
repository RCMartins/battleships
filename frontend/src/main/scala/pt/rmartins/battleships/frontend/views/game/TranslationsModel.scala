package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import org.scalajs.dom.html.Span
import scalatags.JsDom.all.span

case class TranslationsModel(
    extraTurnText: Span,
    myBoardTitle: Span,
    enemyBoardTitle: Span,
    realEnemyBoardTitle: Span,
    previewBoardTitle: Span,
    withoutRuleTimeLimit: Span,
    withRuleTimeLimit: Span,
    seconds: Span,
    totalTime: Span,
    eachTurn: Span,
    amountOfShots: Span,
    turnBonuses: Span,
    bonusFirstBlood: Span,
    bonusDoubleKill: Span,
    bonusTripleKill: Span,
    shots: Span,
    placeMarksCorrectly1: Span,
    placeMarksCorrectly2: Span,
    sendPuzzleAnswer: Span,
    solvedPuzzles: Span,
    puzzleCorrect: Span,
    puzzleIncorrect: Span
)

object TranslationsModel extends HasModelPropertyCreator[TranslationsModel] {

  val default: TranslationsModel =
    TranslationsModel(
      extraTurnText = span.render,
      myBoardTitle = span.render,
      enemyBoardTitle = span.render,
      realEnemyBoardTitle = span.render,
      previewBoardTitle = span.render,
      withoutRuleTimeLimit = span.render,
      withRuleTimeLimit = span.render,
      seconds = span.render,
      totalTime = span.render,
      eachTurn = span.render,
      amountOfShots = span.render,
      turnBonuses = span.render,
      bonusFirstBlood = span.render,
      bonusDoubleKill = span.render,
      bonusTripleKill = span.render,
      shots = span.render,
      placeMarksCorrectly1 = span.render,
      placeMarksCorrectly2 = span.render,
      sendPuzzleAnswer = span.render,
      solvedPuzzles = span.render,
      puzzleCorrect = span.render,
      puzzleIncorrect = span.render
    )

}
