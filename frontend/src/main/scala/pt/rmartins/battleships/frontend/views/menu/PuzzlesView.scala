package pt.rmartins.battleships.frontend.views.menu

import io.udash._
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n._
import io.udash.properties.single.Property
import org.scalajs.dom.html.Div
import org.scalajs.dom.{UIEvent, window}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game._
import pt.rmartins.battleships.frontend.views.model.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.shared.css.GameStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode._
import scalatags.JsDom.all._

class PuzzlesView(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    presenter: GamePresenter,
    boardView: BoardView,
    preGameView: PreGameView,
    gameModals: GameModals,
    translationsService: TranslationsService,
) extends CssView {

  import translationsService._

  private val sendPuzzleSolutionButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    disabled = presenter.gamePuzzleStateProperty.transform(_.exists(_.puzzleSolutionOpt.nonEmpty)),
    componentId = ComponentId("send-puzzle-solution-button")
  )(nested =>
    Seq[Modifier](
      span(nested(translatedDynamic(Translations.Game.sendPuzzleAnswerButton)(_.apply())))
    )
  )

  sendPuzzleSolutionButton.listen { _ =>
    presenter.getPuzzleSolution()
  }

  private val nextPuzzleButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("next-puzzle-button")
  )(nested =>
    Seq[Modifier](
      span(nested(translatedDynamic(Translations.Game.nextPuzzleButton)(_.apply())))
    )
  )

  nextPuzzleButton.listen { _ =>
    presenter.startNewPuzzle()
  }

  def createMainDiv(nested: NestedInterceptor): Div = {
    val mainDiv: Div =
      div(
        `class` := "d-flex justify-content-center",
        GameStyles.mainCardHeight,
        boardView.drawPuzzleBoardDiv(nested),
//        preGameView.createFleetPreview(nested)
      ).render

    window.onresize = (_: UIEvent) => {
      presenter.onCanvasResize(mainDiv)
    }

    var handle: Int = 0
    handle = window.setInterval(
      () => {
        if (mainDiv.clientWidth != 0) {
          presenter.onCanvasResize(mainDiv)
          window.clearTimeout(handle)
        }
      },
      timeout = 20
    )

    div(
      `class` := "card",
      div(
        `class` := "card-body p-0",
        div(
          mainDiv
        )
      ),
      createFooter(nested)
    ).render
  }

  private def createFooter(nested: NestedInterceptor): Div = {
    /*

              case ((_, _, _, _, Some(GamePuzzleState(_, _, _, _, Some(_)))), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(
                `class` := "row mx-0",
                div(`class` := "mx-2", sendPuzzleSolutionButton)
              ),
              div(`class` := "mx-2", nextPuzzleButton)
            ).render
          case ((_, _, _, _, Some(_)), _) =>
            div(
              `class` := "row mx-0",
              div(`class` := "mx-2", sendPuzzleSolutionButton)
            ).render

     */
    div(
      `class` := "card-footer",
      div(
        `class` := "row justify-content-between mx-0",
        div(
          `class` := "row mx-0",
          div(`class` := "mx-2", sendPuzzleSolutionButton)
        ),
        div(`class` := "mx-2", nextPuzzleButton)
      )
    ).render
  }

}
