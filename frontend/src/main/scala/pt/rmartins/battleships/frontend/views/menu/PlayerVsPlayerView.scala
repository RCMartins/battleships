package pt.rmartins.battleships.frontend.views.menu

import io.udash.ModelProperty
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.css.CssView
import org.scalajs.dom.html.{Div, Input}
import pt.rmartins.battleships.frontend.views.game._
import pt.rmartins.battleships.shared.css.GameStyles
import pt.rmartins.battleships.shared.model.game.Username
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class PlayerVsPlayerView(
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gamePresenter: GamePresenter,
    playesVsUtils: PlayerVsUtils,
    preGameView: PreGameView
) extends CssView {

  def mainPlayerDiv(nested: NestedInterceptor): Div = {
    div(
      `class` := "card",
      div(
        `class` := "card-body p-0",
        div(
          `class` := "px-0 py-0 overflow-auto",
          GameStyles.flexContainer,
          GameStyles.mainCardHeight,
          div(
            `class` := "row m-0",
            div(
              `class` := "col-12",
              editUsernameDiv()
            )
          )
        ),
        footer()
      )
    ).render
  }

  def editUsernameDiv(): JsDom.all.Modifier = {
    val usernameDiv: Input =
      input(
        `type` := "text",
        `class` := "form-control",
        placeholder := "Username",
      ).render

    div(
      `class` := "p-3",
      div(
        `class` := "input-group",
        div(
          `class` := "input-group-text",
          "Your username",
        ),
        div(
          `class` := "input-group-append",
          usernameDiv,
          button(
            `type` := "button",
            `class` := "btn btn-outline-secondary",
            "Confirm",
          ).render.tap { _ =>
            gamePresenter.setPlayerName(Username(usernameDiv.textContent))
          }
        )
      )
    )
  }

  def footer(): JsDom.all.Modifier =
    div(
      `class` := "card-footer",
      div(
        `class` := "row mx-0 d-flex justify-content-center",
//        div(`class` := "mx-2", playesVsUtils.confirmRulesButton),
//        div(`class` := "mx-2", playesVsUtils.startGameVsBotButton),
      )
    )

}
