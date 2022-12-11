package pt.rmartins.battleships.frontend.views.menu

import io.udash.ModelProperty
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.css.CssView
import org.scalajs.dom.html.Div
import pt.rmartins.battleships.frontend.views.game._
import pt.rmartins.battleships.shared.css.GameStyles
import scalatags.JsDom
import scalatags.JsDom.all._

class PlayerVsBotsView(
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gamePresenter: GamePresenter,
    playesVsUtils: PlayerVsUtils,
    preGameView: PreGameView
) extends CssView {

  def mainEditorDiv(nested: NestedInterceptor): Div = {
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
//      GameStyles.mainCardHeight,
            div(
              `class` := "col-12",
//        style := "overflow: auto",
//        GameStyles.flexContainer,
//        GameStyles.playerVsCardHeight,
              preGameView.createComponents(nested),
//      ),
//      div(
//        `class` := "col-12",
//        div(
//          `class` := "card-footer",
//          div(
//            `class` := "row mx-0",
//            div(`class` := "mx-2", playesVsUtils.confirmRulesButton),
//          )
//        )
            )
          )
        ),
        footer()
      )
    ).render
  }

  def footer(): JsDom.all.Modifier =
    div(
      `class` := "card-footer",
      div(
        `class` := "row mx-0 d-flex justify-content-center",
//      div(`class` := "mx-2", playesVsUtils.confirmRulesButton),
        div(`class` := "mx-2", playesVsUtils.startGameVsBotButton),
      )
    )

}
