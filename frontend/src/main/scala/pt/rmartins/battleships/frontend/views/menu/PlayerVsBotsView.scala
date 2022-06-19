package pt.rmartins.battleships.frontend.views.menu

import io.udash.ModelProperty
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.css.CssView
import org.scalajs.dom.html.Div
import pt.rmartins.battleships.frontend.views.game._
import scalatags.JsDom
import scalatags.JsDom.all._

class PlayerVsBotsView(
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gamePresenter: GamePresenter,
    playesVsUtils: PlayesVsUtils,
    preGameView: PreGameView
) extends CssView {

  def mainEditorDiv(nested: NestedInterceptor): Div = {
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
    ).render
  }

  def footer(nested: NestedInterceptor): JsDom.all.Modifier =
//    div(
//      `class` := "col-12",
//      div(
//        `class` := "card-footer",
    div(
      `class` := "row mx-0 d-flex justify-content-center",
//      div(`class` := "mx-2", playesVsUtils.confirmRulesButton),
      div(`class` := "mx-2", playesVsUtils.startGameVsBotButton),
    )
//      )
//    )

}
