package pt.rmartins.battleships.frontend.views.menu

import io.udash.ModelProperty
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.css.CssView
import org.scalajs.dom.html.Div
import pt.rmartins.battleships.frontend.views.game._
import scalatags.JsDom.all._

class PlayerVsPlayerView(
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gamePresenter: GamePresenter,
    preGameView: PreGameView
) extends CssView {

  def mainEditorDiv(nested: NestedInterceptor): Div = {
    div.render
  }

}
