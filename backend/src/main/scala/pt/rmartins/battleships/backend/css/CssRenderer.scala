package pt.rmartins.battleships.backend.css

import io.udash.css._
import pt.rmartins.battleships.shared.css.{ChatStyles, GameStyles, GlobalStyles, LoginPageStyles}
import scalacss.internal.{Renderer, StringRenderer}

/** Renderer of styles based on UdashCSS. */
class CssRenderer(path: String, renderPretty: Boolean) {
  private val renderer: Renderer[String] =
    if (renderPretty) StringRenderer.defaultPretty
    else StringRenderer.formatTiny

  def render(): Unit = {
    new CssFileRenderer(
      path,
      Seq(
        // the list of styles to be rendered
        GlobalStyles,
        LoginPageStyles,
        ChatStyles,
        GameStyles
      ),
      createMain = true
    ).render()(renderer)
  }
}

object CssRenderer {
  def main(args: Array[String]): Unit = {
    require(args.length == 2, " Expected two arguments: target path and pretty print enable flag")
    new CssRenderer(args(0), java.lang.Boolean.parseBoolean(args(1))).render()
  }
}
