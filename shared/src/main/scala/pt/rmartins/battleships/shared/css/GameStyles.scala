package pt.rmartins.battleships.shared.css

import io.udash.css._

import scala.language.postfixOps

object GameStyles extends CssBase {
  import dsl._

  val canvasWithoutBorder: CssStyle = style(
    outline.none
  )

}
