package pt.rmartins.battleships.shared.css

import io.udash.css._

import scala.language.postfixOps

object GameStyles extends CssBase {
  import dsl._

  val canvasWithoutBorder: CssStyle = style(
    outline.none
  )

  val flexContainer: CssStyle = style(
    display.flex,
    overflowX.hidden,
    overflowY.hidden,
    unsafeChild("> *")(
      overflowY.scroll
    )
  )

  val hideScrollX: CssStyle = style(
    overflowX.hidden
  )

}
