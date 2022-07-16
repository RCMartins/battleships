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

  val hideScrolls: CssStyle = style(
    overflowX.hidden,
    overflowY.hidden
  )

  val mainCardHeight: CssStyle = style(
//    height :=! "calc(100vh - 85px)",
    height :=! "calc(100vh - 140px)",
  )

  val playerVsCardHeight: CssStyle = style(
    height :=! "calc(100vh - 300px)",
  )

  val noWarp: CssStyle = style(
    whiteSpace.nowrap.important
  )

  val redText: CssStyle = style(
    color :=! "#FF0000"
  )

}
