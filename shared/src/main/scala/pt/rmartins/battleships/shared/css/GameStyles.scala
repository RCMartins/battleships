package pt.rmartins.battleships.shared.css

import io.udash.css._

import scala.concurrent.duration.DurationInt
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
    height :=! "calc(100vh - 140px)",
  )

  val playerVsCardHeight: CssStyle = style(
    height :=! "calc(100vh - 300px)",
  )

  val noWarp: CssStyle = style(
    whiteSpace.nowrap.important
  )

  val blackTextStyle: CssStyle = style(
    color :=! "#000000"
  )

  val redTextStyle: CssStyle = style(
    color :=! "#FF0000"
  )

  val turnTextSize: CssStyle = style(
    fontSize :=! "20px"
  )

  private val growAndShrinkKeyFrames = style(
    keyframes(
      0.0 -> keyframe(blackTextStyle, turnTextSize, height :=! "30px"),
      50.0 -> keyframe(redTextStyle, fontSize :=! "26px", height :=! "30px"),
      100.0 -> keyframe(blackTextStyle, turnTextSize, height :=! "30px"),
    )
  )

  val growAndShrink: CssStyle = style(
    animationName(growAndShrinkKeyFrames),
    animationIterationCount.infinite,
    animationDuration(2 seconds),
  )

}
