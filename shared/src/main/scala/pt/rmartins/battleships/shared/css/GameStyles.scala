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

  val cursorPointer: CssStyle = style(
    cursor :=! "pointer"
  )

  val unselectableText: CssStyle = style(
    userSelect :=! "none"
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

  val myCollapsible: CssStyle = style(
    position.relative,
    zIndex(1),
  )

  /*


.collapsible {
  /* The div that will be collapsed/expanded */
  position: relative;
  /* Position the div above other elements on the page */
  z-index: 1;
  /* Set the initial height of the div to 0, so it will be hidden */
  width: 0;
  /* Set the overflow property to "hidden" to hide the content of the div */
  overflow: hidden;
  /* Add a transition to smoothly animate the height of the div */
  transition: left 0.3s ease;
}

.my-collapsible {
  /* The div that will be collapsed/expanded */
  position: relative;
  /* Position the div above other elements on the page */
  z-index: 1;
  /* Add a transition to smoothly animate the height of the div */
  transition: left 0.3s ease;
}

.collapsible.expanded {
  /* When the div is expanded, set the height to auto to show the content */
  width: auto;
}
   */

}
