package pt.rmartins.battleships.shared.css

import io.udash.css._

import scala.language.postfixOps

object GlobalStyles extends CssBase {
  import dsl._

  val floatRight: CssStyle = style(
    float.right
  )

  val msgDate: CssStyle = style(
    marginLeft(5 px),
    fontSize(0.7 em),
    color.gray
  )

  val msgContainer: CssStyle = style(
    unsafeChild(s".${msgDate.className}")(
      display.none
    ),
    &.hover(
      unsafeChild(s".${msgDate.className}")(
        display.initial
      )
    )
  )
}
