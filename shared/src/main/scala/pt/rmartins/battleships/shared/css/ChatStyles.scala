package pt.rmartins.battleships.shared.css

import io.udash.css._

import scala.language.postfixOps

object ChatStyles extends CssBase {
  import dsl._

  val messagesWindow: CssStyle = style(
    height :=! "calc(50vh - 300px)",
    overflowY.auto
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

  val turnContainer: CssStyle = style(
    display.flex,
    alignItems.center
  )
}
