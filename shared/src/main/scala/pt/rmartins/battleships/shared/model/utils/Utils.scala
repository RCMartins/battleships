package pt.rmartins.battleships.shared.model.utils

import pt.rmartins.battleships.shared.model.game.{BoardMark, Coordinate}

object Utils {

  def updateVectorUsing(
      marks: Vector[Vector[(Option[Int], BoardMark)]],
      coordinate: Coordinate,
      f: PartialFunction[(Option[Int], BoardMark), (Option[Int], BoardMark)]
  ): Vector[Vector[(Option[Int], BoardMark)]] = {
    val vectorX: Vector[(Option[Int], BoardMark)] = marks(coordinate.x)
    val current = vectorX(coordinate.y)
    marks.updated(
      coordinate.x,
      vectorX.updated(
        coordinate.y,
        f.applyOrElse(current, (_: (Option[Int], BoardMark)) => current)
      )
    )
  }

}
