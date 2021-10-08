package pt.rmartins.battleships.shared.model.utils

import pt.rmartins.battleships.shared.model.game.{BoardMark, Coordinate, Turn}

object Utils {

  def updateVectorUsing(
      marks: Vector[Vector[(Option[Turn], BoardMark)]],
      coordinate: Coordinate,
      f: PartialFunction[(Option[Turn], BoardMark), (Option[Turn], BoardMark)]
  ): Vector[Vector[(Option[Turn], BoardMark)]] = {
    val vectorX: Vector[(Option[Turn], BoardMark)] = marks(coordinate.x)
    val current = vectorX(coordinate.y)
    marks.updated(
      coordinate.x,
      vectorX.updated(
        coordinate.y,
        f.applyOrElse(current, (_: (Option[Turn], BoardMark)) => current)
      )
    )
  }

}
