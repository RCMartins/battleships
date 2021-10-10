package pt.rmartins.battleships.shared.model.utils

import pt.rmartins.battleships.shared.model.game.{Board, BoardMark, Coordinate, Ship, Turn}

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

  def canPlaceInBoard(board: Board, shipToPlace: Ship, boardCoor: Coordinate): Boolean = {
    val actualPiecePositions = shipToPlace.pieces.map(_ + boardCoor)

    actualPiecePositions.forall(_.isInsideBoard(board.boardSize)) &&
    !actualPiecePositions
      .exists(coor => board.ships.exists(_.shipActualPieces.exists(_.distance(coor) <= 1)))
  }

}
