package pt.rmartins.battleships.shared.model.utils

import pt.rmartins.battleships.shared.model.game.{Board, BoardMark, Coordinate, Ship, Turn}

object BoardUtils {

  type BoardMarks = Vector[Vector[(Option[Turn], BoardMark)]]

  def createEmptyBoardMarks(boardSize: Coordinate): BoardMarks =
    Vector.fill(boardSize.x)(Vector.fill(boardSize.y)((None, BoardMark.Empty)))

  def updateBoardMarksUsing(
      boardMarks: BoardMarks,
      coordinate: Coordinate,
      f: PartialFunction[(Option[Turn], BoardMark), (Option[Turn], BoardMark)]
  ): BoardMarks = {
    val vectorX: Vector[(Option[Turn], BoardMark)] = boardMarks(coordinate.x)
    val current = vectorX(coordinate.y)
    boardMarks.updated(
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

  def toList(boardSize: Coordinate, boardMarks: BoardMarks): List[(Coordinate, BoardMark)] =
    (for {
      x <- 0 until boardSize.x
      y <- 0 until boardSize.y
    } yield (Coordinate(x, y), boardMarks(x)(y)._2)).toList

}
