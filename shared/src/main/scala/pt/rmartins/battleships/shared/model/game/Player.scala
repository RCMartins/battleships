package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.utils.BoardUtils
import pt.rmartins.battleships.shared.model.utils.BoardUtils.BoardMarks

case class Player(
    myBoard: Board,
    enemyBoardMarks: BoardMarks,
    turnPlayHistory: List[TurnPlay]
) {

  def updateBoardMark(boardCoor: Coordinate, updatedBoardMark: BoardMark): Player =
    copy(enemyBoardMarks =
      BoardUtils.updateBoardMarksUsing(
        enemyBoardMarks,
        boardCoor,
        { case (turnNumberOpt, _) => (turnNumberOpt, updatedBoardMark) }
      )
    )

  def enemyBoardMarksWithCoor: Vector[(Coordinate, Option[Turn], BoardMark)] =
    enemyBoardMarks.zipWithIndex.flatMap { case (vectorY, x) =>
      vectorY.zipWithIndex.map { case ((turnNumberOpt, boardMark), y) =>
        (Coordinate(x, y), turnNumberOpt, boardMark)
      }
    }

}

object Player extends HasGenCodec[Player]
