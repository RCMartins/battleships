package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.utils.Utils

case class Player(
    clientId: String,
    username: String,
    shipsLeftToPlace: List[Ship],
    myBoard: Board,
    enemyBoardMarks: Vector[Vector[(Option[Int], BoardMark)]],
    turnPlayHistory: List[TurnPlay]
) {

  def updateBoardMark(boardCoor: Coordinate, updatedBoardMark: BoardMark): Player =
    copy(enemyBoardMarks =
      Utils.updateVectorUsing(
        enemyBoardMarks,
        boardCoor,
        { case (turnNumberOpt, _) => (turnNumberOpt, updatedBoardMark) }
      )
    )

  def enemyBoardMarksWithCoor: Vector[(Coordinate, Option[Int], BoardMark)] =
    enemyBoardMarks.zipWithIndex.flatMap { case (vectorY, x) =>
      vectorY.zipWithIndex.map { case ((turnNumberOpt, boardMark), y) =>
        (Coordinate(x, y), turnNumberOpt, boardMark)
      }
    }

}

object Player extends HasGenCodec[Player]
