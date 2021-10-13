package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Fleet(ships: List[Ship]) {

  lazy val shipAmount: Int = ships.size

  lazy val size: Coordinate =
    Coordinate(ships.maxBy(_.size.x).size.x, ships.maxBy(_.size.y).size.y)

}

object Fleet extends HasGenCodec[Fleet]
