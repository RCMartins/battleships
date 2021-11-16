package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Fleet(ships: List[Ship]) {

  lazy val shipAmount: Int = ships.size

  lazy val maxSize: Coordinate =
    Coordinate(ships.maxBy(_.size.x).size.x, ships.maxBy(_.size.y).size.y)

  lazy val shipsCounter: Map[ShipId, Int] =
    ships.groupBy(_.shipId).map { case (shipId, list) => (shipId, list.size) }

}

object Fleet extends HasGenCodec[Fleet] {

  def fromShips(initialShips: List[Ship]): Fleet =
    Fleet(
      initialShips
        .groupBy(_.shipId)
        .toList
        .sortBy { case (shipId, list) =>
          (-list.head.piecesSize, shipId.id)
        }
        .flatMap(_._2)
    )

  val default10By10: (Coordinate, Fleet) =
    (
      Coordinate.square(10),
      fromShips(
        List.fill(4)(Ship.Submarine) ++
          List.fill(3)(Ship.Skeeter) ++
          List.fill(2)(Ship.Ranger) ++
          List.fill(1)(Ship.Conqueror) ++
          List.fill(1)(Ship.AircraftCarrier)
      )
    )

  val default15By15: (Coordinate, Fleet) =
    (
      Coordinate.square(10),
      fromShips(
        List.fill(0)(Ship.Submarine) ++
          List.fill(4)(Ship.Skeeter) ++
          List.fill(3)(Ship.Ranger) ++
          List.fill(2)(Ship.Conqueror) ++
          List.fill(2)(Ship.TorpedoBoat) ++
          List.fill(2)(Ship.Cruiser) ++
          List.fill(1)(Ship.Epoch) ++
          List.fill(1)(Ship.Battleship)
      )
    )

}
