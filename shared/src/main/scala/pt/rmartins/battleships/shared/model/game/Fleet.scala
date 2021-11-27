package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Fleet(shipCounterList: List[(ShipId, (Int, Rotation))]) {

  lazy val shipAmount: Int = shipCounterList.map(_._2._1).sum

  lazy val shipCounterMap: Map[ShipId, (Int, Rotation)] = shipCounterList.toMap

  lazy val maxSize: Coordinate = {
    val ships: List[Ship] =
      shipCounterList.map { case (shipId, (_, rotation)) =>
        Ship.getShip(shipId, rotation)
      }
    Coordinate(ships.maxBy(_.size.x).size.x, ships.maxBy(_.size.y).size.y)
  }

  lazy val shipsList: List[Ship] =
    shipCounterList.flatMap { case (shipId, (amount, rotation)) =>
      List.fill(amount)(Ship.getShip(shipId, rotation))
    }

  def updateCounter(
      shipId: ShipId,
      amount: Int,
      rotation: Rotation
  ): List[(ShipId, (Int, Rotation))] =
    if (shipCounterMap.contains(shipId))
      shipCounterList.map {
        case (currentShipId, _) if currentShipId == shipId =>
          (currentShipId, (amount, rotation))
        case other =>
          other
      }
    else
      (shipId, (amount, rotation)) :: shipCounterList

}

object Fleet extends HasGenCodec[Fleet] {

  def fromShips(initialShips: List[(Ship, Int)]): Fleet =
    Fleet(
      initialShips.map { case (ship, amount) =>
        ship.shipId -> ((amount, Rotation.Rotation0))
      }
    )

  def fromShipIds(initialShips: List[(ShipId, Int)]): Fleet =
    Fleet(
      initialShips.map { case (shipId, amount) =>
        shipId -> ((amount, Rotation.Rotation0))
      }
    )

  val default10By10: (Coordinate, Fleet) =
    (
      Coordinate.square(10),
      fromShips(
        List(
          Ship.Submarine -> 4,
          Ship.Skeeter -> 3,
          Ship.Ranger -> 2,
          Ship.Conqueror -> 1,
          Ship.AircraftCarrier -> 1
        )
      )
    )

  val default15By15: (Coordinate, Fleet) =
    (
      Coordinate.square(15),
      fromShips(
        List(
          Ship.Skeeter -> 4,
          Ship.Ranger -> 3,
          Ship.Conqueror -> 2,
          Ship.TorpedoBoat -> 2,
          Ship.Cruiser -> 2,
          Ship.Epoch -> 1,
          Ship.Battleship -> 1
        )
      )
    )

}
