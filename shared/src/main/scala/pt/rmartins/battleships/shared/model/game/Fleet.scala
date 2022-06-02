package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

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

  implicit val fleetEncoder: JsonEncoder[Fleet] =
    DeriveJsonEncoder.gen[Fleet]

  implicit val fleetDecoder: JsonDecoder[Fleet] =
    DeriveJsonDecoder.gen[Fleet]

  val empty: Fleet = Fleet(Nil)

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

  def fromShipsList(initialShips: List[Ship]): Fleet =
    Fleet(
      initialShips.groupBy(_.shipId).toList.map { case (shipId, list) =>
        shipId -> ((list.size, Rotation.Rotation0))
      }
    )

}
