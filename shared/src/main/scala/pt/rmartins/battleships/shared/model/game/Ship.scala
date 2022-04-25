package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.game.Rotation._

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

case class Ship(shipId: ShipId, pieces: List[Coordinate], rotation: Rotation) {

  val piecesSize: Int = pieces.size

  val size: Coordinate = Coordinate(pieces.maxBy(_.x).x + 1, pieces.maxBy(_.y).y + 1)

  @inline
  def rotateBy(delta: Int): Ship =
    rotateTo(rotation.rotateBy(delta))

  def rotateTo(rotation: Rotation): Ship =
    Ship.getShip(shipId, rotation)

  private[Ship] def mapPieces(f: Coordinate => Coordinate): Ship =
    Ship(shipId, pieces.map(f), rotation)

  def shipBiggestToSmallestOrder: (Int, Int) =
    (-piecesSize, shipId.id)

  def name: String =
    Ship.shipsNamesMap(shipId)

  def shortName: String =
    Ship.shipsShortNamesMap(shipId)

}

object Ship extends HasGenCodec[Ship] {

  private var id: Int = 0

  private def toShip(piecePairs: List[(Int, Int)]): Ship = {
    val thisShipId = id
    id += 1
    Ship(ShipId(thisShipId), piecePairs.map { case (x, y) => Coordinate(x, y) }, Rotation0)
  }

  def l(y: Int, x: Int*): Seq[(Int, Int)] = x.map((_, y))

  val Submarine: Ship = List((0, 0)).pipe(toShip)
  val Skeeter: Ship = List((0, 0), (0, 1)).pipe(toShip)
  val Ranger: Ship = List((0, 0), (0, 1), (0, 2)).pipe(toShip)
  val Conqueror: Ship = List((0, 0), (0, 1), (0, 2), (0, 3)).pipe(toShip)
  val AircraftCarrier: Ship = List((0, 0), (1, 0), (2, 0), (1, 1), (1, 2)).pipe(toShip)
  val TorpedoBoat: Ship = List((1, 0), (0, 1), (0, 2), (2, 1), (2, 2)).pipe(toShip)
  val Cruiser: Ship = List((0, 0), (1, 1), (2, 2)).pipe(toShip)
  val MiniCruiser: Ship = List((0, 0), (1, 1)).pipe(toShip)
  val Epoch: Ship =
    List((0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (1, 2)).pipe(toShip)
  val Battleship: Ship =
    List((0, 0), (1, 0), (2, 0), (1, 1), (0, 2), (1, 2), (2, 2), (1, 3), (1, 4)).pipe(toShip)
  val MotherShip: Ship =
    List(l(y = 0, 0, 1, 3, 5, 6), l(y = 1, 1, 2, 3, 4, 5), l(y = 2, 0, 1, 3, 5, 6)).flatten
      .map(_.swap)
      .pipe(toShip)
  val Atoll: Ship =
    List((2, 0), (1, 1), (3, 1), (0, 2), (4, 2), (1, 3), (3, 3)).pipe(toShip)
  val HoleStar: Ship =
    List((0, 1), (1, 0), (1, 2), (2, 1)).pipe(toShip)
  val SmallStar: Ship =
    List((0, 1), (1, 0), (1, 1), (1, 2), (2, 1)).pipe(toShip)
  val StarShip: Ship =
    List((2, 0), (2, 1), (0, 2), (1, 2), (2, 2), (3, 2), (4, 2), (2, 3), (2, 4)).pipe(toShip)
  val LShip: Ship =
    List((0, 0), (0, 1), (0, 2), (1, 2)).pipe(toShip)
  val MissileShip: Ship =
    List((0, 1), (1, 0), (1, 1), (1, 2), (2, 1), (2, 2), (2, 3), (3, 2), (3, 3)).pipe(toShip)
  val WShip: Ship =
    List((0, 2), (1, 1), (1, 2), (2, 0), (2, 1)).pipe(toShip)
  val ArrowShip: Ship =
    List((0, 2), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)).pipe(toShip)
  val LongShip: Ship =
    List((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5)).pipe(toShip)
  val SmallSnake: Ship =
    List((0, 2), (1, 0), (1, 1), (1, 2), (2, 0)).pipe(toShip)
  val LongArrow: Ship =
    List((0, 0), (1, 1), (2, 2), (2, 4), (3, 3), (3, 4), (4, 2), (4, 3), (4, 4)).pipe(toShip)
  val Plane: Ship =
    List((0, 2), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 3)).pipe(toShip)
  val HoledMissile: Ship =
    List((0, 1), (1, 0), (1, 2), (2, 1), (2, 3), (3, 2), (3, 3)).pipe(toShip)
  val StaffShip: Ship =
    List((0, 1), (1, 0), (1, 2), (1, 3), (1, 4), (2, 1)).pipe(toShip)
  val Butterfly: Ship =
    List((0, 1), (1, 0), (1, 1), (2, 2), (2, 3), (3, 2)).pipe(toShip)
  val HammerHead: Ship =
    List((0, 0), (0, 2), (1, 1), (2, 0), (2, 1), (2, 2)).pipe(toShip)
  val CornerShip: Ship =
    List((0, 0), (0, 1), (0, 2), (1, 0), (2, 0)).pipe(toShip)
  val MiniCorner: Ship =
    List((0, 0), (0, 1), (1, 0)).pipe(toShip)
  val SmallC: Ship =
    List((0, 0), (0, 1), (0, 2), (1, 0), (1, 2)).pipe(toShip)

  private val allShipsListNames: List[(Ship, String, String)] =
    List[(Ship, String, String)](
      (Submarine, "Submarine", "Su"),
      (Skeeter, "Skeeter", "Sk"),
      (Ranger, "Ranger", "Ra"),
      (Conqueror, "Conqueror", "Co"),
      (AircraftCarrier, "AircraftCarrier", "AC"),
      (TorpedoBoat, "TorpedoBoat", "TB"),
      (Cruiser, "Cruiser", "Cr"),
      (MiniCruiser, "MiniCruiser", "MC"),
      (Epoch, "Epoch", "Ep"),
      (Battleship, "Battleship", "Ba"),
      (MotherShip, "MotherShip", "MS"),
      (Atoll, "Atoll", "At"),
      (HoleStar, "HoleStar", "HS"),
      (SmallStar, "SmallStar", "sS"),
      (StarShip, "StarShip", "SS"),
      (LShip, "LShip", "LS"),
      (MissileShip, "MissileShip", "MS"),
      (HoledMissile, "HoledMissile", "HM"),
      (WShip, "WShip", "SD"),
      (ArrowShip, "ArrowShip", "AS"),
      (LongShip, "LongShip", "LS"),
      (SmallSnake, "SmallSnake", "SS"),
      (LongArrow, "LongArrow", "LA"),
      (Plane, "Plane", "Pl"),
      (StaffShip, "StaffShip", "St"),
      (Butterfly, "Butterfly", "Bu"),
      (HammerHead, "HammerHead", "HH"),
      (CornerShip, "CornerShip", "CS"),
      (MiniCorner, "MiniCorner", "MC"),
      (SmallC, "SmallC", "SC")
    ).sortBy(_._1.piecesSize)

  val shipsNamesMap: Map[ShipId, String] =
    allShipsListNames.map { case (ship, name, _) => ship.shipId -> name }.toMap

  val shipsShortNamesMap: Map[ShipId, String] =
    allShipsListNames.map { case (ship, _, shortName) => ship.shipId -> shortName }.toMap

  if (allShipsListNames.size != shipsShortNamesMap.size)
    throw new Exception(
      s"Duplicate Short Name: ${allShipsListNames.map(_._3).groupBy(identity).keys}"
    )

  val allShipsList: List[Ship] =
    allShipsListNames.map(_._1)

  val allShipIdsSet: Set[ShipId] =
    allShipsList.map(_.shipId).toSet

  lazy val allShipsFleetMaxX: Fleet = {
    val initialFleet =
      Fleet(
        allShipsList.map { ship =>
          ship.shipId -> ((1, if (ship.size.y > ship.size.x) Rotation1 else Rotation0))
        }
      )

    val fleetCoordinateSize = initialFleet.maxSize

    Fleet(
      initialFleet.shipsList.map { ship =>
        val finalShip =
          Some(getShip(ship.shipId, Rotation.Rotation0))
            .filter(_.size <= fleetCoordinateSize)
            .getOrElse(ship)
        finalShip.shipId -> ((1, ship.rotation))
      }
    )
  }

  lazy val shipMaxXMessagesMap: Map[ShipId, Ship] =
    allShipsList
      .map { ship =>
        if (ship.size.y > ship.size.x) ship.rotateBy(1) else ship
      }
      .map(ship => ship.shipId -> ship)
      .toMap

  private val cacheRotations: mutable.Map[(ShipId, Int), Ship] =
    mutable.Map.empty[(ShipId, Int), Ship] ++
      allShipsList.map(ship => (ship.shipId, Rotation0.rIndex) -> ship)

  def getShip(shipId: ShipId, rotation: Rotation): Ship =
    cacheRotations.getOrElseUpdate(
      (shipId, rotation.rIndex),
      Ship.createRotatedShip(shipId, rotation)
    )

  val allShipsMap: Map[ShipId, Ship] =
    allShipsList.map(ship => ship.shipId -> ship).toMap

  val allShipsUniqueRotations: Map[ShipId, List[Ship]] =
    allShipsList
      .map(ship =>
        ship.shipId ->
          Rotation.all
            .map(getShip(ship.shipId, _))
            .map(ship => (ship, ship.pieces.toSet))
            .distinctBy(_._2)
            .map(_._1)
            .toList
      )
      .toMap

  def createRotatedShip(shipId: ShipId, rotation: Rotation): Ship = {
    def moveNegativeCoor(ship: Ship): Ship = {
      val maxNegX = ship.pieces.minBy(_.x).x
      val maxNegY = ship.pieces.minBy(_.y).y
      val coor = -Coordinate(maxNegX, maxNegY)
      ship.mapPieces(_ + coor)
    }

    val shipRotation0: Ship = {
      val ship = cacheRotations((shipId, Rotation0.rIndex))
      ship.mapPieces(_ - ship.size)
    }
    moveNegativeCoor(
      rotation match {
        case Rotation1 =>
          shipRotation0.mapPieces({ case Coordinate(x, y) => Coordinate(-y, x) })
        case Rotation2 =>
          shipRotation0.mapPieces({ case Coordinate(x, y) => Coordinate(-x, -y) })
        case Rotation3 =>
          shipRotation0.mapPieces({ case Coordinate(x, y) => Coordinate(y, -x) })
        case _ =>
          shipRotation0
      }
    ).copy(rotation = rotation)
  }

}
