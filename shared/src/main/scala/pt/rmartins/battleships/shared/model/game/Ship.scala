package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.game.Rotation._

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

case class Ship(shipId: Int, pieces: List[Coordinate], rotation: Rotation) {

  val piecesSize: Int = pieces.size

  val size: Coordinate = Coordinate(pieces.maxBy(_.x).x + 1, pieces.maxBy(_.y).y + 1)

  @inline
  def rotateBy(delta: Int): Ship =
    rotateTo(rotation.rotateBy(delta))

  def rotateTo(rotation: Rotation): Ship =
    Ship.getShip(shipId, rotation)

  private[Ship] def mapPieces(f: Coordinate => Coordinate): Ship =
    Ship(shipId, pieces.map(f), rotation)

}

object Ship extends HasGenCodec[Ship] {

  private var id: Int = 0

  private def toShip(piecePairs: List[(Int, Int)]): Ship = {
    val thisShipId = id
    id += 1
    Ship(thisShipId, piecePairs.map { case (x, y) => Coordinate(x, y) }, Rotation0)
  }

  def l(y: Int, x: Int*): Seq[(Int, Int)] = x.map((_, y))

  val Submarine: Ship = List((0, 0)).pipe(toShip)
  val Skeeter: Ship = List((0, 0), (0, 1)).pipe(toShip)
  val Ranger: Ship = List((0, 0), (0, 1), (0, 2)).pipe(toShip)
  val Conqueror: Ship = List((0, 0), (0, 1), (0, 2), (0, 3)).pipe(toShip)
  val AircraftCarrier: Ship = List((0, 0), (1, 0), (2, 0), (1, 1), (1, 2)).pipe(toShip)
  val TorpedoBoat: Ship = List((1, 0), (0, 1), (0, 2), (2, 1), (2, 2)).pipe(toShip)
  val Cruiser: Ship = List((0, 0), (1, 1), (2, 2)).pipe(toShip)
  val Epoch: Ship =
    List((0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1), (1, 2)).pipe(toShip)
  val Battleship: Ship =
    List((0, 0), (1, 0), (2, 0), (1, 1), (0, 2), (1, 2), (2, 2), (1, 3), (1, 4)).pipe(toShip)
  val MotherShip: Ship =
    List(l(y = 0, 0, 1, 3, 5, 6), l(y = 1, 1, 2, 3, 4, 5), l(y = 2, 0, 1, 3, 5, 6)).flatten
      .pipe(toShip)
  val HeavyCruiser: Ship =
    List((2, 0), (1, 1), (3, 1), (0, 2), (4, 2), (1, 3), (3, 3)).pipe(toShip)
  val Destroyer: Ship =
    List((2, 0), (2, 1), (0, 2), (1, 2), (2, 2), (3, 2), (4, 2), (2, 3), (2, 4)).pipe(toShip)
  val FireShip: Ship =
    List((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5)).pipe(toShip)
  //val Colossus 3,0 3,1 2,2 3,2 4,2 1,3 2,3 3,3 4,3 5,3 1,4 2,4 3,4 4,4 5,4 1,5 3,5 5,5 0,6 3,6 6,6 3,7

  private val allShipsList: List[Ship] =
    List[Ship](
      Submarine,
      Skeeter,
      Ranger,
      Conqueror,
      AircraftCarrier,
      TorpedoBoat,
      Cruiser,
      Epoch,
      Battleship,
      MotherShip,
      HeavyCruiser,
      Destroyer,
      FireShip
    )

  private val cacheRotations: mutable.Map[(Int, Int), Ship] =
    mutable.Map.empty[(Int, Int), Ship] ++
      allShipsList.map(ship => (ship.shipId, Rotation0.rIndex) -> ship)

  def getShip(shipId: Int, rotation: Rotation): Ship =
    cacheRotations.getOrElseUpdate(
      (shipId, rotation.rIndex),
      Ship.createRotatedShip(shipId, rotation)
    )

  val allShips: Map[Int, Ship] =
    allShipsList.map(ship => ship.shipId -> ship).toMap

  val shipLongXMap: Map[Int, Ship] =
    allShips.keys
      .map(shipId => shipId -> Rotation.all.map(getShip(shipId, _)).maxBy(_.size.x))
      .toMap

  def createRotatedShip(shipId: Int, rotation: Rotation): Ship = {
    def moveNegativeCoor(ship: Ship): Ship = {
      val maxNegX = ship.pieces.minBy(_.x).x
      val maxNegY = ship.pieces.minBy(_.y).y
      val coor = -Coordinate(maxNegX, maxNegY)
      ship.mapPieces(_ + coor)
    }

    val shipRotation0 = cacheRotations((shipId, Rotation0.rIndex))
    moveNegativeCoor(
      rotation match {
        case Rotation1 =>
          shipRotation0.mapPieces({ case Coordinate(x, y) => Coordinate(y, x) })
        case Rotation2 =>
          shipRotation0.mapPieces({ case Coordinate(x, y) => Coordinate(x, -y) })
        case Rotation3 =>
          shipRotation0.mapPieces({ case Coordinate(x, y) => Coordinate(-y, x) })
        case _ =>
          shipRotation0
      }
    ).copy(rotation = rotation)
  }

}
