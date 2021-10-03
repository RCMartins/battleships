package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.game.Rotation.{
  Rotation0,
  Rotation1,
  Rotation2,
  Rotation3
}
import pt.rmartins.battleships.shared.model.game.Ship.toShip

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

case class Ship(shipId: Int, pieces: List[Coordinate], rotation: Rotation) {

  val piecesSize: Int = pieces.size

  val size: Coordinate = Coordinate(pieces.maxBy(_.x).x + 1, pieces.maxBy(_.y).y + 1)

  def rotateBy(delta: Int): Ship =
    rotateTo(rotation.rotateBy(delta))

  def rotateTo(rotation: Rotation): Ship =
    Ship.cacheRotations.getOrElseUpdate(
      (shipId, rotation.rIndex),
      Ship.createRotatedShip(shipId, rotation)
    )

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

  val Submarine: Ship = List((0, 0)).pipe(toShip)
  val PatrolBoat2: Ship = List((0, 0), (0, 1)).pipe(toShip)
  val PatrolBoat3: Ship = List((0, 0), (0, 1), (0, 2)).pipe(toShip)
  val PatrolBoat4: Ship = List((0, 0), (0, 1), (0, 2), (0, 3)).pipe(toShip)
  val Carrier: Ship = List((0, 0), (1, 0), (2, 0), (1, 1), (1, 2)).pipe(toShip)

  val allShips: Map[Int, Ship] =
    List(
      Submarine,
      PatrolBoat2,
      PatrolBoat3,
      PatrolBoat4,
      Carrier
    ).map(ship => ship.shipId -> ship).toMap

  val allShipsNames: Map[Int, String] =
    Map(
      Submarine -> "Submarine",
      PatrolBoat2 -> "PatrolBoat2",
      PatrolBoat3 -> "PatrolBoat3",
      PatrolBoat4 -> "PatrolBoat4",
      Carrier -> "Carrier"
    ).map { case (ship, name) => ship.shipId -> name }

  private[Ship] lazy val cacheRotations: mutable.Map[(Int, Int), Ship] =
    mutable.Map.empty[(Int, Int), Ship] ++
      List[Ship](
        Submarine,
        PatrolBoat2,
        PatrolBoat3,
        PatrolBoat4,
        Carrier
      ).map(ship => (ship.shipId, Rotation0.rIndex) -> ship)

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
