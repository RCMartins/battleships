package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Coordinate(x: Int, y: Int) {

  def unary_- : Coordinate = Coordinate(-x, -y)

  def +(other: Coordinate): Coordinate = Coordinate(x + other.x, y + other.y)

  def -(other: Coordinate): Coordinate = Coordinate(x - other.x, y - other.y)

  def *(mult: Int): Coordinate = Coordinate(x * mult, y * mult)

  def *(mult: Double): Coordinate = Coordinate((x * mult).toInt, (y * mult).toInt)

  def /(mult: Int): Coordinate = Coordinate(x / mult, y / mult)

  def <(other: Coordinate): Boolean = x < other.x && y < other.y

  def <=(other: Coordinate): Boolean = x <= other.x && y <= other.y

  def >(other: Coordinate): Boolean = x > other.x && y > other.y

  def >=(other: Coordinate): Boolean = x >= other.x && y >= other.y

  def isInsideBoard(maxCoor: Coordinate): Boolean = x >= 0 && y >= 0 && this < maxCoor

  def roundTo(maxSize: Coordinate): Coordinate =
    Coordinate(Math.max(0, Math.min(maxSize.x - 1, x)), Math.max(0, Math.min(maxSize.y - 1, y)))

  def distance(other: Coordinate): Int = Math.max(Math.abs(x - other.x), Math.abs(y - other.y))

  def flipCoor: Coordinate = Coordinate(y, x)

  def get8CoorAround: List[Coordinate] =
    (
      for {
        dx <- -1 to 1
        dy <- -1 to 1
        if dx != 0 || dy != 0
      } yield Coordinate(x + dx, y + dy)
    ).toList

  def min: Int = Math.min(x, y)

  def max: Int = Math.max(x, y)

  def map(f: Coordinate => Coordinate): Coordinate = f(this)

  override def toString: String = s"Coor($x,$y)"

  def toCodeString: String = s"Coordinate($x, $y)"

}

object Coordinate extends HasGenCodec[Coordinate] {

  implicit val coordinateEncoder: JsonEncoder[Coordinate] =
    DeriveJsonEncoder.gen[Coordinate]

  implicit val coordinateDecoder: JsonDecoder[Coordinate] =
    DeriveJsonDecoder.gen[Coordinate]

  val origin: Coordinate = Coordinate(0, 0)

  def square(size: Int): Coordinate = Coordinate(size, size)

  def fill(boardSize: Coordinate): Set[Coordinate] =
    (for {
      x <- 0 until boardSize.x
      y <- 0 until boardSize.y
    } yield Coordinate(x, y)).toSet

  def defaultCompare(coor1: Coordinate, coor2: Coordinate): Int =
    if (coor1.x == coor2.x)
      coor2.y - coor1.y
    else
      coor2.x - coor1.x

}
