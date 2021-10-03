package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

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

  def isInside(maxCoor: Coordinate): Boolean = x >= 0 && y >= 0 && this < maxCoor

  def roundTo(maxSize: Coordinate): Coordinate =
    Coordinate(Math.max(0, Math.min(maxSize.x - 1, x)), Math.max(0, Math.min(maxSize.y - 1, y)))

  def distance(other: Coordinate): Int = Math.max(Math.abs(x - other.x), Math.abs(y - other.y))

  override def toString: String = s"Coor($x,$y)"

}

object Coordinate extends HasGenCodec[Coordinate]
