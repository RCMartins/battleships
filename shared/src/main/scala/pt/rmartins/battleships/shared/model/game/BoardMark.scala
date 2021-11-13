package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait BoardMark {

  def isPermanent: Boolean = false

  def isWater: Boolean =
    this == BoardMark.Water || this == BoardMark.ManualWater

  def isShip: Boolean =
    this == BoardMark.ShipHit || this == BoardMark.ManualShip

}

object BoardMark extends HasGenCodec[BoardMark] {

  case object Empty extends BoardMark

  case object ManualWater extends BoardMark

  case object ManualShip extends BoardMark

  case object Water extends BoardMark {
    override def isPermanent: Boolean = true
  }

  case object ShipHit extends BoardMark {
    override def isPermanent: Boolean = true
  }

}
