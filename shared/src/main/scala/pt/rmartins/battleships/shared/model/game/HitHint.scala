package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait HitHint {

  def isWater: Boolean

  def isShip: Boolean = !isWater

  def isDestroyed: Boolean

}

object HitHint extends HasGenCodec[HitHint] {

  case object Water extends HitHint {
    val isWater: Boolean = true
    val isDestroyed: Boolean = false
  }

  case class ShipHit(shipId: Int, destroyed: Boolean) extends HitHint {
    val isWater: Boolean = false
    val isDestroyed: Boolean = destroyed
  }

}
