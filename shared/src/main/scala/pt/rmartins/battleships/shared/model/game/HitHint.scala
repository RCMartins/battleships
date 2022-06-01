package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

sealed trait HitHint {

  def isWater: Boolean

  def isShip: Boolean = !isWater

  def isDestroyed: Boolean

  def shipIdOpt: Option[ShipId]

  def shipIdDestroyedOpt: Option[ShipId]

}

object HitHint extends HasGenCodec[HitHint] {

  implicit val hitHintEncoder: JsonEncoder[HitHint] =
    DeriveJsonEncoder.gen[HitHint]

  implicit val hitHintDecoder: JsonDecoder[HitHint] =
    DeriveJsonDecoder.gen[HitHint]

  case object Water extends HitHint {
    val isWater: Boolean = true
    val isDestroyed: Boolean = false
    val shipIdOpt: Option[ShipId] = None
    val shipIdDestroyedOpt: Option[ShipId] = None
  }

  case class ShipHit(shipId: ShipId, destroyed: Boolean) extends HitHint {
    val isWater: Boolean = false
    val isDestroyed: Boolean = destroyed
    def shipIdOpt: Option[ShipId] = Some(shipId)
    def shipIdDestroyedOpt: Option[ShipId] = Some(shipId).filter(_ => destroyed)
  }

}
