package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait BoardMark {

  def isPermanent: Boolean = false

}

object BoardMark extends HasGenCodec[BoardMark] {

  case object Empty extends BoardMark

  case object ManualWater extends BoardMark

  case object ManualShip extends BoardMark

  case object ManualQuestionShip extends BoardMark

  case object ManualQuestionWater extends BoardMark

  case object Miss extends BoardMark {
    override def isPermanent: Boolean = true
  }

  case object ShipHit extends BoardMark {
    override def isPermanent: Boolean = true
  }

}
