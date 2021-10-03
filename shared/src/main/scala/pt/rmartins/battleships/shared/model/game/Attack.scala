package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Attack(attackType: AttackType, coordinateOpt: Option[Coordinate]) {

  def isPlaced: Boolean = coordinateOpt.nonEmpty

}

object Attack extends HasGenCodec[Attack]
