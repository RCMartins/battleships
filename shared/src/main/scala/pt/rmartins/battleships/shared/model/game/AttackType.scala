package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait AttackType

object AttackType extends HasGenCodec[AttackType] {

  case object Simple extends AttackType

  case object Radar extends AttackType

}
