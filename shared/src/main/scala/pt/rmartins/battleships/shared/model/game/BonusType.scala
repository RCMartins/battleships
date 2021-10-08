package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait BonusType

object BonusType extends HasGenCodec[BonusType] {

  case object FirstBlood extends BonusType

  case object DoubleKill extends BonusType

  case object TripleKill extends BonusType

}
