package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class TimeRemaining(
    totalTimeRemainingMillis: Int,
    turnTimeRemainingMillisOpt: Option[Int]
)

object TimeRemaining extends HasGenCodec[TimeRemaining]
