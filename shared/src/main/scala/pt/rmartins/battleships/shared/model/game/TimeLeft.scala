package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class TimeLeft(
    totalTimeLeftMillis: Int,
    turnTimeLeftMillisOpt: Option[Int]
)

object TimeLeft extends HasGenCodec[TimeLeft]
