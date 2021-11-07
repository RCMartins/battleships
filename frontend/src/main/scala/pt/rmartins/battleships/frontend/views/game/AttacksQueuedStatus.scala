package pt.rmartins.battleships.frontend.views.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait AttacksQueuedStatus

object AttacksQueuedStatus extends HasGenCodec[AttacksQueuedStatus] {

  case object NotSet extends AttacksQueuedStatus

  case object Queued extends AttacksQueuedStatus

  case object Sent extends AttacksQueuedStatus

}
