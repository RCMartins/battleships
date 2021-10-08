package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Username(username: String) {

  override def toString: String = username

}

object Username extends HasGenCodec[Username]
