package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Username(username: String) {

  def trim: Username = Username(username.trim)

  def toLowerCase: Username = Username(username.toLowerCase)

  override def toString: String = username

}

object Username extends HasGenCodec[Username]
