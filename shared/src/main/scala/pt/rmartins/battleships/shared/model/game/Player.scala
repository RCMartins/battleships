package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class Player(clientId: String, username: String, shipsLeftToPlace: List[Ship], myBoard: Board)

object Player extends HasGenCodec[Player]
