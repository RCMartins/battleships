package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class PuzzleId(id: String)

object PuzzleId extends HasGenCodec[PuzzleId]
