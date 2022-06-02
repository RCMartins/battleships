package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class TurnPlay(
    turn: Turn,
    turnAttacks: List[Attack],
    hitHints: List[HitHint]
)

object TurnPlay extends HasGenCodec[TurnPlay] {

  implicit val turnPlayEncoder: JsonEncoder[TurnPlay] =
    DeriveJsonEncoder.gen[TurnPlay]

  implicit val turnPlayDecoder: JsonDecoder[TurnPlay] =
    DeriveJsonDecoder.gen[TurnPlay]

}
