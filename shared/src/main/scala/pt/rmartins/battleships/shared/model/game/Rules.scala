package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Rules(
    boardSize: Coordinate,
    gameFleet: Fleet,
    defaultTurnAttacks: List[AttackType],
    turnBonuses: List[TurnBonus],
    timeLimit: RuleTimeLimit
)

object Rules extends HasGenCodec[Rules] {

  implicit val rulesEncoder: JsonEncoder[Rules] =
    DeriveJsonEncoder.gen[Rules]

  implicit val rulesDecoder: JsonDecoder[Rules] =
    DeriveJsonDecoder.gen[Rules]

}
