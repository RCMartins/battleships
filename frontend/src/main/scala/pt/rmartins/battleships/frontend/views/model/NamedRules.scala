package pt.rmartins.battleships.frontend.views.model

import pt.rmartins.battleships.shared.model.game.Rules
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class NamedRules(name: String, rules: Rules)

object NamedRules {

  val MaxNamedRulesLength = 30

  implicit val namedRulesEncoder: JsonEncoder[NamedRules] =
    DeriveJsonEncoder.gen[NamedRules]

  implicit val namedRulesDecoder: JsonDecoder[NamedRules] =
    DeriveJsonDecoder.gen[NamedRules]

}
