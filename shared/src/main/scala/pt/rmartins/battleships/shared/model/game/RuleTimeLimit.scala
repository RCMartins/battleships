package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class RuleTimeLimit(
    initialTotalTimeSeconds: Int,
    additionalTurnTimeSeconds: Option[
      (Int, Boolean)
    ] // TODO create new trait for this type? (shouldTurnTimeAccumulate: Boolean)
)

object RuleTimeLimit extends HasGenCodec[RuleTimeLimit]
