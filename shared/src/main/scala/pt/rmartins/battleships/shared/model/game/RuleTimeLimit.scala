package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

sealed trait RuleTimeLimit {

  def toOption: Option[RuleTimeLimit.WithRuleTimeLimit] =
    this match {
      case RuleTimeLimit.WithoutRuleTimeLimit =>
        None
      case withRuleTimeLimit: RuleTimeLimit.WithRuleTimeLimit =>
        Some(withRuleTimeLimit)
    }

}

object RuleTimeLimit extends HasGenCodec[RuleTimeLimit] {

  case object WithoutRuleTimeLimit extends RuleTimeLimit

  case class WithRuleTimeLimit(
      initialTotalTimeSeconds: Int,
      additionalTurnTimeSeconds: Option[
        (Int, Boolean)
      ] // TODO create new trait for this type? (shouldTurnTimeAccumulate: Boolean)
  ) extends RuleTimeLimit

}
