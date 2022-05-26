package pt.rmartins.battleships.backend.services

import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit.WithRuleTimeLimit
import pt.rmartins.battleships.shared.model.game.{AttackType, BonusType, Fleet, Rules, TurnBonus}

object BattleshipsMocks {

  val default10v10Rules: Rules =
    Rules(
      boardSize = Fleet.default10By10._1,
      gameFleet = Fleet.default10By10._2,
      defaultTurnAttacks = List.fill(3)(AttackType.Simple),
      turnBonuses = List(
        TurnBonus(BonusType.FirstBlood, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
        TurnBonus(BonusType.DoubleKill, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
        TurnBonus(BonusType.TripleKill, List(ExtraTurn(List.fill(3)(AttackType.Simple))))
      ),
      timeLimit = WithRuleTimeLimit(
        initialTotalTimeSeconds = 600,
        additionalTurnTimeSeconds = Some((10, false))
      )
    )

}
