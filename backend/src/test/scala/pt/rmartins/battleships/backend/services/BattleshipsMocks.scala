package pt.rmartins.battleships.backend.services

import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.Fleet.fromShips
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit.WithRuleTimeLimit
import pt.rmartins.battleships.shared.model.game._

object BattleshipsMocks {

  val default10v10Rules: Rules =
    Rules(
      boardSize = Coordinate.square(10),
      gameFleet = fromShips(
        List(
          Ship.Submarine -> 4,
          Ship.Skeeter -> 3,
          Ship.Ranger -> 2,
          Ship.Conqueror -> 1,
          Ship.AircraftCarrier -> 1
        )
      ),
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
