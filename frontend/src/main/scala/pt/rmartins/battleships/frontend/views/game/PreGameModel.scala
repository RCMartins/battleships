package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit.WithRuleTimeLimit
import pt.rmartins.battleships.shared.model.game._

case class PreGameModel(
    enemyUsernameText: Username,
    invitedUsername: Option[Username],
    inviter: Option[(Username, PlayerInviteType)],
    inJoinedPreGame: Option[JoinedPreGame],
    rules: Rules,
    previewBoardOpt: Option[(Board, Int)] // TODO change int to double 0.0 to 1.0,
)

object PreGameModel extends HasModelPropertyCreator[PreGameModel] {

  val MaxPreviewTries: Int = 100
  val MinPreviewTriesPerc: Double = 0.15
  val WarningPreviewTriesPerc: Double = 0.75
  val MinPreviewTries: Int = Math.ceil(MinPreviewTriesPerc * MaxPreviewTries).toInt

  val default: PreGameModel =
    PreGameModel(
      enemyUsernameText = Username(""),
      invitedUsername = None,
      inviter = None,
      inJoinedPreGame = None,
      rules = Rules(
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
      ),
      previewBoardOpt = None
    )

}
