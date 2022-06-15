package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import org.scalajs.dom.html.Div
import pt.rmartins.battleships.frontend.views.model.{JoinedPreGame, NamedRules}
import pt.rmartins.battleships.shared.model.game.BonusReward.ExtraTurn
import pt.rmartins.battleships.shared.model.game.Fleet.fromShips
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit.WithRuleTimeLimit
import pt.rmartins.battleships.shared.model.game._
import scalatags.JsDom.all.div

import scala.collection.immutable.SortedMap

case class PreGameModel(
    enemyUsernameText: Username,
    invitedUsername: Option[Username],
    inviter: Option[(Username, PlayerInviteType)],
    inJoinedPreGame: Option[JoinedPreGame],
    rules: Rules,
    previewEnabled: Boolean,
    previewBoardOpt: Option[(Board, Int)],
    customNamedRulesMap: SortedMap[String, NamedRules],
    selectedNamedRule: Option[NamedRules],
    selectedNamedRuleChanges: Boolean,
    editGameBonusType: BonusType,
    editGameBonusRewards: List[BonusReward],
    editGameBonusDiv: Div
) {

  def validatedEditGameBonusRewards: List[BonusReward] =
    PreGameModel.validateEditGameBonusRewards(editGameBonusRewards)

}

object PreGameModel extends HasModelPropertyCreator[PreGameModel] {

  def validateEditGameBonusRewards(editGameBonusRewards: List[BonusReward]): List[BonusReward] =
    editGameBonusRewards.filter {
      case BonusReward.ExtraTurn(Nil) => false
      case _                          => true
    }

  val MaxPreviewTries: Int = 100
  val MinPreviewTriesPerc: Double = 0.15
  val WarningPreviewTriesPerc: Double = 0.75
  val MinPreviewTries: Int = Math.ceil(MinPreviewTriesPerc * MaxPreviewTries).toInt

  private val default10By10: (String, Coordinate, Fleet, Rules => Rules) =
    (
      "Standard 10x10",
      Coordinate.square(10),
      fromShips(
        List(
          Ship.Submarine -> 4,
          Ship.Skeeter -> 3,
          Ship.Ranger -> 2,
          Ship.Conqueror -> 1,
          Ship.AircraftCarrier -> 1
        )
      ),
      (_: Rules).copy(timeLimit =
        WithRuleTimeLimit(
          initialTotalTimeSeconds = 600,
          additionalTurnTimeSeconds = Some((10, false))
        )
      )
    )

  private val default15By15: (String, Coordinate, Fleet, Rules => Rules) =
    (
      "Big 15x15",
      Coordinate.square(15),
      fromShips(
        List(
          Ship.Skeeter -> 4,
          Ship.Ranger -> 3,
          Ship.Conqueror -> 2,
          Ship.TorpedoBoat -> 2,
          Ship.Cruiser -> 2,
          Ship.Epoch -> 1,
          Ship.Battleship -> 1
        )
      ),
      (_: Rules).copy(timeLimit =
        WithRuleTimeLimit(
          initialTotalTimeSeconds = 1200,
          additionalTurnTimeSeconds = Some((15, false))
        )
      )
    )

  val allDefaultNamedRules: List[NamedRules] =
    List(default10By10, default15By15).map { case (name, boardSize, gameFleet, ruleModifier) =>
      NamedRules(
        name,
        ruleModifier(
          Rules(
            boardSize = boardSize,
            gameFleet = gameFleet,
            defaultTurnAttacks = List.fill(3)(AttackType.Simple),
            turnBonuses = List(
              TurnBonus(BonusType.FirstBlood, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
              TurnBonus(BonusType.DoubleKill, List(ExtraTurn(List.fill(1)(AttackType.Simple)))),
              TurnBonus(BonusType.TripleKill, List(ExtraTurn(List.fill(3)(AttackType.Simple))))
            ),
            timeLimit = RuleTimeLimit.WithoutRuleTimeLimit
          )
        )
      )
    }

  val default: PreGameModel =
    PreGameModel(
      enemyUsernameText = Username(""),
      invitedUsername = None,
      inviter = None,
      inJoinedPreGame = None,
      rules = allDefaultNamedRules.head.rules,
      previewEnabled = true,
      previewBoardOpt = None,
      customNamedRulesMap = SortedMap.empty,
      selectedNamedRule = Some(allDefaultNamedRules.head),
      selectedNamedRuleChanges = false,
      editGameBonusType = BonusType.FirstBlood,
      editGameBonusRewards = Nil,
      editGameBonusDiv = div.render
    )

}
