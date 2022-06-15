package pt.rmartins.battleships.shared.model.game

import com.avsystem.commons.serialization.HasGenCodec

case class PreGameRulesPatch(
    boardSizePatch: Option[Coordinate] = None,
    gameFleetPatch: Option[(ShipId, Int)] = None,
    defaultTurnAttacksPatch: Option[List[AttackType]] = None,
    gameBonusesPatch: Option[List[GameBonus]] = None,
    timeLimitPatch: Option[RuleTimeLimit] = None
) {
  def containsPatch: Boolean =
    List(
      boardSizePatch,
      gameFleetPatch,
      defaultTurnAttacksPatch,
      gameBonusesPatch,
      timeLimitPatch
    ).exists(_.nonEmpty)

}

object PreGameRulesPatch extends HasGenCodec[PreGameRulesPatch]
