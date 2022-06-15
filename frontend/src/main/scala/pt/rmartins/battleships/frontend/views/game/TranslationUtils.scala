package pt.rmartins.battleships.frontend.views.game

import io.udash.bindings.modifiers.Binding
import io.udash.i18n.{LangProperty, TranslationProvider, translatedDynamic}
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.BonusType

object TranslationUtils {

  def bonusTypeToText(
      bonusType: BonusType
  )(implicit provider: TranslationProvider, lang: LangProperty): Binding =
    translatedDynamic(
      bonusType match {
        case BonusType.FirstBlood => Translations.PreGame.bonusFirstBlood
        case BonusType.DoubleKill => Translations.PreGame.bonusDoubleKill
        case BonusType.TripleKill => Translations.PreGame.bonusTripleKill
      }
    )(_.apply())

}
