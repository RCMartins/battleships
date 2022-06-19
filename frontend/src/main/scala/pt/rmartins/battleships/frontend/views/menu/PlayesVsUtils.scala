package pt.rmartins.battleships.frontend.views.menu

import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n._
import io.udash.properties.single.Property
import io.udash._
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.{GamePresenter, PreGameModel}
import pt.rmartins.battleships.frontend.views.model.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.shared.i18n.Translations
import scalatags.JsDom.all._

class PlayesVsUtils(
    preGameModel: ModelProperty[PreGameModel],
    presenter: GamePresenter,
    translationsService: TranslationsService,
) extends CssView {

  import translationsService._

  val startGameVsBotButton: UdashButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("start-game-bot-button")
    )(nested =>
      Seq[Modifier](span(nested(translatedDynamic(Translations.Game.startGameVsBot)(_.apply()))))
    )

  startGameVsBotButton.listen { _ =>
    presenter.startGameWithBots()
  }

  val confirmRulesButton: UdashButton = {
    val isConfirmedProperty: ReadableProperty[Boolean] =
      preGameModel.subProp(_.inJoinedPreGame).transform {
        case Some(PlayingAgainstPlayer(_, true, _, _)) => true
        case _                                         => false
      }

    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = Property(true),
      componentId = ComponentId("confirm-rules-button"),
      disabled = isConfirmedProperty
    )(nested =>
      Seq[Modifier](
        span(nested(translatedDynamic(Translations.Game.confirmRulesButton)(_.apply())))
      )
    )
  }

  confirmRulesButton.listen { _ =>
    presenter.confirmRules()
  }

}
