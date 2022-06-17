package pt.rmartins.battleships.frontend.views

import io.udash._
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.{BootstrapStyles, UdashBootstrap}
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n.Lang
import pt.rmartins.battleships.BuildInfo
import pt.rmartins.battleships.frontend.routing.RoutingRootState
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.shared.css.GlobalStyles
import pt.rmartins.battleships.shared.i18n.Translations
import scalatags.JsDom.all._

class RootViewFactory(translationsService: TranslationsService)
    extends StaticViewFactory[RoutingRootState.type](() => new RootView(translationsService))

class RootView(translationsService: TranslationsService) extends ContainerView with CssView {

  private def langChangeButton(lang: Lang): Modifier = {
    val btn = UdashButton(
      buttonStyle = Color.Link.toProperty,
      componentId = ComponentId(s"lang-btn-${lang.lang}")
    )(_ => lang.lang.toUpperCase())

    btn.listen { case UdashButton.ButtonClickEvent(_, _) =>
      translationsService.setLanguage(lang)
    }

    btn.render
  }

  // ContainerView contains default implementation of child view rendering
  // It puts child view into `childViewContainer`
  override def getTemplate: Modifier = div(
    // loads Bootstrap and FontAwesome styles from CDN
    UdashBootstrap.loadBootstrapStyles(),
    UdashBootstrap.loadFontAwesome(),
    BootstrapStyles.containerFluid,
    div(
      GlobalStyles.floatRight,
      span(`class` := "small text-black-50 mr-5", s"v${BuildInfo.version}"),
      Translations.langs.map(v => langChangeButton(Lang(v)))
    ),
    h2(`class` := "text-center", "battleships"),
    childViewContainer
  )
}
