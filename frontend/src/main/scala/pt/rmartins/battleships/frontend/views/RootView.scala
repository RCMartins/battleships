package pt.rmartins.battleships.frontend.views

import io.udash._
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.bootstrap.{BootstrapStyles, UdashBootstrap}
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n.{Lang, translatedDynamic}
import pt.rmartins.battleships.BuildInfo
import pt.rmartins.battleships.frontend.routing.RoutingRootState
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.Globals
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.css.GlobalStyles
import pt.rmartins.battleships.shared.i18n.Translations
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.nav

class RootViewFactory(translationsService: TranslationsService)
    extends StaticViewFactory[RoutingRootState.type](() => new RootView(translationsService))

class RootView(translationsService: TranslationsService) extends ContainerView with CssView {

  import translationsService._

  // ContainerView contains default implementation of child view rendering
  // It puts child view into `childViewContainer`
  override def getTemplate: Modifier =
    div(
      // loads Bootstrap and FontAwesome styles from CDN
      UdashBootstrap.loadBootstrapStyles(),
      link(
        rel := "stylesheet",
        href := "https://use.fontawesome.com/releases/v6.1.1/css/all.css"
      ).render,
      childViewContainer
    )
//    div(
//      `class` := "row m-0 p-0 vh-100",
//      // loads Bootstrap and FontAwesome styles from CDN
//      UdashBootstrap.loadBootstrapStyles(),
//      UdashBootstrap.loadFontAwesome(),
//      BootstrapStyles.containerFluid,
//      div(
//        `class` := "navbar col-12",
//        button(
//          `class` := "navbar-toggler",
//          `type` := "button",
//          attr("data-toggle") := "collapse",
//          attr("data-target") := "#navbarToggleExternalContent",
//          span(FontAwesome.Solid.bars, FontAwesome.Modifiers.Sizing.x2)
//        ),
//        h2(`class` := "text-center", "battleships"),
//        div(
//          GlobalStyles.floatRight,
//          span(`class` := "small text-black-50 mr-5", s"v${BuildInfo.version}"),
//          Translations.langs.map(v => langChangeButton(Lang(v)))
//        )
//      ),
//      div(
//        `class` := "col-12",
//        childViewContainer
//      )
//    )

}
