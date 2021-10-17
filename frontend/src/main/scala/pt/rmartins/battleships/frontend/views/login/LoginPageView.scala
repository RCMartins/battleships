package pt.rmartins.battleships.frontend.views.login

import com.avsystem.commons._
import io.udash._
import io.udash.bootstrap.alert.UdashAlert
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.form.UdashForm.FormEvent
import io.udash.bootstrap.form.{FormElementsFactory, UdashForm}
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.component.ComponentId
import io.udash.css._
import io.udash.i18n._
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.shared.css.LoginPageStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.Username
import scalatags.JsDom.all._

class LoginPageView(
    model: ModelProperty[LoginPageModel],
    presenter: LoginPagePresenter,
    translationsService: TranslationsService
) extends View
    with CssView {

  import translationsService._

  private val errorsAlert = UdashAlert(Color.Danger.toProperty, ComponentId("alerts"))(nested =>
    nested(repeat(model.subSeq(_.errors)) { error =>
      div(translatedDynamic(error.get)(_.apply())).render
    })
  )

//  private val infoIcon = span(
//    LoginPageStyles.infoIcon,
//    i(FontAwesome.Solid.infoCircle)
//  ).render
//
//  // infoIcon - translated popover
//  UdashPopover(
//    content = span(translated(Translations.Auth.info())).render,
//    trigger = Seq(UdashPopover.Trigger.Hover)
//  )(infoIcon)

  private def usernameInput(factory: FormElementsFactory) = {
    factory.input.formGroup(groupId = ComponentId("username"))(
      nested =>
        factory.input
          .textInput(model.subProp(_.username).bitransform(_.username)(Username(_)))(
            Some(nested =>
              nested(
                translatedAttrDynamic(Translations.Auth.usernameFieldPlaceholder, "placeholder")(
                  _.apply()
                )
              )
            )
          )
          .setup(nested)
          .render,
      labelContent = Some(nested =>
        Seq[Modifier](
          nested(translatedDynamic(Translations.Auth.usernameFieldLabel)(_.apply()))
        )
      )
    )
  }

  // Button from Udash Bootstrap wrapper
  private val submitButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    disabled = model.subProp(_.username).transform(_.username.isEmpty),
    componentId = ComponentId("login")
  )(nested =>
    Seq[Modifier](
      nested(translatedDynamic(Translations.Auth.submitButton)(_.apply())),
      tpe := "submit"
    )
  )

  def getTemplate: Modifier = div(
    LoginPageStyles.container,
    showIf(model.subProp(_.errors).transform(_.nonEmpty))(
      errorsAlert.render
    ),
    UdashForm(componentId = ComponentId("login-from"))(factory =>
      Seq(
        usernameInput(factory),

        // submit button or spinner
        showIfElse(model.subProp(_.waitingForResponse))(
          div(
            LoginPageStyles.textCenter,
            span(
              FontAwesome.Solid.spinner,
              FontAwesome.Modifiers.Animation.spin,
              FontAwesome.Modifiers.Sizing.x3
            )
          ).render,
          submitButton.render
        )
      )
    ).setup(_.listen { case FormEvent(_, FormEvent.EventType.Submit) =>
      if (!model.subProp(_.waitingForResponse).get)
        presenter.login()
    })
  )
}
