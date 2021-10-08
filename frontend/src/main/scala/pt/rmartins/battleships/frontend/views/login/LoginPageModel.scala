package pt.rmartins.battleships.frontend.views.login

import io.udash._
import io.udash.i18n.TranslationKey0
import pt.rmartins.battleships.shared.model.game.Username

/** The form's model structure. */
case class LoginPageModel(
    username: Username,
    password: String,
    waitingForResponse: Boolean,
    errors: Seq[TranslationKey0]
)
object LoginPageModel extends HasModelPropertyCreator[LoginPageModel]
