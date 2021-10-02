package pt.rmartins.battleships.frontend.views.login

import pt.rmartins.battleships.frontend.routing.{
  RoutingInGameState,
  RoutingLoginPageState,
  RoutingState
}
import pt.rmartins.battleships.frontend.services.UserContextService
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.auth.{UserContext, UserToken}
import pt.rmartins.battleships.shared.model.SharedExceptions
import io.udash.Application
import io.udash.i18n.TranslationKey
import io.udash.properties.model.ModelProperty
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future

class LoginPageTest extends AsyncWordSpec with Matchers with AsyncMockFactory {
  private class MockableApplication extends Application[RoutingState](null, null, null)

  "LoginPage" should {
    "redirect to the chat view if user is already authenticated" in {
      val userService = mock[UserContextService]
      (() => userService.currentContext)
        .expects()
        .returning(Some(UserContext(UserToken("t1"), "name", Set.empty)))

      val application = mock[MockableApplication]
      (application.goTo _).expects(RoutingInGameState, false).once()

      val model = ModelProperty(LoginPageModel("", "", false, Seq.empty))
      val presenter = new LoginPagePresenter(model, userService, application)

      presenter.handleState(RoutingLoginPageState) should be(())
    }

    "pass login request to the user service" in {
      val userService = mock[UserContextService]
      (userService.login _)
        .expects("u1", "p1")
        .returning(Future.successful(UserContext(UserToken("t1"), "name", Set.empty)))

      val application = mock[MockableApplication]
      (application.goTo _).expects(RoutingInGameState, false).once()

      val model =
        ModelProperty(LoginPageModel("u1", "p1", false, Seq(TranslationKey.untranslatable("Bla"))))
      val presenter = new LoginPagePresenter(model, userService, application)

      for {
        _ <- presenter.login()
      } yield {
        val m = model.get
        m.errors should be(Seq.empty)
      }
    }

    "handle login request errors" in {
      val userService = mock[UserContextService]
      (userService.login _)
        .expects("u5", "p3")
        .returning(Future.failed(new SharedExceptions.UserNotFound))

      val application = mock[MockableApplication]
      val model = ModelProperty(LoginPageModel("u1", "p3", false, Seq.empty))
      val presenter = new LoginPagePresenter(model, userService, application)

      model.subProp(_.username).set("u5")
      for {
        _ <- recoverToSucceededIf[SharedExceptions.UserNotFound](presenter.login())
      } yield {
        val m = model.get
        m.errors should be(Seq(Translations.Auth.userNotFound))
      }
    }
  }
}
