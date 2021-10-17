package pt.rmartins.battleships.frontend.views.game

import org.scalajs.dom.document
import pt.rmartins.battleships.shared.model.auth.UserToken
import pt.rmartins.battleships.shared.model.game.Username
import sttp.model.Cookie

object Cookies {

  private val UserTokenKey = "userToken"
  private val UsernameKey = "username"

  def saveCookieData(userToken: UserToken, username: Username): Unit = {
    Globals.setCookie(Cookie.apply(UserTokenKey, userToken.token).toString())
    Globals.setCookie(Cookie.apply(UsernameKey, username.username).toString())
  }

  def getCookieData(): Option[(UserToken, Username)] = {
    Cookie.parse(document.cookie) match {
      case Left(_) =>
        None
      case Right(cookiesList) =>
        (cookiesList.find(_.name == UserTokenKey), cookiesList.find(_.name == UsernameKey)) match {
          case (Some(Cookie(_, tokenCookie)), Some(Cookie(_, usernameCookie)))
              if tokenCookie.nonEmpty && usernameCookie.nonEmpty =>
            Some((UserToken(tokenCookie), Username(usernameCookie)))
          case _ =>
            None
        }
    }
  }

  def clearCookies(): Unit = {
    Globals.setCookie(Cookie.apply(UserTokenKey, "").toString())
    Globals.setCookie(Cookie.apply(UsernameKey, "").toString())
  }

}
