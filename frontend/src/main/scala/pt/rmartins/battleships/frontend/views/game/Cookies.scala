package pt.rmartins.battleships.frontend.views.game

import org.scalajs.dom.document
import pt.rmartins.battleships.frontend.views.model.NamedRules
import pt.rmartins.battleships.shared.model.auth.UserToken
import pt.rmartins.battleships.shared.model.game.Username
import sttp.model.headers.Cookie

object Cookies {

  private val UserTokenKey = "userToken"
  private val UsernameKey = "username"
  private val FleetCounterKey = "fleetsCounter"
  private def fleetsKey(n: Int): String = s"fleets-$n"

  def saveLoginCookieData(userToken: UserToken, username: Username): Unit = {
    Globals.setCookie(Cookie.apply(UserTokenKey, userToken.token).toString())
    Globals.setCookie(Cookie.apply(UsernameKey, username.username).toString())
  }

  def getLoginCookieData(): Option[(UserToken, Username)] =
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

  def clearLoginCookieData(): Unit = {
    Globals.setCookie(Cookie.apply(UserTokenKey, "").toString())
    Globals.setCookie(Cookie.apply(UsernameKey, "").toString())
  }

  def getAllNamedRulesCookieData(): List[NamedRules] =
    Cookie.parse(document.cookie) match {
      case Left(_) =>
        Nil
      case Right(cookiesList) =>
        val cookiesMap: Map[String, Cookie] =
          cookiesList.map(cookie => cookie.name -> cookie).toMap

        def loadNamedRules(namedRulesCookie: String): Option[NamedRules] =
          NamedRules.namedRulesDecoder.decodeJson(namedRulesCookie) match {
            case Left(error) =>
              println(error)
              None
            case Right(namedRules) =>
              Some(namedRules)
          }

        cookiesMap.get(FleetCounterKey) match {
          case Some(Cookie(_, fleetCounterCookie)) if fleetCounterCookie.toIntOption.nonEmpty =>
            (0 until fleetCounterCookie.toInt).flatMap { index =>
              cookiesMap
                .get(fleetsKey(index))
                .flatMap(cookie => loadNamedRules(cookie.value))
            }.toList
          case _ =>
            Nil
        }
    }

  def saveAllNamedRulesCookieData(namedRulesList: List[NamedRules]): Unit = {
    Globals.setCookie(Cookie.apply(FleetCounterKey, namedRulesList.size.toString).toString())

    namedRulesList.zipWithIndex.foreach { case (namedRules, index) =>
      Globals.setCookie(
        Cookie
          .apply(
            fleetsKey(index),
            NamedRules.namedRulesEncoder.encodeJson(namedRules, None).toString
          )
          .toString()
      )
    }
  }

}
