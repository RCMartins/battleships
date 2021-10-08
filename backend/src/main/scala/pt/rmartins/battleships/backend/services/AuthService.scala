package pt.rmartins.battleships.backend.services

import com.avsystem.commons._
import pt.rmartins.battleships.shared.model.SharedExceptions
import pt.rmartins.battleships.shared.model.auth.{Permission, UserContext, UserToken}
import pt.rmartins.battleships.shared.model.game.Username

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class AuthService(usersData: JList[String]) {
  // data is provided in configuration file
  // every user is described by a separated string in format: <username>:<password>
  private val usersWithPasswords: Map[Username, String] =
    usersData.asScala.map { case s"$user:$pass" => Username(user) -> pass }.toMap

  private val tokens: MMap[UserToken, UserContext] = MMap.empty

  /** Tries to authenticate user with provided credentials. */
  def login(username: Username, password: String): Future[UserContext] = Future {
    if (usersWithPasswords.get(username).contains(password)) {
      val token = UserToken(UUID.randomUUID().toString)
      val ctx = UserContext(
        token,
        username,
        // on every login user gets random set of permissions
//        Permission.values.iterator.filter(_ => Random.nextBoolean()).map(_.id).toSet
        Permission.values.iterator.map(_.id).toSet
      )

      tokens.synchronized { tokens(token) = ctx }

      ctx
    } else
      throw SharedExceptions.UserNotFound()
  }

  def findUserCtx(userToken: UserToken): Option[UserContext] =
    tokens.synchronized { tokens.get(userToken) }
}
