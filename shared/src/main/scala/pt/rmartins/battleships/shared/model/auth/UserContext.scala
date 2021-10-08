package pt.rmartins.battleships.shared.model.auth

import com.avsystem.commons.serialization.HasGenCodec
import io.udash.auth.{Permission => UdashPermission, PermissionId, UserCtx => UdashUserCtx}
import pt.rmartins.battleships.shared.model.game.Username

/** User data container. */
case class UserContext(token: UserToken, username: Username, permissions: Set[PermissionId])
    extends UdashUserCtx {
  override def has(permission: UdashPermission): Boolean = permissions.contains(permission.id)
  override def isAuthenticated: Boolean = true
}

object UserContext extends HasGenCodec[UserContext]
