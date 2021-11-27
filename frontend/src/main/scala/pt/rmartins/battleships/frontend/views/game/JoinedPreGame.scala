package pt.rmartins.battleships.frontend.views.game

import com.avsystem.commons.serialization.HasGenCodec
import pt.rmartins.battleships.shared.model.game._

sealed trait JoinedPreGame {

  def gameIdOpt: Option[GameId]

  def enemyUsernameOpt: Option[Username]

}

object JoinedPreGame extends HasGenCodec[JoinedPreGame] {

  case object PlayingAgainstBot extends JoinedPreGame {

    override def gameIdOpt: Option[GameId] = None

    override def enemyUsernameOpt: Option[Username] = None

  }

  case class PlayingAgainstPlayer(
      gameId: GameId,
      confirmed: Boolean,
      enemyConfirmed: Boolean,
      enemyUsername: Username
  ) extends JoinedPreGame {

    override def gameIdOpt: Option[GameId] = Some(gameId)

    override def enemyUsernameOpt: Option[Username] = Some(enemyUsername)

  }

}
