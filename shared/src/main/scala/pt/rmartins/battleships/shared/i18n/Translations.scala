package pt.rmartins.battleships.shared.i18n

import io.udash.i18n._

object Translations {
  import TranslationKey._

  /** The list of supported languages. */
  val langs = Seq("en")

  /** The list of translation bundles available in the `backend` resources. */
  val bundlesNames = Seq("auth", "game", "chat", "global")

  object Global {
    val unknownError = key("global.unknownError")
  }

  object Auth {
    val usernameFieldLabel = key("auth.username.label")
    val usernameFieldPlaceholder = key("auth.username.placeholder")
    val passwordFieldLabel = key("auth.password.label")
    val passwordFieldPlaceholder = key("auth.password.placeholder")
    val submitButton = key("auth.submit.label")

    val userNotFound = key("auth.user_not_found")
    val info = key("auth.info")

    val randomPermissionsInfo = key("auth.random_permissions")
  }

  object Chat {
    // notice that this translation takes a single Int argument
    val connections = key1[Int]("chat.connections")
    val inputPlaceholder = key("chat.input.placeholder")
  }

  object Game {
    val placeShips = key("game.header.place_ships")
    val placeShipsWaitEnemy = key("game.header.place_ships_wait_enemy")
    val placeShipsEnemyReady = key("game.header.place_ships_enemy_ready")
    val gameTurn = key1[Int]("game.header.game_turn")
    val yourTurn = key("game.header.your_turn")
    val enemyTurn = key("game.header.enemy_turn")
    val youWon = key("game.header.you_won")
    val enemyWon = key("game.header.enemy_won")

    val restartButton = key("game.buttons.restart")

    val extraTurnPopup = key("game.popup.extra_turn")
  }
}
