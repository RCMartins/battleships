package pt.rmartins.battleships.shared.i18n

import io.udash.i18n._

object Translations {
  import TranslationKey._

  /** The list of supported languages. */
  val langs = Seq("en", "pt")

  /** The list of translation bundles available in the `backend` resources. */
  val bundlesNames = Seq("auth", "chat", "game", "global")

  object Global {
    val unknownError = key("global.unknownError")
  }

  object Auth {
    val usernameFieldLabel = key("auth.username.label")
    val usernameFieldPlaceholder = key("auth.username.placeholder")
    val submitButton = key("auth.submit.label")

    val userNotFound = key("auth.user_not_found")
    val userAlreadyExists = key("auth.user_already_exists")
  }

  object Chat {
    val inputPlaceholder = key("chat.input.placeholder")
  }

  object Game {
    val chooseEnemyPlaceholder = key("game.pregame.choose_enemy_placeholder")
    val placeShips = key("game.header.place_ships")
    val placeShipsWaitEnemy = key("game.header.place_ships_wait_enemy")
    val placeShipsEnemyReady = key("game.header.place_ships_enemy_ready")
    val yourTurn = key("game.header.your_turn")
    val enemyTurn = key("game.header.enemy_turn")
    val youWon = key("game.header.you_won")
    val enemyWon = key("game.header.enemy_won")
    val loggedInAs = key("game.header.logged_in_as")
    val playingAgainst = key("game.header.playing_against")
    val turn = key("game.header.turn")
    val myTime = key("game.header.my_time")
    val enemyTime = key("game.header.enemy_time")

    val startGameVsBot = key("game.buttons.start_game_bot")
    val startGameVsPlayer = key("game.buttons.start_game_player")
    val rematchButton = key("game.buttons.rematch")
    val quitGameButton = key("game.buttons.quitGame")
    val logoutButton = key("game.buttons.logout")

    val confirmButton = key("game.buttons.confirm")
    val undoButton = key("game.buttons.undo")
    val resetButton = key("game.buttons.reset")
    val randomButton = key("game.buttons.random")
    val launchAttackButton = key("game.buttons.launch_attack")
    val queueAttackButton = key("game.buttons.queue_attack")
    val waitForTurnButton = key("game.buttons.wait_for_turn")
    val hideMyBoardButton = key("game.buttons.hide_my_board")
    val showMyBoardButton = key("game.buttons.show_my_board")
    val hideEnemyBoardButton = key("game.buttons.hide_enemy_board")
    val showEnemyBoardButton = key("game.buttons.show_enemy_board")
    val closeButton = key("game.buttons.close")

    val extraTurnPopup = key("game.popup.extra_turn")

    val myBoardTitle = key("game.board.my_board_title")
    val enemyBoardTitle = key("game.board.enemy_board_title")
    val realEnemyBoardTitle = key("game.board.real_enemy_board_title")

    val chatTab = key("game.tab.chat")
    val myMovesTab = key("game.tab.my_moves")
    val enemyMovesTab = key("game.tab.enemy_moves")
    val missesMoves = key("game.tab.missed_moves")
    val disabledMoves = key("game.tab.disabled_moves")

    val cannotStartGameTitle = key("game.modal.cannot_start_game_title")
    val startGameBoardSizeError = key("game.modal.start_game_board_size_error")
    val startGameEmptyFleetError = key("game.modal.start_game_empty_fleet_error")
    val confirmQuitGameTitle = key("game.modal.confirm_quit_game_title")

    val fleet = key("game.pregame.fleet")
    val options = key("game.pregame.options")
    val previewBoardTitle = key("game.pregame.previewBoardTitle")
    val timeLimit = key("game.pregame.timeLimit")
    val turnTimeLimit = key("game.pregame.turnTimeLimit")
  }
}
