package pt.rmartins.battleships.frontend.views.game

import com.avsystem.commons.universalOps
import io.udash._
import io.udash.bindings.modifiers.Binding
import io.udash.bootstrap.{BootstrapStyles, UdashBootstrap}
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.form.UdashForm.FormEvent
import io.udash.bootstrap.form.{FormElementsFactory, UdashForm, UdashInputGroup}
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.component.ComponentId
import io.udash.css._
import io.udash.i18n._
import org.scalajs.dom
import org.scalajs.dom.html.{Canvas, Div, Input, LI}
import org.scalajs.dom.{html, _}
import pt.rmartins.battleships.BuildInfo
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.frontend.views.menu._
import pt.rmartins.battleships.frontend.views.model.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.frontend.views.model.ModeType._
import pt.rmartins.battleships.frontend.views.model.{AttacksQueuedStatus, MenuState}
import pt.rmartins.battleships.shared.css.{ChatStyles, GlobalStyles}
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode._
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils._
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class GameView(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    gameStateModel: ModelProperty[GameStateModel],
    chatModel: ModelProperty[ChatModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    presenter: GamePresenter,
    translationsService: TranslationsService,
    playerVsUtils: PlayerVsUtils,
    playerVsBotsView: PlayerVsBotsView,
    playerVsPlayerView: PlayerVsPlayerView,
    puzzlesView: PuzzlesView,
    gameModals: GameModals,
    viewUtils: ViewUtils
) extends View
    with CssView {

  import translationsService._

//  private def reloadBoardView(): Unit = {
//    if (!presenter.onCanvasResize(boardView)) {
//      window.setTimeout(
//        () => boardView.paint(),
//        1
//      )
//    }
//  }

//  gameStateModel.listen(_ => reloadBoardView())
//
//  screenModel.subProp(_.canvasSize).listen(_ => reloadBoardView())
//  screenModel.subProp(_.missilesPopupMillisOpt).listen(_ => reloadBoardView())
//  screenModel.subProp(_.extraTurnPopup).listen(_ => reloadBoardView())
//  screenModel.subProp(_.screenResized).listen(_ => reloadBoardView())
//  screenModel.subProp(_.hoverMove).listen(_ => reloadBoardView())
//  screenModel.subProp(_.tick).listen(_ => reloadBoardView())
//  translationsService.currentLangProperty.listen(_ => reloadBoardView())
//  translationsModel.listen(_ => reloadBoardView())
//
//  gameModel.subProp(_.mousePosition).listen(_ => reloadBoardView())
//  gameModel.subProp(_.mouseDown).listen(_ => reloadBoardView())
//  gameModel.subProp(_.selectedShip).listen(_ => reloadBoardView())
//  gameModel.subProp(_.turnAttacks).listen(_ => reloadBoardView())
//  gameModel.subProp(_.turnAttacksQueuedStatus).listen(_ => reloadBoardView())
//  gameModel.subProp(_.selectedAction).listen(_ => reloadBoardView())
//  gameModel.subProp(_.lineDashOffset).listen(_ => reloadBoardView())

  private val invitePlayerButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("invite-player-button")
  )(nested =>
    Seq[Modifier](span(nested(translatedDynamic(Translations.Game.invitePlayerButton)(_.apply()))))
  )

  private val cancelPlayerInviteButton = UdashButton(
    buttonStyle = Color.Danger.toProperty,
    block = true.toProperty,
    componentId = ComponentId("cancel-invite-player-button")
  )(nested =>
    Seq[Modifier](
      span(nested(translatedDynamic(Translations.Game.cancelInvitePlayerButton)(_.apply())))
    )
  )

  private val cancelRulesButton: html.Element =
    UdashButton(
      buttonStyle = Color.Danger.toProperty,
      block = true.toProperty,
      componentId = ComponentId("cancel-rules-button")
    )(_ => Seq[Modifier](`class` := "invisible", span(FontAwesome.Solid.times).render)).render

  preGameModel
    .subProp(_.inJoinedPreGame)
    .transform(_.collect { case PlayingAgainstPlayer(_, confirmed, _, _) =>
      confirmed
    })
    .listen(
      {
        case Some(true) =>
          cancelRulesButton.classList.remove("invisible")
          cancelRulesButton.classList.add("visible")
        case _ =>
          cancelRulesButton.classList.remove("visible")
          cancelRulesButton.classList.add("invisible")
      },
      initUpdate = true
    )

  cancelRulesButton.onclick = _ => {
    presenter.cancelRules()
  }

  private def usernameInput(factory: FormElementsFactory): Element =
    factory.input
      .formGroup(groupId = ComponentId("username")) { nested =>
        factory.input
          .textInput(
            preGameModel.subProp(_.enemyUsernameText).bitransform(_.username)(Username(_))
          )(
            Some(nested =>
              nested(
                translatedAttrDynamic(Translations.Game.chooseEnemyPlaceholder, "placeholder")(
                  _.apply()
                )
              )
            )
          )
          .setup(nested)
          .render
      }
      .render
      .tap(_.classList.add("my-0"))
      .tap { elem =>
        elem.asInstanceOf[Input].onkeypress = (event: KeyboardEvent) => {
          if (event.key.equalsIgnoreCase("Enter")) {
            presenter.invitePlayer(preGameModel.subProp(_.enemyUsernameText).get)
          }
        }
      }

  private val solvePuzzleButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("solve-puzzle-button")
  )(nested =>
    Seq[Modifier](span(nested(translatedDynamic(Translations.Game.solvePuzzleButton)(_.apply()))))
  )

  private val addEnemyTimeButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("add-enemy-time-button")
    )(_ =>
      Seq[Modifier](`class` := "btn-sm col-4 p-0 ml-2", span(FontAwesome.Solid.plus, " 30s").render)
    )

  private val revealEnemyBoardButton =
    UdashButton(
      buttonStyle = screenModel.subProp(_.revealEnemyBoard).transform {
        case true  => Color.Secondary
        case false => Color.Primary
      },
      block = true.toProperty,
      componentId = ComponentId("hide-enemy-board-button")
    )(nested =>
      Seq(nested(produceWithNested(screenModel.subProp(_.revealEnemyBoard)) {
        case (true, nested) =>
          span(nested(translatedDynamic(Translations.Game.hideEnemyBoardButton)(_.apply()))).render
        case (false, nested) =>
          span(nested(translatedDynamic(Translations.Game.showEnemyBoardButton)(_.apply()))).render
      }))
    )

  private val rematchButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("rematch-button")
    )(nested => Seq(nested(translatedDynamic(Translations.Game.rematchButton)(_.apply()))))

  private val mainGameForm = UdashForm(
    componentId = ComponentId("main-game-from")
  )(factory =>
    factory.disabled(Property(false))(nested =>
      nested(
        produceWithNested(
          combine(
            preGameModel.subProp(_.invitedUsername),
            preGameModel.subProp(_.inJoinedPreGame),
            presenter.modeTypeProperty,
            presenter.placingShipsModeProperty,
            presenter.gamePuzzleStateProperty
          )
        ) {
          case ((Some(Username(invitedUsername)), _, _, _, _), nested) =>
            div(
              `class` := "row mx-0",
              div(`class` := "mr-2", cancelPlayerInviteButton),
              div(
                `class` := "mt-1",
                nested(translatedDynamic(Translations.Game.waitingPlayerInvitation)(_.apply())),
                span(" (", b(invitedUsername), ")")
              )
            ).render
          case ((_, Some(_), _, _, _), nested) =>
            div(
              `class` := "row mx-0",
              div(`class` := "ml-2", cancelRulesButton),
//              div(`class` := "mx-2", confirmRulesButton),
              // TODO why using produceWithNested here if we already have inJoinedPreGame value?
              nested(produceWithNested(preGameModel.subProp(_.inJoinedPreGame)) {
                case (Some(PlayingAgainstPlayer(_, _, true, _)), nested) =>
                  div(
                    `class` := "mx-3 mt-1",
                    nested(translatedDynamic(Translations.PreGame.enemyAcceptedRules)(_.apply()))
                  ).render
                case _ =>
                  div.render
              })
            ).render
          case ((_, _, _, _, Some(GamePuzzleState(_, _, _, _, Some(_)))), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(
                `class` := "row mx-0",
//                div(`class` := "mx-2", sendPuzzleSolutionButton)
              ),
//              div(`class` := "mx-2", nextPuzzleButton)
            ).render
          case ((_, _, _, _, Some(_)), _) =>
            div(
              `class` := "row mx-0",
//              div(`class` := "mx-2", sendPuzzleSolutionButton)
            ).render
          case ((_, _, None, _, _), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(
                `class` := "row mx-0",
//                div(`class` := "mx-2", startGameVsBotButton),
                div(`class` := "ml-2", invitePlayerButton),
                usernameInput(factory)
              ),
              div(`class` := "mx-2", solvePuzzleButton)
            ).render
          case ((_, _, _, Some(PlacingShipsMode(false, _)), _), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(
                `class` := "row mx-0",
//                div(`class` := "mx-2", confirmShipsButton),
//                div(`class` := "mx-2", undoButton),
//                div(`class` := "mx-2", resetButton),
//                div(`class` := "mx-2", randomPlacementButton)
              ),
//              div(`class` := "mx-2", editRulesButton)
            ).render
          case ((_, _, _, Some(PlacingShipsMode(true, _)), _), _) =>
            div(
              `class` := "row mx-0",
//              div(`class` := "mx-2", undoButton),
//              div(`class` := "mx-2", resetButton)
            ).render
          case ((_, _, Some(PlayingModeType), _, _), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(
                `class` := "row mx-0",
//                div(`class` := "ml-2", cancelQueuedAttacksButton),
//                div(`class` := "mx-1", launchAttackButton)
              ),
//              div(`class` := "mx-2", hideMyBoardButton)
            ).render
          case ((_, _, Some(GameOverModeType), _, _), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(`class` := "mx-2", rematchButton),
              div(`class` := "mx-2", revealEnemyBoardButton)
            ).render
          case _ =>
            span.render
        }
      )
    )
  )

  invitePlayerButton.listen { _ =>
    presenter.invitePlayer(preGameModel.subProp(_.enemyUsernameText).get)
  }

  cancelPlayerInviteButton.listen { _ =>
    preGameModel.subProp(_.invitedUsername).set(None)
  }

  solvePuzzleButton.listen { _ =>
    presenter.startNewPuzzle()
  }

  rematchButton.listen { _ =>
    presenter.rematchGame()
  }

  revealEnemyBoardButton.listen { _ =>
    screenModel.subProp(_.revealEnemyBoard).set(!screenModel.get.revealEnemyBoard)
  }

  addEnemyTimeButton.listen { _ =>
    presenter.addToEnemyTimeSeconds(30)
  }

  screenModel.subProp(_.errorModalType).listen {
    case Some(_) =>
      Globals.modalToggle(gameModals.errorModalId)
    case None =>
  }

  screenModel.subProp(_.newTurn).listen { _ =>
    Option(document.getElementById(ScreenModel.myMovesTab)).foreach(_.scrollTop = 0)
  }

  screenModel.subProp(_.receiveEditRequest).listen {
    case None =>
      Globals.modalHide(gameModals.editRulesModalId)
    case Some(()) =>
      Globals.modalToggle(gameModals.editRulesModalId)
  }

  preGameModel.subProp(_.inviter).listen {
    case None =>
      Globals.modalHide(gameModals.acceptPlayerInviteModalId)
    case Some(_) =>
      Globals.modalToggle(gameModals.acceptPlayerInviteModalId)
  }

  private val quitGameButton = UdashButton(
    buttonStyle = Color.Danger.toProperty,
    componentId = ComponentId("quit-game-button")
  )(nested =>
    Seq[Modifier](
      nested(
        produceWithNested(
          combine(
            presenter.gameStateProperty.transform(_.nonEmpty),
            presenter.gamePuzzleStateProperty.transform(_.nonEmpty),
            preGameModel.subProp(_.inJoinedPreGame).transform(_.nonEmpty)
          )
        ) { case ((hasGameState, hasPuzzleState, inJoinedPreGame), nested) =>
          span(
            nested(
              translatedDynamic(
                if (hasGameState || hasPuzzleState || inJoinedPreGame)
                  Translations.Game.quitGameButton
                else
                  Translations.Game.logoutButton
              )(_.apply())
            )
          ).render
        }
      ),
      span(`class` := "pl-2", FontAwesome.Solid.signOutAlt)
    )
  )

  quitGameButton.listen { _ =>
    if (
      presenter.gameStateProperty.transform(_.nonEmpty).get ||
      presenter.gamePuzzleStateProperty.transform(_.nonEmpty).get ||
      preGameModel.subProp(_.inJoinedPreGame).transform(_.nonEmpty).get
    )
      Globals.modalToggle(gameModals.quitGameModalId)
    else
      presenter.logout()
  }

  def mainDiv(): Modifier = {
    div(
      produceWithNested(
        combine(
          presenter.gameStateProperty.transform(_.nonEmpty),
          presenter.gamePuzzleStateProperty.transform(_.nonEmpty)
        )
      ) {
        case ((true, _), nested) =>
          playerVsUtils.mainDiv(nested)
        case ((_, true), nested) =>
          puzzlesView.createGameDiv(nested)
        case ((false, false), nested) =>
          div(
            nested(
              produceWithNested(preGameModel.subProp(_.menuState)) {
                case (MenuState.PlayingVsBots, nested) =>
                  playerVsBotsView.mainEditorDiv(nested)
                case (MenuState.PlayingVsPlayer, nested) =>
                  playerVsPlayerView.mainEditorDiv(nested)
              }
            )
          ).render
      }
    ).render
  }

  private def langChangeButton(lang: Lang): Modifier = {
    val btn = UdashButton(
      buttonStyle = Color.Link.toProperty,
      componentId = ComponentId(s"lang-btn-${lang.lang}")
    )(_ => lang.lang.toUpperCase())

    btn.listen { case UdashButton.ButtonClickEvent(_, _) =>
      translationsService.setLanguage(lang)
    }

    btn.render
  }

  override def getTemplate: Modifier =
    div(
      `class` := "row m-0 p-0 vh-100",
      // loads Bootstrap and FontAwesome styles from CDN
      UdashBootstrap.loadBootstrapStyles(),
      UdashBootstrap.loadFontAwesome(),
      BootstrapStyles.containerFluid,
      div(
        `class` := "navbar col-12",
//        button(
//          `class` := "navbar-toggler",
//          `type` := "button",
//          attr("data-toggle") := "collapse",
//          attr("data-target") := "#navbarToggleExternalContent",
//          span(FontAwesome.Solid.bars, FontAwesome.Modifiers.Sizing.x2)
//        ),
        quitGameButton,
        h2(
          `class` := "text-center",
          style := "font-family: 'Holtwood One SC', serif;",
          "battleships"
        ),
        div(
          GlobalStyles.floatRight,
          span(`class` := "small text-black-50 mr-5", s"v${BuildInfo.version}"),
          Translations.langs.map(v => langChangeButton(Lang(v)))
        )
      ),
      div(
        `class` := "col-12",
        div(
          //      GameStyles.flexContainer,
          //      GameStyles.mainCardHeight,
          gameModals.initialize(),
          mainDiv(),
        )
      )
    )

//    div(
//      UdashCard(componentId = ComponentId("game-panel"))(factory =>
//        Seq[Modifier](
//          factory.body { nested =>
//            Seq[Modifier](
//              `class` := "px-0 py-0 overflow-auto",
//              div(
//                GameStyles.flexContainer,
//                GameStyles.mainCardHeight,
//                gameModals.initialize(nested),
//                mainDiv(nested),
//              )
//            )
//          },
//          factory.footer { nested =>
//            playerVsBotsView.footer(nested)
//          }
//        )
//      )
//    )
//          div(
//            `class` := "row justify-content-between",
//            div(
//              `class` := "col-6",
//              span(
//                nested(translatedDynamic(Translations.Game.loggedInAs)(_.apply())),
//                " ",
//                b(bind(chatModel.subProp(_.username)))
//              ),
//              nested(produceWithNested(presenter.enemyUsernameProperty) {
//                case (Some(Username(enemyUsername)), nested) =>
//                  span(
//                    " - ",
//                    nested(translatedDynamic(Translations.Game.playingAgainst)(_.apply())),
//                    " ",
//                    b(enemyUsername)
//                  ).render
//                case _ =>
//                  span.render
//              }),
//              br,
//              nested(produceWithNested(presenter.gameModeProperty) {
//                case (Some(PlacingShipsMode(iPlacedShips, enemyPlacedShips)), nested) =>
//                  val placeShipsBinding =
//                    nested(
//                      translatedDynamic(
//                        if (iPlacedShips)
//                          Translations.Game.placeShipsWaitEnemy
//                        else if (enemyPlacedShips)
//                          Translations.Game.placeShipsEnemyReady
//                        else
//                          Translations.Game.placeShips
//                      )(_.apply())
//                    )
//                  span(color := "#FF0000", b(placeShipsBinding)).render
//                case (Some(PlayingMode(isMyTurn, turn, _, _, _)), nested) =>
//                  val turnStrBinding: Binding =
//                    nested(
//                      translatedDynamic(
//                        if (isMyTurn)
//                          Translations.Game.yourTurn
//                        else
//                          Translations.Game.enemyTurn
//                      )(_.apply())
//                    )
//
//                  span(
//                    nested(translatedDynamic(Translations.Game.turn)(_.apply())),
//                    " ",
//                    turn.toTurnString,
//                    ": ",
//                    span(color := "#FF0000", b(turnStrBinding))
//                  ).render
//                case (Some(GameOverMode(turn, youWon, _, _, _)), nested) =>
//                  val turnStrBinding: Binding =
//                    nested(
//                      translatedDynamic(
//                        if (youWon)
//                          Translations.Game.youWon
//                        else
//                          Translations.Game.enemyWon
//                      )(_.apply())
//                    )
//
//                  span(
//                    nested(translatedDynamic(Translations.Game.turn)(_.apply())),
//                    " ",
//                    turn.toTurnString,
//                    ": ",
//                    span(color := "#FF0000", b(turnStrBinding))
//                  ).render
//                case _ =>
//                  span.render
//              })
//            ), {
//              def toTimeStr(seconds: Int): String =
//                "%02d:%02d".format(seconds / 60, seconds % 60)
//
//              def toShortTimeStr(secondsOpt: Option[Int]): String =
//                secondsOpt
//                  .map { seconds =>
//                    if (seconds >= 60)
//                      " + %02d:%02d".format(seconds / 60, seconds % 60)
//                    else
//                      " + %02d".format(seconds)
//                  }
//                  .getOrElse("")
//
//              def showTimeStr(nested: NestedInterceptor, key: TranslationKey0): Binding =
//                nested(
//                  produceWithNested(presenter.modeTypeProperty) {
//                    case (Some(PlayingModeType | GameOverModeType), nested) =>
//                      span(nested(translatedDynamic(key)(_.apply()))).render
//                    case _ =>
//                      div.render
//                  }
//                )
//
//              def showTime(timeRemaining: TimeRemaining): Span =
//                span(
//                  `class` := "px-0",
//                  span(b(toTimeStr(timeRemaining.totalTimeRemainingMillis / 1000))),
//                  span(b(toShortTimeStr(timeRemaining.turnTimeRemainingMillisOpt.map(_ / 1000))))
//                ).render
//
//              div(
//                `class` := "col-4 row p-0 m-0",
//                div(
//                  `class` := "col px-0",
//                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.nonEmpty)) {
//                    case true =>
//                      span(showTimeStr(nested, Translations.Game.myTime)).render
//                    case false =>
//                      span.render
//                  }),
//                  br,
//                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.map(_._1))) {
//                    case Some(myTimeRemaining) =>
//                      showTime(myTimeRemaining)
//                    case None =>
//                      span.render
//                  })
//                ),
//                div(
//                  `class` := "col px-0",
//                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.nonEmpty)) {
//                    case true =>
//                      span(showTimeStr(nested, Translations.Game.enemyTime)).render
//                    case false =>
//                      span.render
//                  }),
//                  br,
//                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.nonEmpty)) {
//                    case true =>
//                      div(
//                        `class` := "row m-0",
//                        nested(produce(gameModel.subProp(_.timeRemaining).transform(_.map(_._2))) {
//                          case Some(enemyTimeRemaining) =>
//                            div(
//                              `class` := "m-0",
//                              showTime(enemyTimeRemaining)
//                            ).render
//                          case None =>
//                            span.render
//                        }),
//                        addEnemyTimeButton
//                      ).render
//                    case false =>
//                      span.render
//                  })
//                )
//              )
//            },
//            div(
//              `class` := "col-2 container",
//              div(
//                `class` := "row justify-content-end",
//                span(quitGameButton).render
//              )
//            )
//          ).render
//        ),
//        factory.body(nested =>
//          Seq[Modifier](
//            `class` := "p-0",
//            nested(
//              produceWithNested(
//                gameStateModel.transform(gameStateModel =>
//                  (gameStateModel.gameState.isEmpty, gameStateModel.gamePuzzleState.isEmpty)
//                )
//              ) {
//                case ((true, false), _) =>
//                  boardView.canvasDiv
//                case ((true, true), nested) =>
//                  val basePreGameDiv = div.render
//
//                  var handle: Int = 0
//
//                  handle = window.setInterval(
//                    () => {
//                      if (basePreGameDiv.clientWidth != 0) {
//                        val innerDiv = preGameView.createComponents(basePreGameDiv, nested)
//                        basePreGameDiv.appendChild(innerDiv)
//                        window.clearTimeout(handle)
//                      }
//                    },
//                    timeout = 20
//                  )
//
//                  basePreGameDiv
//                case (_, _) =>
//                  boardView.canvasDiv
//              }
//            )
//          )
//        ),
//        factory.footer(nested => nested(mainGameForm)),
//        factory.body(nested =>
//          Seq[Modifier](
//            `class` := "py-1",
//            messagesTab(nested)
//          )
//        )
//      )
//    )
//  )

}
