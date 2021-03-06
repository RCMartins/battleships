package pt.rmartins.battleships.frontend.views.game

import com.avsystem.commons.universalOps
import io.udash._
import io.udash.bindings.modifiers.Binding
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.card.UdashCard
import io.udash.bootstrap.form.UdashForm.FormEvent
import io.udash.bootstrap.form.{FormElementsFactory, UdashForm, UdashInputGroup}
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.component.ComponentId
import io.udash.css._
import io.udash.i18n._
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div, Input, LI, Span}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.frontend.views.model.AttacksQueuedStatus
import pt.rmartins.battleships.frontend.views.model.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.frontend.views.model.ModeType._
import pt.rmartins.battleships.shared.css.ChatStyles
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
    boardView: BoardView,
    preGameView: PreGameView,
    gameModals: GameModals,
    viewUtils: ViewUtils
) extends View
    with CssView {

  import translationsService._

  private def reloadBoardView(): Unit = {
    if (!presenter.onCanvasResize(boardView)) {
      window.setTimeout(
        () => boardView.paint(),
        1
      )
    }
  }

  window.onresize = (_: UIEvent) => {
    presenter.onCanvasResize(boardView)
  }

  gameStateModel.listen(_ => reloadBoardView())

  screenModel.subProp(_.canvasSize).listen(_ => reloadBoardView())
  screenModel.subProp(_.missilesPopupMillisOpt).listen(_ => reloadBoardView())
  screenModel.subProp(_.extraTurnPopup).listen(_ => reloadBoardView())
  screenModel.subProp(_.screenResized).listen(_ => reloadBoardView())
  screenModel.subProp(_.hoverMove).listen(_ => reloadBoardView())
  screenModel.subProp(_.tick).listen(_ => reloadBoardView())
  translationsService.currentLangProperty.listen(_ => reloadBoardView())
  translationsModel.listen(_ => reloadBoardView())

  gameModel.subProp(_.mousePosition).listen(_ => reloadBoardView())
  gameModel.subProp(_.mouseDown).listen(_ => reloadBoardView())
  gameModel.subProp(_.selectedShip).listen(_ => reloadBoardView())
  gameModel.subProp(_.turnAttacks).listen(_ => reloadBoardView())
  gameModel.subProp(_.turnAttacksQueuedStatus).listen(_ => reloadBoardView())
  gameModel.subProp(_.selectedAction).listen(_ => reloadBoardView())
  gameModel.subProp(_.lineDashOffset).listen(_ => reloadBoardView())

  private val startGameVsBotButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("start-game-bot-button")
  )(nested =>
    Seq[Modifier](span(nested(translatedDynamic(Translations.Game.startGameVsBot)(_.apply()))))
  )

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

  private val confirmRulesButton = {
    val isConfirmedProperty: ReadableProperty[Boolean] =
      preGameModel.subProp(_.inJoinedPreGame).transform {
        case Some(PlayingAgainstPlayer(_, true, _, _)) => true
        case _                                         => false
      }

    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("confirm-rules-button"),
      disabled = isConfirmedProperty
    )(nested =>
      Seq[Modifier](
        span(nested(translatedDynamic(Translations.Game.confirmRulesButton)(_.apply())))
      )
    )
  }

  private val cancelRulesButton =
    UdashButton(
      buttonStyle = Color.Danger.toProperty,
      block = true.toProperty,
      componentId = ComponentId("cancel-rules-button")
    )(_ => Seq[Modifier](`class` := "invisible", span(FontAwesome.Solid.times).render))

  preGameModel
    .subProp(_.inJoinedPreGame)
    .transform(_.collect { case PlayingAgainstPlayer(_, confirmed, _, _) =>
      confirmed
    })
    .listen(
      { confirmedOpt =>
        val cancelButtonOpt: Option[Element] =
          Option(document.getElementById(cancelRulesButton.componentId.value))
        cancelButtonOpt.foreach { cancelButton =>
          if (confirmedOpt.contains(true)) {
            cancelButton.classList.remove("invisible")
            cancelButton.classList.add("visible")
          } else {
            cancelButton.classList.remove("visible")
            cancelButton.classList.add("invisible")
          }
        }
      },
      initUpdate = true
    )

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

  private val sendPuzzleSolutionButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    disabled = presenter.gamePuzzleStateProperty.transform(_.exists(_.puzzleSolutionOpt.nonEmpty)),
    componentId = ComponentId("send-puzzle-solution-button")
  )(nested =>
    Seq[Modifier](
      span(nested(translatedDynamic(Translations.Game.sendPuzzleAnswerButton)(_.apply())))
    )
  )

  private val nextPuzzleButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("next-puzzle-button")
  )(nested =>
    Seq[Modifier](
      span(nested(translatedDynamic(Translations.Game.nextPuzzleButton)(_.apply())))
    )
  )

  private val confirmShipsButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("confirm-button"),
      disabled = gameModel.subProp(_.shipsLeftToPlace).transform(_.nonEmpty)
    )(nested => Seq(nested(translatedDynamic(Translations.Game.confirmButton)(_.apply()))))

  private val undoButton =
    UdashButton(
      buttonStyle = Color.Secondary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("undo-button"),
      disabled = presenter.gameStateProperty.transform(!_.exists(_.me.myBoard.ships.nonEmpty))
    )(nested => Seq(nested(translatedDynamic(Translations.Game.undoButton)(_.apply()))))

  private val resetButton =
    UdashButton(
      buttonStyle = Color.Danger.toProperty,
      block = true.toProperty,
      componentId = ComponentId("reset-button"),
      disabled = presenter.gameStateProperty.transform(!_.exists(_.me.myBoard.ships.nonEmpty))
    )(nested => Seq(nested(translatedDynamic(Translations.Game.resetButton)(_.apply()))))

  private val randomPlacementButton =
    UdashButton(
      buttonStyle = Color.Secondary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("random-button"),
      disabled = gameModel.subProp(_.shipsLeftToPlace).transform(_.isEmpty)
    )(nested => Seq(nested(translatedDynamic(Translations.Game.randomButton)(_.apply()))))

  private val editRulesButton =
    UdashButton(
      buttonStyle = Color.Secondary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("edit-rules-button")
    )(nested => Seq(nested(translatedDynamic(Translations.Game.editRulesButton)(_.apply()))))

  private val cancelQueuedAttacksButton =
    UdashButton(
      buttonStyle = Color.Danger.toProperty,
      block = true.toProperty,
      componentId = ComponentId("cancel-queued-attacks-button")
    )(_ => Seq[Modifier](`class` := "invisible", span(FontAwesome.Solid.times).render))

  gameModel
    .subProp(_.turnAttacksQueuedStatus)
    .listen(
      { attacksQueuedStatus =>
        val cancelButtonOpt: Option[Element] =
          Option(document.getElementById(cancelQueuedAttacksButton.componentId.value))
        cancelButtonOpt.foreach { cancelButton =>
          attacksQueuedStatus match {
            case AttacksQueuedStatus.Queued =>
              cancelButton.classList.remove("invisible")
              cancelButton.classList.add("visible")
            case _ =>
              cancelButton.classList.remove("visible")
              cancelButton.classList.add("invisible")
          }
        }
      },
      initUpdate = true
    )

  private val launchAttackButton = {
    val launchAttackIsDisabledProperty =
      gameModel
        .subProp(_.turnAttacksQueuedStatus)
        .combine(gameModel.subProp(_.turnAttacks)) { case (turnAttacksQueuedStatus, turnAttacks) =>
          turnAttacksQueuedStatus != AttacksQueuedStatus.NotSet ||
            !turnAttacks.forall(_.isPlaced)
        }

    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("launch-attack-button"),
      disabled = launchAttackIsDisabledProperty
    )(nested =>
      Seq(
        nested(
          produceWithNested(
            combine(
              gameModel.subProp(_.turnAttacksQueuedStatus),
              presenter.isMyTurnProperty,
              gameModel.subProp(_.turnAttacks).transform(_.count(_.isPlaced)),
              gameModel.subProp(_.turnAttacks).transform(_.size)
            )
          ) {
            case ((AttacksQueuedStatus.NotSet, true, placed, size), nested) =>
              span(
                nested(translatedDynamic(Translations.Game.launchAttackButton)(_.apply())),
                s" $placed/$size"
              ).render
            case ((AttacksQueuedStatus.NotSet, false, placed, size), nested) =>
              span(
                nested(translatedDynamic(Translations.Game.queueAttackButton)(_.apply())),
                s" $placed/$size"
              ).render
            case ((_, _, _, _), nested) =>
              span(
                nested(translatedDynamic(Translations.Game.waitForTurnButton)(_.apply()))
              ).render
          }
        )
      )
    )
  }

  private val hideMyBoardButton =
    UdashButton(
      buttonStyle = screenModel.subProp(_.hideMyBoard).transform {
        case true  => Color.Danger
        case false => Color.Secondary
      },
      block = true.toProperty,
      componentId = ComponentId("hide-my-board-button")
    )(nested =>
      Seq(nested(produceWithNested(screenModel.subProp(_.hideMyBoard)) {
        case (true, nested) =>
          span(nested(translatedDynamic(Translations.Game.showMyBoardButton)(_.apply()))).render
        case (false, nested) =>
          span(nested(translatedDynamic(Translations.Game.hideMyBoardButton)(_.apply()))).render
      }))
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
              div(`class` := "mx-2", confirmRulesButton),
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
                div(`class` := "mx-2", sendPuzzleSolutionButton)
              ),
              div(`class` := "mx-2", nextPuzzleButton)
            ).render
          case ((_, _, _, _, Some(_)), _) =>
            div(
              `class` := "row mx-0",
              div(`class` := "mx-2", sendPuzzleSolutionButton)
            ).render
          case ((_, _, None, _, _), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(
                `class` := "row mx-0",
                div(`class` := "mx-2", startGameVsBotButton),
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
                div(`class` := "mx-2", confirmShipsButton),
                div(`class` := "mx-2", undoButton),
                div(`class` := "mx-2", resetButton),
                div(`class` := "mx-2", randomPlacementButton)
              ),
              div(`class` := "mx-2", editRulesButton)
            ).render
          case ((_, _, _, Some(PlacingShipsMode(true, _)), _), _) =>
            div(
              `class` := "row mx-0",
              div(`class` := "mx-2", undoButton),
              div(`class` := "mx-2", resetButton)
            ).render
          case ((_, _, Some(PlayingModeType), _, _), _) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(
                `class` := "row mx-0",
                div(`class` := "ml-2", cancelQueuedAttacksButton),
                div(`class` := "mx-1", launchAttackButton)
              ),
              div(`class` := "mx-2", hideMyBoardButton)
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

  startGameVsBotButton.listen { _ =>
    presenter.startGameWithBots()
  }

  invitePlayerButton.listen { _ =>
    presenter.invitePlayer(preGameModel.subProp(_.enemyUsernameText).get)
  }

  cancelPlayerInviteButton.listen { _ =>
    preGameModel.subProp(_.invitedUsername).set(None)
  }

  solvePuzzleButton.listen { _ =>
    presenter.startNewPuzzle()
  }

  sendPuzzleSolutionButton.listen { _ =>
    presenter.getPuzzleSolution()
  }

  nextPuzzleButton.listen { _ =>
    presenter.startNewPuzzle()
  }

  confirmRulesButton.listen { _ =>
    presenter.confirmRules()
  }

  cancelRulesButton.listen { _ =>
    presenter.cancelRules()
  }

  confirmShipsButton.listen { _ =>
    presenter.confirmShipPlacement()
  }

  undoButton.listen { _ =>
    presenter.undoLastPlacedShip()
  }

  resetButton.listen { _ =>
    presenter.resetPlacedShips()
  }

  randomPlacementButton.listen { _ =>
    presenter.randomPlacement()
  }

  editRulesButton.listen { _ =>
    presenter.requestEditRules()
  }

  cancelQueuedAttacksButton.listen { _ =>
    presenter.cancelQueuedAttacks()
  }

  launchAttackButton.listen { _ =>
    presenter.launchAttack()
  }

  hideMyBoardButton.listen { _ =>
    screenModel.subProp(_.hideMyBoard).set(!screenModel.get.hideMyBoard)
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

  private val chatTabButton: UdashButton =
    UdashButton()(nested =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.chatTab) " active"
           else ""),
        nested(translatedDynamic(Translations.Game.chatTab)(_.apply())),
        nested(produce(presenter.chatMessagesShowNotification) {
          case None         => span.render
          case Some(number) => span(`class` := "badge badge-light ml-1", number).render
        })
      )
    )

  private val myMovesTabDisabledProperty: ReadableProperty[Boolean] =
    combine(presenter.modeTypeProperty, presenter.gamePuzzleStateProperty.transform(_.nonEmpty))
      .transform {
        case (_, true)                                     => false
        case (Some(PlayingModeType | GameOverModeType), _) => false
        case _                                             => true
      }

  private val enemyMovesTabDisabledProperty: ReadableProperty[Boolean] =
    presenter.modeTypeProperty.transform {
      case Some(PlayingModeType | GameOverModeType) => false
      case _                                        => true
    }

  private val myMovesTabButton: UdashButton =
    UdashButton(
      disabled = myMovesTabDisabledProperty
    )(nested =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.myMovesTab) " active"
           else ""),
        nested(translatedDynamic(Translations.Game.myMovesTab)(_.apply())),
        nested(produce(presenter.myMovesHistoryShowNotification) {
          case None         => span.render
          case Some(number) => span(`class` := "badge badge-light ml-1", number).render
        })
      )
    )

  private val enemyMovesTabButton: UdashButton =
    UdashButton(
      disabled = enemyMovesTabDisabledProperty
    )(nested =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.enemyMovesTab) " active"
           else ""),
        nested(translatedDynamic(Translations.Game.enemyMovesTab)(_.apply())),
        nested(produce(presenter.enemyMovesHistoryShowNotification) {
          case None         => span.render
          case Some(number) => span(`class` := "badge badge-light ml-1", number).render
        })
      )
    )

  chatTabButton.listen { _ =>
    presenter.setSelectedTab(ScreenModel.chatTab)
  }

  myMovesTabButton.listen { _ =>
    presenter.setSelectedTab(ScreenModel.myMovesTab)
  }

  enemyMovesTabButton.listen { _ =>
    presenter.setSelectedTab(ScreenModel.enemyMovesTab)
  }

  private def selectedTabToButtonId(id: String): String =
    id match {
      case ScreenModel.chatTab       => chatTabButton.componentId.value
      case ScreenModel.myMovesTab    => myMovesTabButton.componentId.value
      case ScreenModel.enemyMovesTab => enemyMovesTabButton.componentId.value
    }

  presenter.selectedTabProperty.listen { selectedTab =>
    def updateClass(id: String): Unit =
      if (selectedTab == id) {
        document.getElementById(selectedTabToButtonId(id)).classList.add("active")
        Option(document.getElementById(id)).foreach { elem =>
          elem.classList.add("show")
          elem.classList.add("active")
        }
      } else {
        document.getElementById(selectedTabToButtonId(id)).classList.remove("active")
        Option(document.getElementById(id)).foreach { elem =>
          elem.classList.remove("show")
          elem.classList.remove("active")
        }
      }

    List(ScreenModel.chatTab, ScreenModel.myMovesTab, ScreenModel.enemyMovesTab)
      .foreach(updateClass)
  }

  private def messagesTab(nested: Binding.NestedInterceptor): Modifier = {
    def makeNavItem(button: UdashButton): JsDom.TypedTag[LI] =
      li(
        `class` := "nav-item",
        role := "presentation",
        nested(button)
      )

    val missesMovesCheckbox =
      input(
        `class` := "form-control px-1",
        id := "missesMoves-checkbox",
        width := "32px",
        `type` := "checkbox"
      ).render

    screenModel
      .subProp(_.showMissesMoves)
      .listen(
        { showMissesMoves =>
          if (missesMovesCheckbox.checked != showMissesMoves)
            missesMovesCheckbox.checked = showMissesMoves
        },
        initUpdate = true
      )

    missesMovesCheckbox.onchange = _ => {
      screenModel.subProp(_.showMissesMoves).set(missesMovesCheckbox.checked)
    }

    val disabledMovesCheckbox =
      input(
        `class` := "form-control px-1",
        id := "disabledMoves-checkbox",
        width := "32px",
        `type` := "checkbox"
      ).render

    screenModel
      .subProp(_.showDisabledMoves)
      .listen(
        { showDisabledMoves =>
          if (disabledMovesCheckbox.checked != showDisabledMoves)
            disabledMovesCheckbox.checked = showDisabledMoves
        },
        initUpdate = true
      )

    disabledMovesCheckbox.onchange = _ => {
      screenModel.subProp(_.showDisabledMoves).set(disabledMovesCheckbox.checked)
    }

    div(
      `class` := "row",
      div(
        `class` := "col-8",
        ul(
          `class` := "nav nav-tabs",
          `role` := "tablist",
          makeNavItem(chatTabButton),
          makeNavItem(myMovesTabButton),
          makeNavItem(enemyMovesTabButton)
        ),
        div(
          `class` := "tab-content",
          messagesTabItem(nested),
          myMovesTabItem(nested),
          enemyMovesTabItem(nested)
        )
      ),
      div(
        `class` := "col-4 px-0",
        div(
          `class` := "btn-group m-3 mt-5",
          missesMovesCheckbox,
          label(
            `class` := "input-group-text",
            `for` := "missesMoves-checkbox",
            style := "user-select: none",
            span(FontAwesome.Solid.eye),
            span(
              `class` := "pl-1",
              nested(translatedDynamic(Translations.Game.missesMoves)(_.apply()))
            )
          )
        ),
        div(
          `class` := "btn-group m-3",
          disabledMovesCheckbox,
          label(
            `class` := "input-group-text",
            `for` := "disabledMoves-checkbox",
            style := "user-select: none",
            span(FontAwesome.Solid.eye),
            span(
              `class` := "pl-1",
              nested(translatedDynamic(Translations.Game.disabledMoves)(_.apply()))
            )
          )
        )
      )
    )
  }

  private def messagesTabItem(nested: Binding.NestedInterceptor): JsDom.TypedTag[Div] =
    div(
      `class` := "tab-pane fade" +
        (if (presenter.selectedTabProperty.get == ScreenModel.chatTab)
           " show active"
         else ""),
      id := ScreenModel.chatTab,
      role := "tabpanel",
      div(
        ChatStyles.messagesWindow,
        nested(repeat(presenter.chatMessagesProperty) { msgProperty =>
          val msg = msgProperty.get
          div(
            ChatStyles.msgContainer,
            strong(msg.author, ": "),
            span(msg.text),
            span(ChatStyles.msgDate, msg.created.toString)
          ).render
        })
      ),
      msgForm
    )

  private def turnPlaysToHtml(showCheckbox: Boolean, turnPlay: TurnPlay): Seq[dom.Element] = {
    val fleetMaxSize: Coordinate = {
      val size: Coordinate =
        presenter.gameFleetMaxSize.get
      if (size.y > size.x)
        size.flipCoor
      else
        size
    }
    val sqSize = if (fleetMaxSize.y > 4) 7 else if (fleetMaxSize.y > 3) 8 else 10
    val canvasSize: Coordinate = fleetMaxSize * sqSize

    def emptyCanvasDiv: Div = {
      val emptyCanvas: Canvas = canvas(`class` := "mr-3").render
      val sizeWithMargin = canvasSize + viewUtils.defaultMargin
      emptyCanvas.setAttribute("width", sizeWithMargin.x.toString)
      emptyCanvas.setAttribute("height", sizeWithMargin.y.toString)
      div(emptyCanvas).render
    }

    val toTurnString = turnPlay.turn.toTurnString
    val checkId = "check" + toTurnString

    val inputCheckBox =
      input(
        id := checkId,
        `type` := "checkbox"
      ).render

    def createWaterCanvas(drawRadar: Boolean): Seq[Modifier] =
      Seq[Modifier](
        viewUtils.createWaterCanvas(
          canvasSize,
          sqSize,
          centerXCanvas = false,
          centerYCanvas = true,
          drawRadar = drawRadar
        ),
        viewUtils.createEmptyCanvas(x = sqSize * 2, y = canvasSize.y)
      )

    def createShitHit(shipId: ShipId, destroyed: Boolean, drawRadar: Boolean): Seq[Modifier] =
      Seq[Modifier](
        viewUtils.createShipCanvas(
          canvasSize,
          sqSize,
          Ship.shipMaxXMessagesMap(shipId),
          destroyed,
          centerXCanvas = false,
          centerYCanvas = true,
          drawRadar = drawRadar
        ),
        viewUtils.createEmptyCanvas(x = sqSize * 2, y = canvasSize.y)
      )

    val turnHits: Div = {
      val radarShots: Seq[Modifier] =
        turnPlay.turnAttacks
          .filter(_.attackType == AttackType.Radar)
          .flatMap(turnAttack => turnAttack.coordinateOpt)
          .flatMap { coor =>
            val isHit =
              gameStateModel.subProp(_.gameState).get match {
                case Some(GameState(_, _, me, _, _)) =>
                  me.enemyBoardMarks.getMarkAt(coor) == BoardMark.ShipHit
                case _ =>
                  false
              }

            if (isHit)
              createShitHit(Ship.Submarine.shipId, destroyed = false, drawRadar = true)
            else
              createWaterCanvas(drawRadar = true)
          }

      div(
        turnPlay.hitHints.flatMap {
          case HitHint.Water =>
            createWaterCanvas(drawRadar = false)
          case HitHint.ShipHit(shipId, destroyed) =>
            createShitHit(shipId, destroyed, drawRadar = false)
        } ++
          radarShots
      ).render
    }

    if (showCheckbox) {
      if (!screenModel.get.disabledMovesSet(turnPlay.turn))
        inputCheckBox.checked = true

      inputCheckBox.onchange = _ => {
        screenModel
          .subProp(_.disabledMovesSet)
          .set(
            if (inputCheckBox.checked)
              screenModel.get.disabledMovesSet - turnPlay.turn
            else
              screenModel.get.disabledMovesSet + turnPlay.turn
          )
        if (!inputCheckBox.checked)
          screenModel.subProp(_.hoverMove).set(None)
      }
    } else
      inputCheckBox.style.visibility = "hidden"

    val turnDiv =
      div(
        ChatStyles.turnContainer,
        div(
          `class` := "form-group my-0",
          div(
            `class` := "checkbox",
            label(
              minWidth := "75px",
              `for` := checkId,
              inputCheckBox,
              strong(
                `class` := "col-6 pl-3 py-2",
                style := "user-select: none",
                toTurnString,
                ": "
              )
            )
          )
        ),
        span(
          `class` := "mt-1",
          if (showCheckbox && !inputCheckBox.checked)
            emptyCanvasDiv
          else
            turnHits
        )
      ).render

    if (showCheckbox) {
      if (inputCheckBox.checked)
        turnDiv.onmouseenter = _ => {
          screenModel.subProp(_.hoverMove).set(Some(turnPlay.turn))
        }

      turnDiv.onmouseleave = _ => {
        screenModel.subProp(_.hoverMove).set(None)
      }
    }

    turnDiv
  }

  private def myMovesTabItem(nested: Binding.NestedInterceptor): Div =
    div(
      `class` := "tab-pane fade" +
        (if (presenter.selectedTabProperty.get == ScreenModel.myMovesTab)
           " show active"
         else ""),
      id := ScreenModel.myMovesTab,
      role := "tabpanel",
      ChatStyles.myMovesWindow,
      nested(produce(presenter.myMovesHistoryProperty) { turnPlaySeqProperty =>
        turnPlaySeqProperty.flatMap(turnPlay => turnPlaysToHtml(showCheckbox = true, turnPlay))
      })
    ).render

  private def enemyMovesTabItem(nested: Binding.NestedInterceptor): Div =
    div(
      `class` := "tab-pane fade" +
        (if (presenter.selectedTabProperty.get == ScreenModel.enemyMovesTab)
           " show active"
         else ""),
      id := ScreenModel.enemyMovesTab,
      role := "tabpanel",
      ChatStyles.myMovesWindow,
      nested(repeat(presenter.enemyMovesHistoryProperty) { turnPlayProperty =>
        turnPlaysToHtml(showCheckbox = false, turnPlayProperty.get)
      })
    ).render

  private val msgInput = TextInput(chatModel.subProp(_.msgInput))(
    translatedAttrDynamic(Translations.Chat.inputPlaceholder, "placeholder")(_.apply())
  )

  private val submitButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("msg-send")
  )(_ => Seq(span(FontAwesome.Solid.paperPlane), tpe := "submit"))

  private val msgForm = UdashForm(
    componentId = ComponentId("msg-from")
  )(factory =>
    Seq[Modifier](
      `class` := "my-1",
      factory.disabled(presenter.gameModeProperty.transform(_.isEmpty))(nested =>
        nested(
          UdashInputGroup()(
            UdashInputGroup.input(msgInput.render),
            UdashInputGroup.appendButton(submitButton)
          )
        )
      )
    )
  )

  msgForm.listen { case FormEvent(_, FormEvent.EventType.Submit) =>
    presenter.sendMsg()
    presenter.setSelectedTab(ScreenModel.chatTab)
    document.getElementById(chatTabButton.componentId.value).classList.add("active")
    document.getElementById(myMovesTabButton.componentId.value).classList.remove("active")
    document.getElementById(enemyMovesTabButton.componentId.value).classList.remove("active")
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

  override def getTemplate: Modifier = div(
    UdashCard(componentId = ComponentId("game-panel"))(factory =>
      Seq(
        factory.header(nested =>
          div(
            `class` := "row justify-content-between",
            gameModals.generalGameErrorModal(nested).tap {
              _.addEventListener(
                "hidden.bs.modal",
                (_: Event) => {
                  screenModel.subProp(_.errorModalType).set(None)
                }
              )
            },
            gameModals
              .fleetNameModal(nested)
              .tap {
                _.addEventListener(
                  "shown.bs.modal",
                  (_: Event) => {
                    gameModals.fleetNameInput.focus()
                  }
                )
              }
              .tap {
                _.addEventListener(
                  "hidden.bs.modal",
                  (_: Event) => {
                    presenter.saveNewNamedRules()
                  }
                )
              },
            gameModals.quitGameModal(nested),
            gameModals.acceptPlayerInviteModal(nested).tap {
              _.addEventListener(
                "hidden.bs.modal",
                (_: Event) => {
                  presenter.answerInvitePlayerRequest(false)
                }
              )
            },
            gameModals.acceptEditRulesModal(nested).tap {
              _.addEventListener(
                "hidden.bs.modal",
                (_: Event) => {
                  screenModel.subProp(_.receiveEditRequest).set(None)
                  presenter.answerEditRulesRequest(false)
                }
              )
            },
            gameModals.editGameBonusModal(nested),
            div(
              `class` := "col-6",
              span(
                nested(translatedDynamic(Translations.Game.loggedInAs)(_.apply())),
                " ",
                b(bind(chatModel.subProp(_.username)))
              ),
              nested(produceWithNested(presenter.enemyUsernameProperty) {
                case (Some(Username(enemyUsername)), nested) =>
                  span(
                    " - ",
                    nested(translatedDynamic(Translations.Game.playingAgainst)(_.apply())),
                    " ",
                    b(enemyUsername)
                  ).render
                case _ =>
                  span.render
              }),
              br,
              nested(produceWithNested(presenter.gameModeProperty) {
                case (Some(PlacingShipsMode(iPlacedShips, enemyPlacedShips)), nested) =>
                  val placeShipsBinding =
                    nested(
                      translatedDynamic(
                        if (iPlacedShips)
                          Translations.Game.placeShipsWaitEnemy
                        else if (enemyPlacedShips)
                          Translations.Game.placeShipsEnemyReady
                        else
                          Translations.Game.placeShips
                      )(_.apply())
                    )
                  span(color := "#FF0000", b(placeShipsBinding)).render
                case (Some(PlayingMode(isMyTurn, turn, _, _, _)), nested) =>
                  val turnStrBinding: Binding =
                    nested(
                      translatedDynamic(
                        if (isMyTurn)
                          Translations.Game.yourTurn
                        else
                          Translations.Game.enemyTurn
                      )(_.apply())
                    )

                  span(
                    nested(translatedDynamic(Translations.Game.turn)(_.apply())),
                    " ",
                    turn.toTurnString,
                    ": ",
                    span(color := "#FF0000", b(turnStrBinding))
                  ).render
                case (Some(GameOverMode(turn, youWon, _, _, _)), nested) =>
                  val turnStrBinding: Binding =
                    nested(
                      translatedDynamic(
                        if (youWon)
                          Translations.Game.youWon
                        else
                          Translations.Game.enemyWon
                      )(_.apply())
                    )

                  span(
                    nested(translatedDynamic(Translations.Game.turn)(_.apply())),
                    " ",
                    turn.toTurnString,
                    ": ",
                    span(color := "#FF0000", b(turnStrBinding))
                  ).render
                case _ =>
                  span.render
              })
            ), {
              def toTimeStr(seconds: Int): String =
                "%02d:%02d".format(seconds / 60, seconds % 60)

              def toShortTimeStr(secondsOpt: Option[Int]): String =
                secondsOpt
                  .map { seconds =>
                    if (seconds >= 60)
                      " + %02d:%02d".format(seconds / 60, seconds % 60)
                    else
                      " + %02d".format(seconds)
                  }
                  .getOrElse("")

              def showTimeStr(nested: NestedInterceptor, key: TranslationKey0): Binding =
                nested(
                  produceWithNested(presenter.modeTypeProperty) {
                    case (Some(PlayingModeType | GameOverModeType), nested) =>
                      span(nested(translatedDynamic(key)(_.apply()))).render
                    case _ =>
                      div.render
                  }
                )

              def showTime(timeRemaining: TimeRemaining): Span =
                span(
                  `class` := "px-0",
                  span(b(toTimeStr(timeRemaining.totalTimeRemainingMillis / 1000))),
                  span(b(toShortTimeStr(timeRemaining.turnTimeRemainingMillisOpt.map(_ / 1000))))
                ).render

              div(
                `class` := "col-4 row p-0 m-0",
                div(
                  `class` := "col px-0",
                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.nonEmpty)) {
                    case true =>
                      span(showTimeStr(nested, Translations.Game.myTime)).render
                    case false =>
                      span.render
                  }),
                  br,
                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.map(_._1))) {
                    case Some(myTimeRemaining) =>
                      showTime(myTimeRemaining)
                    case None =>
                      span.render
                  })
                ),
                div(
                  `class` := "col px-0",
                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.nonEmpty)) {
                    case true =>
                      span(showTimeStr(nested, Translations.Game.enemyTime)).render
                    case false =>
                      span.render
                  }),
                  br,
                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.nonEmpty)) {
                    case true =>
                      div(
                        `class` := "row m-0",
                        nested(produce(gameModel.subProp(_.timeRemaining).transform(_.map(_._2))) {
                          case Some(enemyTimeRemaining) =>
                            div(
                              `class` := "m-0",
                              showTime(enemyTimeRemaining)
                            ).render
                          case None =>
                            span.render
                        }),
                        addEnemyTimeButton
                      ).render
                    case false =>
                      span.render
                  })
                )
              )
            },
            div(
              `class` := "col-2 container",
              div(
                `class` := "row justify-content-end",
                span(quitGameButton).render
              )
            )
          ).render
        ),
        factory.body(nested =>
          Seq[Modifier](
            `class` := "p-0",
            nested(
              produceWithNested(
                gameStateModel.transform(gameStateModel =>
                  (gameStateModel.gameState.isEmpty, gameStateModel.gamePuzzleState.isEmpty)
                )
              ) {
                case ((true, false), _) =>
                  boardView.canvasDiv
                case ((true, true), nested) =>
                  val basePreGameDiv = div.render

                  var handle: Int = 0

                  handle = window.setInterval(
                    () => {
                      if (basePreGameDiv.clientWidth != 0) {
                        val innerDiv = preGameView.createComponents(basePreGameDiv, nested)
                        basePreGameDiv.appendChild(innerDiv)
                        window.clearTimeout(handle)
                      }
                    },
                    timeout = 20
                  )

                  basePreGameDiv
                case (_, _) =>
                  boardView.canvasDiv
              }
            )
          )
        ),
        factory.footer(nested => nested(mainGameForm)),
        factory.body(nested =>
          Seq[Modifier](
            `class` := "py-1",
            messagesTab(nested)
          )
        )
      )
    )
  )

}
