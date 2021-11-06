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
import org.scalajs.dom.html.{Canvas, Div, LI, Span}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.ModeType._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.css.ChatStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, PlayingMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class GameView(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    gameStateModel: ModelProperty[GameStateModel],
    chatModel: ModelProperty[ChatModel],
    screenModel: ModelProperty[ScreenModel],
    presenter: GamePresenter,
    translationsService: TranslationsService,
    boardView: BoardView,
    preGameView: PreGameView,
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
  screenModel.subProp(_.extraTurnText).listen(_ => reloadBoardView())
  screenModel.subProp(_.screenResized).listen(_ => reloadBoardView())
  screenModel.subProp(_.hoverMove).listen(_ => reloadBoardView())
  screenModel.subProp(_.tick).listen(_ => reloadBoardView())

  gameModel.subProp(_.mousePosition).listen(_ => reloadBoardView())
  gameModel.subProp(_.mouseDown).listen(_ => reloadBoardView())
  gameModel.subProp(_.selectedShip).listen(_ => reloadBoardView())
  gameModel.subProp(_.turnAttacks).listen(_ => reloadBoardView())
  gameModel.subProp(_.turnAttacksSent).listen(_ => reloadBoardView())
  gameModel.subProp(_.selectedBoardMarkOpt).listen(_ => reloadBoardView())
  gameModel.subProp(_.lineDashOffset).listen(_ => reloadBoardView())

  private val startGameVsBotButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("start-game-bot-button")
  )(nested =>
    Seq[Modifier](span(nested(translatedDynamic(Translations.Game.startGameVsBot)(_.apply()))))
  )

  private val startGameVsPlayerButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("start-game-player-button")
  )(nested =>
    Seq[Modifier](span(nested(translatedDynamic(Translations.Game.startGameVsPlayer)(_.apply()))))
  )

  private def usernameInput(factory: FormElementsFactory) =
    factory.input
      .formGroup(groupId = ComponentId("username")) { nested =>
        factory.input
          .textInput(preGameModel.subProp(_.enemyUsername).bitransform(_.username)(Username(_)))(
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

  private val confirmShipsButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("confirm-button"),
      disabled = presenter.gameStateProperty.transform(!_.exists(_.me.shipsLeftToPlace.isEmpty))
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
      disabled = presenter.gameStateProperty.transform(!_.exists(_.me.shipsLeftToPlace.nonEmpty))
    )(nested => Seq(nested(translatedDynamic(Translations.Game.randomButton)(_.apply()))))

  private val launchAttackButton = {
    val launchAttackIsDisabledProperty =
      presenter.playingModeProperty
        .transform(_.map(_.isMyTurn))
        .combine(gameModel.subProp(_.turnAttacks)) { case (isMyTurnOpt, turnAttacks) =>
          !isMyTurnOpt.exists(isMyTurn => isMyTurn && turnAttacks.forall(_.isPlaced))
        }

    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("launch-attack-button"),
      disabled = launchAttackIsDisabledProperty
    )(nested =>
      Seq(nested(produceWithNested(presenter.isMyTurnProperty) {
        case (true, nested) =>
          span(nested(translatedDynamic(Translations.Game.launchAttackButton)(_.apply()))).render
        case (false, nested) =>
          span(nested(translatedDynamic(Translations.Game.waitForTurnButton)(_.apply()))).render
      }))
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
      nested(produce(combine(presenter.modeTypeProperty, presenter.preGameModeProperty)) {
        case (None, _) =>
          div(
            `class` := "row",
            div(`class` := "mx-2", startGameVsBotButton),
            div(`class` := "ml-2", startGameVsPlayerButton),
            usernameInput(factory)
          ).render
        case (_, Some(PreGameMode(false, _))) =>
          div(
            `class` := "row",
            div(`class` := "mx-2", confirmShipsButton),
            div(`class` := "mx-2", undoButton),
            div(`class` := "mx-2", resetButton),
            div(`class` := "mx-2", randomPlacementButton)
          ).render
        case (_, Some(PreGameMode(true, _))) =>
          div(
            `class` := "row",
            div(`class` := "mx-2", undoButton),
            div(`class` := "mx-2", resetButton)
          ).render
        case (Some(PlayingModeType), _) =>
          div(
            `class` := "row justify-content-between",
            div(`class` := "mx-2", launchAttackButton),
            div(`class` := "mx-2", hideMyBoardButton)
          ).render
        case (Some(GameOverModeType), _) =>
          div(
            `class` := "row justify-content-between",
            div(`class` := "mx-2", rematchButton),
            div(`class` := "mx-2", revealEnemyBoardButton)
          ).render
        case _ =>
          span.render
      })
    )
  )

  startGameVsBotButton.listen { _ =>
    presenter.startGameWithBots()
  }

  startGameVsPlayerButton.listen { _ =>
    presenter.startGameWith(preGameModel.subProp(_.enemyUsername).get)
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

  private val movesTabDisabledProperty: ReadableProperty[Boolean] =
    presenter.modeTypeProperty.transform {
      case Some(PlayingModeType | GameOverModeType) => false
      case _                                        => true
    }

  private val myMovesTabButton: UdashButton =
    UdashButton(
      disabled = movesTabDisabledProperty
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
      disabled = movesTabDisabledProperty
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
        `class` := "col-9",
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
        `class` := "col-3 px-0",
        div(
          `class` := "btn-group m-3 mt-5",
          missesMovesCheckbox,
          label(
            `class` := "input-group-text",
            `for` := "missesMoves-checkbox",
            style := "user-select: none",
            nested(translatedDynamic(Translations.Game.missesMoves)(_.apply()))
          )
        ),
        div(
          `class` := "btn-group m-3",
          disabledMovesCheckbox,
          label(
            `class` := "input-group-text",
            `for` := "disabledMoves-checkbox",
            style := "user-select: none",
            nested(translatedDynamic(Translations.Game.disabledMoves)(_.apply()))
          )
        )
      )
    )
  }

  private def messagesTabItem(nested: Binding.NestedInterceptor): Div =
    div(
      `class` := "tab-pane fade" +
        (if (presenter.selectedTabProperty.get == ScreenModel.chatTab)
           " show active"
         else ""),
      id := ScreenModel.chatTab,
      role := "tabpanel",
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
    ).render

  private def turnPlaysToHtml(showCheckbox: Boolean, turnPlay: TurnPlay): Seq[dom.Element] = {
    val sqSize = 9
    val fleetMaxSize: Coordinate = {
      val size =
        gameStateModel.get.gameState.map(_.rules.gameFleet.size).getOrElse(Coordinate.origin)
      if (size.y > size.x)
        size.flipCoor
      else
        size
    }
    val canvasSize: Coordinate = fleetMaxSize * sqSize + Coordinate.square(4)

    def emptyCanvasDiv: Div = {
      val emptyCanvas: Canvas = canvas(`class` := "mr-3").render
      emptyCanvas.setAttribute("width", canvasSize.x.toString)
      emptyCanvas.setAttribute("height", canvasSize.y.toString)
      div(emptyCanvas).render
    }

    val toTurnString = turnPlay.turn.toTurnString
    val checkId = "check" + toTurnString

    val inputCheckBox =
      input(
        id := checkId,
        `type` := "checkbox"
      ).render

    val turnHits: Div =
      div(
        turnPlay.hitHints.flatMap {
          case HitHint.Water =>
            Seq[Modifier](
              viewUtils.createWaterCanvas(canvasSize, sqSize),
              viewUtils.createEmptyCanvas(x = sqSize * 2, y = canvasSize.y)
            )
          case HitHint.ShipHit(shipId, destroyed) =>
            Seq[Modifier](
              viewUtils.createShipCanvas(canvasSize, sqSize, Ship.shipMaxXMap(shipId), destroyed),
              viewUtils.createEmptyCanvas(x = sqSize * 2, y = canvasSize.y)
            )
        }
      ).render

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
      ChatStyles.messagesWindow,
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
      ChatStyles.messagesWindow,
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
    factory.disabled(presenter.gameModeProperty.transform(_.isEmpty))(nested =>
      nested(
        UdashInputGroup()(
          UdashInputGroup.input(msgInput.render),
          UdashInputGroup.appendButton(submitButton)
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
      nested(produceWithNested(presenter.gameStateProperty.transform(_.isEmpty)) {
        case (emptyGameState, nested) =>
          span(
            nested(
              translatedDynamic(
                if (emptyGameState) Translations.Game.logoutButton
                else Translations.Game.quitGameButton
              )(_.apply())
            )
          ).render
      }),
      span(`class` := "pl-2", FontAwesome.Solid.signOutAlt)
    )
  )

  private val errorModalId: String = "error-modal"
  def startGameErrorModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := errorModalId,
      div(
        `class` := "modal-dialog",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            h5(nested(translatedDynamic(Translations.Game.cannotStartGameTitle)(_.apply()))),
            span(
              FontAwesome.Solid.times,
              FontAwesome.Modifiers.Sizing.x2,
              attr("data-bs-dismiss") := "modal"
            )
          ),
          div(
            `class` := "modal-body",
            nested(produceWithNested(screenModel.subProp(_.errorModalType)) {
              case (Some(ErrorModalType.SmallBoardError), nested) =>
                span(
                  nested(translatedDynamic(Translations.Game.startGameBoardSizeError)(_.apply()))
                ).render
              case (Some(ErrorModalType.EmptyFleetError), nested) =>
                span(
                  nested(translatedDynamic(Translations.Game.startGameEmptyFleetError)(_.apply()))
                ).render
              case _ =>
                span.render
            })
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-primary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.closeButton)(_.apply()))
            )
          )
        )
      )
    ).render

  private val quitGameModalId: String = "quit-game-modal"
  def quitGameModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := quitGameModalId,
      div(
        `class` := "modal-dialog",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            h5(nested(translatedDynamic(Translations.Game.confirmQuitGameTitle)(_.apply()))),
            span(
              FontAwesome.Solid.times,
              FontAwesome.Modifiers.Sizing.x2,
              attr("data-bs-dismiss") := "modal"
            )
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-secondary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.closeButton)(_.apply()))
            ),
            button(
              `class` := "btn btn-primary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.confirmButton)(_.apply()))
            ).render.tap {
              _.onclick = _ => {
                presenter.quitCurrentGame()
              }
            }
          )
        )
      )
    ).render

  quitGameButton.listen { _ =>
    presenter.gameStateProperty.get match {
      case None =>
        presenter.logout()
      case Some(_) =>
        Globals.modalToggle(quitGameModalId)
    }
  }

  screenModel.subProp(_.errorModalType).listen {
    case Some(_) =>
      Globals.modalToggle(errorModalId)
    case None =>
  }

  override def getTemplate: Modifier = div(
    UdashCard(componentId = ComponentId("game-panel"))(factory =>
      Seq(
        factory.header(nested =>
          div(
            `class` := "row justify-content-between",
            startGameErrorModal(nested),
            quitGameModal(nested),
            div(
              `class` := "col-7",
              span(
                nested(translatedDynamic(Translations.Game.loggedInAs)(_.apply())),
                " ",
                b(bind(chatModel.subProp(_.username)))
              ),
              nested(produceWithNested(presenter.enemyProperty.transform(_.map(_.username))) {
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
                case (Some(PreGameMode(iPlacedShips, enemyPlacedShips)), nested) =>
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
                  span(b(toTimeStr(timeRemaining.totalTimeRemainingMillis / 1000))),
                  span(b(toShortTimeStr(timeRemaining.turnTimeRemainingMillisOpt.map(_ / 1000))))
                ).render

              div(
                `class` := "col-3 row",
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
                    case _ =>
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
                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.map(_._2))) {
                    case Some(enemyTimeRemaining) =>
                      showTime(enemyTimeRemaining)
                    case _ =>
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
            nested(produceWithNested(gameStateModel.transform(_.gameState.isEmpty)) {
              case (true, nested) =>
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
              case (false, _) =>
                boardView.canvasDiv
            })
          )
        ),
        factory.footer(nested => nested(mainGameForm)),
        factory.body(messagesTab),
        factory.footer(nested => nested(msgForm))
      )
    )
  )

}
