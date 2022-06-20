package pt.rmartins.battleships.frontend.views.menu

import io.udash._
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.form.UdashForm.FormEvent
import io.udash.bootstrap.form.{UdashForm, UdashInputGroup}
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n._
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.{Canvas, Div, LI}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.frontend.views.game._
import pt.rmartins.battleships.frontend.views.model.ModeType.{GameOverModeType, PlayingModeType}
import pt.rmartins.battleships.shared.css.ChatStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils._
import scalatags.JsDom
import scalatags.JsDom.all._

class ChatUtils(
    chatModel: ModelProperty[ChatModel],
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    presenter: GamePresenter,
    boardView: BoardView,
    preGameView: PreGameView,
    gameModals: GameModals,
    viewUtils: ViewUtils,
    translationsService: TranslationsService,
) extends CssView {

  import translationsService._

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
  }

  private def messagesTabItem(nested: NestedInterceptor): Div =
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
    ).render

  private def myMovesTabItem(nested: NestedInterceptor): Div =
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

  private def enemyMovesTabItem(nested: NestedInterceptor): Div =
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
              presenter.gameStateProperty.get match {
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

  private def selectedTabToButtonId(id: String): String =
    id match {
      case ScreenModel.chatTab       => chatTabButton.componentId.value
      case ScreenModel.myMovesTab    => myMovesTabButton.componentId.value
      case ScreenModel.enemyMovesTab => enemyMovesTabButton.componentId.value
    }

  presenter.selectedTabProperty.listen { selectedTab =>
    def updateClass(id: String): Unit =
      if (selectedTab == id) {
        Option(document.getElementById(selectedTabToButtonId(id)))
          .foreach(_.classList.add("active"))
        Option(document.getElementById(id)).foreach { elem =>
          elem.classList.add("show")
          elem.classList.add("active")
        }
      } else {
        Option(document.getElementById(selectedTabToButtonId(id)))
          .foreach(_.classList.remove("active"))
        Option(document.getElementById(id)).foreach { elem =>
          elem.classList.remove("show")
          elem.classList.remove("active")
        }
      }

    List(ScreenModel.chatTab, ScreenModel.myMovesTab, ScreenModel.enemyMovesTab)
      .foreach(updateClass)
  }

  def chatAndMovesDiv(nested: NestedInterceptor): Modifier = {
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

}
