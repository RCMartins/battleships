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
import org.scalajs.dom.html.{Canvas, Div, Input, LI, Span}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game._
import pt.rmartins.battleships.shared.css.{ChatStyles, GameStyles}
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils._
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

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

  private def createNotification(numberOpt: Option[Int]): Span =
    numberOpt match {
      case None         => span.render
      case Some(number) => span(`class` := "badge badge-light ml-1", number).render
    }

  private def createChatTabButton(nested: NestedInterceptor): Div =
    div(
      `class` := "nav-link btn-outline-primary",
      GameStyles.cursorPointer,
      GameStyles.unselectableText,
      nested(translatedDynamic(Translations.Game.chatTab)(_.apply())),
      nested(produce(presenter.chatMessagesShowNotification)(createNotification))
    ).render

  private def createMyMovesTabButton(nested: NestedInterceptor): Div =
    div(
      `class` := "nav-link btn-outline-primary",
      GameStyles.cursorPointer,
      GameStyles.unselectableText,
      nested(translatedDynamic(Translations.Game.myMovesTab)(_.apply())),
      nested(produce(presenter.myMovesHistoryShowNotification)(createNotification))
    ).render

  private def createEnemyMovesTabButton(nested: NestedInterceptor): Div =
    div(
      `class` := "nav-link btn-outline-primary",
      GameStyles.cursorPointer,
      GameStyles.unselectableText,
      nested(translatedDynamic(Translations.Game.enemyMovesTab)(_.apply())),
      nested(produce(presenter.enemyMovesHistoryShowNotification)(createNotification))
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
  }

  private def createMessagesTabItem(nested: NestedInterceptor): Div =
    div(
      `class` := "tab-pane fade",
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

  private def createMyMovesTabItem(nested: NestedInterceptor): Div = {
    val showAllMovesCheckbox: Input =
      input(
        `class` := "form-control px-1",
        id := "allMoves-checkbox",
        width := "32px",
        `type` := "checkbox"
      ).render

    showAllMovesCheckbox.onchange = _ => {
      screenModel.subProp(_.showAllMoves).set(showAllMovesCheckbox.checked)
    }

    val missesMovesCheckbox: Input =
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

    val disabledMovesCheckbox: Input =
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

    val optionsContentDiv: Div =
      div(
        `class` := "position-absolute mr-4 border rounded border-black invisible",
        style := "left: 0; bottom: 0",
        div(
          `class` := "row m-0 px-0 bg-white",
          div(
            `class` := "col-12 btn-group m-2",
            showAllMovesCheckbox,
            label(
              `class` := "input-group-text",
              `for` := "allMoves-checkbox",
              GameStyles.unselectableText,
              span(
                `class` := "pl-1",
                nested(translatedDynamic(Translations.Game.allMoves)(_.apply()))
              )
            )
          ),
          div(
            `class` := "col-12 btn-group m-2 ml-5",
            missesMovesCheckbox,
            label(
              `class` := "input-group-text",
              `for` := "missesMoves-checkbox",
              GameStyles.unselectableText,
              span(
                `class` := "pl-1",
                nested(translatedDynamic(Translations.Game.missesMoves)(_.apply()))
              )
            )
          ),
          div(
            `class` := "col-12 btn-group m-2 ml-5",
            disabledMovesCheckbox,
            label(
              `class` := "input-group-text",
              `for` := "disabledMoves-checkbox",
              GameStyles.unselectableText,
              span(
                `class` := "pl-1",
                nested(translatedDynamic(Translations.Game.disabledMoves)(_.apply()))
              )
            )
          )
        )
      ).render

    screenModel
      .subProp(_.showAllMoves)
      .listen(
        { showAllMoves =>
          if (showAllMovesCheckbox.checked != showAllMoves)
            showAllMovesCheckbox.checked = showAllMoves
          missesMovesCheckbox.disabled = showAllMovesCheckbox.checked
          disabledMovesCheckbox.disabled = showAllMovesCheckbox.checked
        },
        initUpdate = true
      )

    div(
      `class` := "position-relative tab-pane fade",
      role := "tabpanel",
      div(
        ChatStyles.myMovesWindow,
        nested(produceWithNested(screenModel.subProp(_.showAllMoves)) {
          case (showAllMoves, nested) =>
            div(
              nested(produce(presenter.myMovesHistoryProperty) { turnPlaySeqProperty =>
                turnPlaySeqProperty.flatMap(turnPlay =>
                  turnPlaysToHtml(showCheckbox = !showAllMoves, turnPlay)
                )
              })
            ).render
        })
      ),
      optionsContentDiv,
      div(
        `class` := "p-1 position-absolute",
        style := "left: 0; bottom: 0",
        span(
          GameStyles.cursorPointer,
          FontAwesome.fas("gear"),
          FontAwesome.Modifiers.Sizing.x2,
        ),
      ).render.tap { optionsDiv =>
        optionsDiv.onclick = _ => {
          if (optionsContentDiv.classList.contains("invisible"))
            optionsContentDiv.classList.remove("invisible")
          else
            optionsContentDiv.classList.add("invisible")
        }
      }
    ).render
  }

  private def createEnemyMovesTabItem(nested: NestedInterceptor): Div =
    div(
      `class` := "tab-pane fade",
      role := "tabpanel",
      ChatStyles.enemyMovesWindow,
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
                GameStyles.unselectableText,
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

  def chatAndMovesDiv(nested: NestedInterceptor): Modifier = {
    def makeNavItem(div: Div): JsDom.TypedTag[LI] =
      li(
        `class` := "nav-item",
        role := "presentation",
        div
      )

    def setupButtonDiv(div: Div, newSelectedTab: String): Unit =
      div.onclick = _ => {
        presenter.resetLastSeenMessages()
        screenModel.subProp(_.selectedTab).set(newSelectedTab)
        presenter.resetLastSeenMessages()
      }

    val chatTabButton: Div =
      createChatTabButton(nested).tap(setupButtonDiv(_, ScreenModel.chatTab))
    val myMovesTabButton: Div =
      createMyMovesTabButton(nested).tap(setupButtonDiv(_, ScreenModel.myMovesTab))
    val enemyMovesTabButton: Div =
      createEnemyMovesTabButton(nested).tap(setupButtonDiv(_, ScreenModel.enemyMovesTab))

    val messagesTabItem: Div = createMessagesTabItem(nested)
    val myMovesTabItem: Div = createMyMovesTabItem(nested)
    val enemyMovesTabItem: Div = createEnemyMovesTabItem(nested)

    val allTabButtons: List[(String, Div, Div)] =
      List(
        (ScreenModel.chatTab, chatTabButton, messagesTabItem),
        (ScreenModel.myMovesTab, myMovesTabButton, myMovesTabItem),
        (ScreenModel.enemyMovesTab, enemyMovesTabButton, enemyMovesTabItem),
      )

    screenModel
      .subProp(_.selectedTab)
      .listen(
        { newSelectedTab =>
          allTabButtons.foreach {
            case (tabStr, divButton, divContent) if tabStr == newSelectedTab =>
              divButton.classList.remove("btn-outline-primary")
              divButton.classList.add("btn-primary")
              divContent.classList.add("show")
              divContent.classList.add("active")
            case (_, div, divContent) =>
              div.classList.remove("btn-primary")
              div.classList.add("btn-outline-primary")
              divContent.classList.remove("show")
              divContent.classList.remove("active")
          }
        },
        initUpdate = true
      )

    div(
      `class` := "my-2 p-1 border rounded border-black",
      div(
        ul(
          `class` := "nav nav-pills nav-justified",
          `role` := "tablist",
          makeNavItem(chatTabButton),
          makeNavItem(myMovesTabButton),
          makeNavItem(enemyMovesTabButton)
        ),
        div(
          `class` := "tab-content",
          messagesTabItem,
          myMovesTabItem,
          enemyMovesTabItem
        )
      )
    )
  }

}
