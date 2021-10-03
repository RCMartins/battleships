package pt.rmartins.battleships.frontend.views.game

import io.udash._
import io.udash.bindings.modifiers.Binding
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.card.UdashCard
import io.udash.bootstrap.form.UdashForm.FormEvent
import io.udash.bootstrap.form.{UdashForm, UdashInputGroup}
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.component.ComponentId
import io.udash.css._
import io.udash.i18n._
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.{MouseEvent, UIEvent, WheelEvent, window}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.shared.css.ChatStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode.{InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._

import scala.util.chaining.scalaUtilChainingOps

class GameView(
    gameModel: ModelProperty[GameModel],
    chatModel: ModelProperty[ChatModel],
    screenModel: ModelProperty[ScreenModel],
    presenter: GamePresenter,
    translationsService: TranslationsService
) extends View
    with CssView {

  import scalatags.JsDom.all._
  import translationsService._

  private val MainGameCanvasId = "mainGameCanvas"

  private val myBoardCanvas: Canvas =
    canvas(
      id := MainGameCanvasId
    ).render

  private val canvasDiv: Div =
    div(id := "canvas-div", myBoardCanvas).render

  window.onresize = (_: UIEvent) => {
    presenter.onCanvasResize(canvasDiv)
  }

  screenModel.listen { case ScreenModel(canvasSize, _) =>
    myBoardCanvas.setAttribute("width", canvasSize.x.toString)
    myBoardCanvas.setAttribute("height", canvasSize.y.toString)
    boardView.paint(gameModel.get)
  }

  screenModel.get.canvasSize.pipe { canvasSize =>
    myBoardCanvas.setAttribute("width", canvasSize.x.toString)
    myBoardCanvas.setAttribute("height", canvasSize.y.toString)
  }

  private val boardView = new BoardView(gameModel, screenModel, presenter, myBoardCanvas)

  private def gameCanvas(nested: Binding.NestedInterceptor) =
    div(
      nested(produce(gameModel) { gameModel =>
        boardView.paint(gameModel)
        canvasDiv
      })
    )

  myBoardCanvas.onmousemove = (mouseEvent: MouseEvent) => {
    val rect = myBoardCanvas.getBoundingClientRect()
    presenter.mouseMove(
      mouseEvent.clientX.toInt - rect.left.toInt,
      mouseEvent.clientY.toInt - rect.top.toInt
    )
  }

  myBoardCanvas.onmouseleave = (_: MouseEvent) => {
    presenter.mouseLeave()
  }

  myBoardCanvas.onmousedown = (_: MouseEvent) => {
    presenter.mouseClick(boardView)
    false
  }

  myBoardCanvas.onmousewheel = (wheelEvent: WheelEvent) => {
    presenter.mouseWheel(wheelEvent.deltaY.toInt / 100)
    false
  }

//  myBoardCanvas.onkeydown = (keyboardEvent: KeyboardEvent) => {
//    presenter.keyDown(keyboardEvent.key)
//  }

  private val mainStartButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("start-game-button")
  )(_ => Seq(span("Start Game!"), tpe := "submit"))

  private val confirmShipsButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("confirm-button"),
    disabled = gameModel.subProp(_.myGameState).transform(!_.exists(_.me.shipsLeftToPlace.isEmpty))
  )(_ => Seq(span("Confirm")))

  private val undoButton = UdashButton(
    buttonStyle = Color.Secondary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("undo-button"),
    disabled = gameModel.subProp(_.myGameState).transform(!_.exists(_.me.myBoard.ships.nonEmpty))
  )(_ =>
    Seq(
      span("Undo"),
      `class` := "bi bi-arrow-counterclockwise"
    )
  )

  private val resetButton = UdashButton(
    buttonStyle = Color.Danger.toProperty,
    block = true.toProperty,
    componentId = ComponentId("reset-button"),
    disabled = gameModel.subProp(_.myGameState).transform(!_.exists(_.me.myBoard.ships.nonEmpty))
  )(_ => Seq(span("Reset")))

  private val randomPlacementButton = UdashButton(
    buttonStyle = Color.Secondary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("random-button"),
    disabled = gameModel.subProp(_.myGameState).transform(!_.exists(_.me.shipsLeftToPlace.nonEmpty))
  )(_ => Seq(span("Random")))

  private val launchAttackButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("launch-attack-button"),
    disabled = presenter.inGameModeOpt.transform(
      !_.exists(inGameMode => inGameMode.isMyTurn && inGameMode.turnAttacks.forall(_.isPlaced))
    )
  )(_ => Seq(span("Launch Attack!")))

  private val mainGameForm = UdashForm(
    componentId = ComponentId("main-game-from")
  )(factory =>
    factory.disabled(presenter.hasWriteAccess.toProperty.transform(!_))(nested =>
      nested(produce(gameModel.subProp(_.myGameState)) {
        case None =>
          UdashInputGroup()(UdashInputGroup.appendButton(mainStartButton)).render
        case Some(GameState(_, _, _, PreGameMode(_, _, _))) =>
          UdashInputGroup()(
            Seq(
              UdashInputGroup.appendButton(confirmShipsButton),
              UdashInputGroup.appendButton(undoButton),
              UdashInputGroup.appendButton(resetButton),
              UdashInputGroup.appendButton(randomPlacementButton)
            )
          ).render
        case Some(GameState(_, _, _, InGameMode(_, _, _))) =>
          UdashInputGroup()(
            Seq(
              UdashInputGroup.appendButton(launchAttackButton)
            )
          ).render
        case _ =>
          span.render
      })
    )
  )

  mainStartButton.listen { _ =>
    presenter.startGameWith()
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

  private val chatTabButton: UdashButton =
    UdashButton(
      //            buttonStyle = Color.Primary.toProperty
    )(nested =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary",
        data("bs-toggle") := "tab",
        data("bs-target") := "#chat",
        //              aria.controls := "Chat",
        //              aria.selected := true,
        //              attr("aria-current") := "page",
        "Chat"
      )
    )

  private val myMovesTabButton: UdashButton =
    UdashButton(
      //              buttonStyle = Color.Primary.toProperty
    )(_ =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary",
        data("bs-toggle") := "tab",
        data("bs-target") := "#my-moves",
        "My Moves"
      )
    )

  private val enemyMovesTabButton: UdashButton =
    UdashButton(
      //              buttonStyle = Color.Primary.toProperty
    )(_ =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary",
        data("bs-toggle") := "tab",
        data("bs-target") := "#enemy-moves",
        "Enemy Moves"
      )
    )

  chatTabButton.listen { _ =>
    presenter.setSelectedTab("chat")
  }

  myMovesTabButton.listen { _ =>
    presenter.setSelectedTab("my-moves")
  }

  enemyMovesTabButton.listen { _ =>
    presenter.setSelectedTab("enemy-moves")
  }

  private def messagesTab(nested: Binding.NestedInterceptor): Binding = {
    val navTabs =
      ul(
        `class` := "nav nav-tabs",
        `role` := "tablist",
        li(
          `class` := "nav-item",
          role := "presentation",
          chatTabButton
        ),
        showIf(presenter.inGameMode)(
          li(
            `class` := "nav-item",
            role := "presentation",
            myMovesTabButton
          ).render
        ),
        showIf(presenter.inGameMode)(
          li(
            `class` := "nav-item",
            role := "presentation",
            enemyMovesTabButton
          ).render
        )
      )

    nested(produceWithNested(gameModel.subProp(_.myGameState)) {
      case (None, nested2) =>
        Seq(
          navTabs,
          div(
            `class` := "tab-content",
            messagesTabItem(nested2)
          )
        ).render
      case (Some(_), nested2) =>
        Seq(
          navTabs,
          div(
            `class` := "tab-content",
            messagesTabItem(nested2),
            myMovesTabItem(nested2),
            enemyMovesTabItem(nested2)
          )
        ).render
    })
  }

  private def messagesTabItem(nested: Binding.NestedInterceptor) =
    div(
      `class` := "tab-pane fade" + (if (screenModel.get.selectedTab == "chat") " show active"
                                    else ""),
      id := "chat",
      role := "tabpanel",
      ChatStyles.messagesWindow,
      nested(repeat(chatModel.subSeq(_.msgs)) { msgProperty =>
        val msg = msgProperty.get
        div(
          ChatStyles.msgContainer,
          strong(msg.author, ": "),
          span(msg.text),
          span(ChatStyles.msgDate, msg.created.toString)
        ).render
      })
    )

  private def myMovesTabItem(nested: Binding.NestedInterceptor) =
    div(
      `class` := "tab-pane fade" + (if (screenModel.get.selectedTab == "my-moves") " show active"
                                    else ""),
      id := "my-moves",
      role := "tabpanel",
      ChatStyles.messagesWindow,
      nested(
        repeat(
          gameModel
            .subProp(_.myGameState)
            .transformToSeq(_.map(_.me.turnPlayHistory).getOrElse(Seq.empty))
        ) { turnPlayProperty =>
          val TurnPlay(turnNumber, _, hitHints) = turnPlayProperty.get
          div(
            ChatStyles.msgContainer,
            strong(turnNumber, ": "),
            span(
              hitHints
                .map {
                  case HitHint.Water =>
                    "Water"
                  case HitHint.ShipHit(shipId, destroyed) =>
                    val shipName = Ship.allShipsNames(shipId)
                    if (destroyed) shipName + " destroyed!" else shipName
                }
                .mkString(", ")
            )
          ).render
        }
      )
    )

  private def enemyMovesTabItem(nested: Binding.NestedInterceptor) =
    div(
      `class` := "tab-pane fade" + (if (screenModel.get.selectedTab == "enemy-moves") " show active"
                                    else ""),
      id := "enemy-moves",
      role := "tabpanel",
      ChatStyles.messagesWindow,
      nested(
        repeat(
          gameModel
            .subProp(_.myGameState)
            .transformToSeq(_.map(_.enemy.turnPlayHistory).getOrElse(Seq.empty))
        ) { turnPlayProperty =>
          val TurnPlay(turnNumber, _, hitHints) = turnPlayProperty.get
          div(
            ChatStyles.msgContainer,
            strong(turnNumber, ": "),
            span(
              hitHints
                .map {
                  case HitHint.Water =>
                    "Water"
                  case HitHint.ShipHit(shipId, destroyed) =>
                    val shipName = Ship.allShipsNames(shipId)
                    if (destroyed) shipName + " destroyed!" else shipName
                }
                .mkString(", ")
            )
          ).render
        }
      )
    )

  // Standard Udash TextInput (we don't need Bootstrap Forms input wrapping)
  private val msgInput = TextInput(chatModel.subProp(_.msgInput))(
    translatedAttrDynamic(Translations.Chat.inputPlaceholder, "placeholder")(
      _.apply()
    )
  )

  // Button from Udash Bootstrap wrapper
  private val submitButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("msg-send")
  )(_ => Seq(span(FontAwesome.Solid.paperPlane), tpe := "submit"))

  private val msgForm = UdashForm(
    componentId = ComponentId("msg-from")
  )(factory =>
    factory.disabled(presenter.hasWriteAccess.toProperty.transform(!_))(nested =>
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
  }

  private val quitGameButton = UdashButton(
    buttonStyle = Color.Danger.toProperty,
    componentId = ComponentId("quit-game-button")
  )(_ => Seq(span("Quit Game")))

  quitGameButton.listen { _ =>
    presenter.quitCurrentGame()
  }

  override def getTemplate: Modifier = div {
    UdashCard(componentId = ComponentId("game-panel"))(factory =>
      Seq(
        factory.header(nested =>
          nested(produceWithNested(chatModel.subProp(_.username)) { case (username, nested) =>
            div(
              `class` := "row justify-content-between",
              div(
                `class` := "col",
                if (username.nonEmpty)
                  span(s"Logged in as ", b(username)).render
                else
                  span("").render,
                br,
                nested(produce(gameModel.subProp(_.myGameState)) {
                  case Some(GameState(_, _, _, GameMode.PreGameMode(_, _, _))) =>
                    val placeShipsBinding =
                      translatedDynamic(Translations.Game.placeShips)(_.apply())
                    span(color := "#FF0000", b(placeShipsBinding)).render
                  case Some(GameState(_, _, _, GameMode.InGameMode(isFirstPlayer, halfTurns, _))) =>
                    val isMyTurn =
                      halfTurns % 2 == (if (isFirstPlayer) 1 else 0)

                    val turnStrBinding: Binding =
                      translatedDynamic(
                        if (isMyTurn)
                          Translations.Game.yourTurn
                        else
                          Translations.Game.enemyTurn
                      )(_.apply())

                    span(color := "#FF0000", b(turnStrBinding)).render
                  case _ =>
                    span("").render
                })
              ),
              div(
                `class` := "col container",
                div(
                  `class` := "row justify-content-end",
                  showIf(gameModel.subProp(_.myGameState).transform(_.nonEmpty))(
                    span(quitGameButton).render
                  )
                )
              )
            ).render
          })
        ),
        factory.body(nested =>
          Seq(
            `class` := "p-0",
            gameCanvas(nested)
          )
        ),
        factory.footer(nested => nested(mainGameForm)),
//        factory.header(nested =>
//          nested(produce(chatModel.subProp(_.connectionsCount)) { connectionsCount =>
//            span(s"Chat: $connectionsCount").render
//          })
//        ),
        factory.body(messagesTab),
        factory.footer(nested => nested(msgForm))
      )
    )
  }

}
