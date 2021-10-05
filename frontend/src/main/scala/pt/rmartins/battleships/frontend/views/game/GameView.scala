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
import org.scalajs.dom
import org.scalajs.dom.html.{Canvas, Div, Span}
import org.scalajs.dom.{MouseEvent, Node, UIEvent, WheelEvent, window}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.shared.css.ChatStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class GameView(
    gameModel: ModelProperty[GameModel],
    gameStateModel: ModelProperty[GameStateModel],
    chatModel: ModelProperty[ChatModel],
    screenModel: ModelProperty[ScreenModel],
    presenter: GamePresenter,
    translationsService: TranslationsService
) extends View
    with CssView {

  import translationsService._

  private val MainGameCanvasId = "mainGameCanvas"

  private val myBoardCanvas: Canvas =
    canvas(
      id := MainGameCanvasId
    ).render

  screenModel.get.canvasSize.pipe { canvasSize =>
    myBoardCanvas.setAttribute("width", canvasSize.x.toString)
    myBoardCanvas.setAttribute("height", canvasSize.y.toString)
  }

  private val canvasDiv: Div =
    div(id := "canvas-div", myBoardCanvas).render

  window.onresize = (_: UIEvent) => {
    presenter.onCanvasResize(canvasDiv)
  }

  window.setTimeout(
    () => presenter.onCanvasResize(canvasDiv),
    1
  )

  private val boardView =
    new BoardView(gameModel, screenModel, presenter, myBoardCanvas)

  private def reloadBoardView(): Unit = {
    val canvasSize = screenModel.get.canvasSize
    if (myBoardCanvas.clientWidth != canvasSize.x || myBoardCanvas.clientHeight != canvasSize.y) {
      myBoardCanvas.setAttribute("width", canvasSize.x.toString)
      myBoardCanvas.setAttribute("height", canvasSize.y.toString)
    }
    boardView.paint()
  }

  screenModel.subProp(_.canvasSize).listen(_ => reloadBoardView())
  gameStateModel.listen(_ => reloadBoardView())
  gameModel.listen(_ => reloadBoardView())

  myBoardCanvas.onmousemove = (mouseEvent: MouseEvent) => {
    val rect = myBoardCanvas.getBoundingClientRect()
    presenter.mouseMove(
      boardView,
      mouseEvent.clientX.toInt - rect.left.toInt,
      mouseEvent.clientY.toInt - rect.top.toInt
    )
  }

  myBoardCanvas.onmouseleave = (_: MouseEvent) => {
    presenter.mouseLeave()
  }

  myBoardCanvas.onmousedown = (mouseEvent: MouseEvent) => {
    presenter.mouseClick(boardView, mouseEvent.button)
  }
  }

  myBoardCanvas.onmousewheel = (wheelEvent: WheelEvent) => {
    presenter.mouseWheel(wheelEvent.deltaY.toInt / 100)
  }

  myBoardCanvas.oncontextmenu = (event: MouseEvent) => {
    event.preventDefault()
  }

  private val mainStartButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("start-game-button")
  )(_ => Seq[Modifier](span("Start Game!"), tpe := "submit"))

  private val confirmShipsButton = UdashButton(
    buttonStyle = Color.Primary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("confirm-button"),
    disabled = presenter.gameStateProperty.transform(!_.exists(_.me.shipsLeftToPlace.isEmpty))
  )(_ => Seq[Modifier](span("Confirm")))

  private val undoButton = UdashButton(
    buttonStyle = Color.Secondary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("undo-button"),
    disabled = presenter.gameStateProperty.transform(!_.exists(_.me.myBoard.ships.nonEmpty))
  )(_ =>
    Seq[Modifier](
      span("Undo")
    ) //, `class` := "bi bi-arrow-counterclockwise")
  )

  private val resetButton = UdashButton(
    buttonStyle = Color.Danger.toProperty,
    block = true.toProperty,
    componentId = ComponentId("reset-button"),
    disabled = presenter.gameStateProperty.transform(!_.exists(_.me.myBoard.ships.nonEmpty))
  )(_ => Seq[Modifier](span("Reset")))

  private val randomPlacementButton = UdashButton(
    buttonStyle = Color.Secondary.toProperty,
    block = true.toProperty,
    componentId = ComponentId("random-button"),
    disabled = presenter.gameStateProperty.transform(!_.exists(_.me.shipsLeftToPlace.nonEmpty))
  )(_ => Seq[Modifier](span("Random")))

  private val launchAttackButton = {
    val launchAttackIsDisabledProperty =
      presenter.inGameModeProperty.combine(gameModel.subProp(_.turnAttacks)) {
        case (inGameMode, turnAttacks) =>
          !inGameMode.exists(inGameMode => inGameMode.isMyTurn && turnAttacks.forall(_.isPlaced))
      }

    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("launch-attack-button"),
      disabled = launchAttackIsDisabledProperty
    )(nested =>
      Seq(nested(produce(presenter.isMyTurnProperty) {
        case true  => span("Launch Attack!").render
        case false => span("Wait for your turn").render
      }))
    )
  }

  private val restartButton = {
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("restart-button")
    )(nested => Seq(nested(translatedDynamic(Translations.Game.restartButton)(_.apply()))))
  }

  private val mainGameForm = UdashForm(
    componentId = ComponentId("main-game-from")
  )(factory =>
    factory.disabled(presenter.hasWriteAccess.toProperty.transform(!_))(nested =>
      nested(produce(presenter.gameModeProperty) {
        case None =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", mainStartButton)
          ).render
        case Some(PreGameMode(false, _)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", confirmShipsButton),
            div(`class` := "mx-1", undoButton),
            div(`class` := "mx-1", resetButton),
            div(`class` := "mx-1", randomPlacementButton)
          ).render
        case Some(PreGameMode(true, _)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", undoButton),
            div(`class` := "mx-1", resetButton)
          ).render
        case Some(InGameMode(_, _, _)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", launchAttackButton)
          ).render
        case Some(GameOverMode(_, _)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", restartButton)
          ).render
//        case None =>
//          UdashInputGroup()(UdashInputGroup.appendButton(mainStartButton)).render
//        case Some(PreGameMode(_, false, _)) =>
//          UdashInputGroup()(
//            Seq(
//              UdashInputGroup.appendButton(confirmShipsButton),
//              UdashInputGroup.appendButton(undoButton),
//              UdashInputGroup.appendButton(resetButton),
//              UdashInputGroup.appendButton(randomPlacementButton)
//            )
//          ).render
//        case Some(PreGameMode(_, true, _)) =>
//          UdashInputGroup()(
//            Seq(
//              UdashInputGroup.appendButton(undoButton),
//              UdashInputGroup.appendButton(resetButton)
//            )
//          ).render
//        case Some(InGameMode(_, _, _)) =>
//          UdashInputGroup()(
//            Seq(
//              UdashInputGroup.appendButton(launchAttackButton)
//            )
//          ).render
//        case Some(GameOverMode(_, _)) =>
//          UdashInputGroup()(
//            Seq(
//              UdashInputGroup.appendButton(restartButton)
//            )
//          ).render
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

  restartButton.listen { _ =>
    presenter.restartGame()
  }

  private val chatTabButton: UdashButton =
    UdashButton()(_ =>
      Seq(
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.chatTab) " active" else ""),
        data("bs-toggle") := "tab",
        data("bs-target") := "#" + ScreenModel.chatTab,
        span("Chat")
      )
    )

  private val myMovesTabButton: UdashButton =
    UdashButton(
      disabled = presenter.inPreGameMode
    )(_ =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.myMovesTab) " active" else ""),
//        (`class` := "nav-link btn-outline-primary active")
//          .attrIf(presenter.selectedTabProperty.transform(_ == ScreenModel.myMovesTab)),
//        (`class` := "nav-link btn-outline-primary")
//          .attrIf(presenter.selectedTabProperty.transform(_ != ScreenModel.myMovesTab)),
//        `class` := "nav-link btn-outline-primary",
//        (`class` :+= "active")
//          .attrIf(presenter.selectedTabProperty.transform(_ == ScreenModel.myMovesTab)),
        data("bs-toggle") := "tab",
        data("bs-target") := "#" + ScreenModel.myMovesTab,
        "My Moves"
      )
    )

  private val enemyMovesTabButton: UdashButton =
    UdashButton(
      disabled = presenter.inPreGameMode
    )(_ =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.enemyMovesTab) " active" else ""),
//        (`class` := "nav-link btn-outline-primary active")
//          .attrIf(presenter.selectedTabProperty.transform(_ == ScreenModel.enemyMovesTab)),
//        (`class` := "nav-link btn-outline-primary")
//          .attrIf(presenter.selectedTabProperty.transform(_ != ScreenModel.enemyMovesTab)),
        data("bs-toggle") := "tab",
        data("bs-target") := "#" + ScreenModel.enemyMovesTab,
        "Enemy Moves"
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

  private def messagesTab(nested: Binding.NestedInterceptor) = {
    val navTabs =
      nested(produce(screenModel.subProp(_.selectedTab)) { _ =>
        ul(
          `class` := "nav nav-tabs",
          `role` := "tablist",
          li(
            `class` := "nav-item",
            role := "presentation",
            nested(chatTabButton)
          ),
          li(
            `class` := "nav-item",
            role := "presentation",
            nested(myMovesTabButton)
          ),
          li(
            `class` := "nav-item",
            role := "presentation",
            nested(enemyMovesTabButton)
          )
        ).render
      })

    Seq[Modifier](
      navTabs,
      div(
        `class` := "tab-content",
        messagesTabItem(nested),
        myMovesTabItem(nested),
        enemyMovesTabItem(nested)
      )
    )
  }

  private def messagesTabItem(nested: Binding.NestedInterceptor): Binding =
    nested(produceWithNested(presenter.selectedTabProperty) { case (selectedTab, nested2) =>
      div(
        `class` := "tab-pane fade" + (if (selectedTab == ScreenModel.chatTab) " show active"
                                      else ""),
        id := ScreenModel.chatTab,
        role := "tabpanel",
        ChatStyles.messagesWindow,
        nested2(repeat(chatModel.subSeq(_.msgs)) { msgProperty =>
          val msg = msgProperty.get
          div(
            ChatStyles.msgContainer,
            strong(msg.author, ": "),
            span(msg.text),
            span(ChatStyles.msgDate, msg.created.toString)
          ).render
        })
      ).render
    })

  private def turnPlaysToHtml(turnPlay: TurnPlay): Seq[dom.Element] =
    div(
      ChatStyles.msgContainer,
      strong(turnPlay.turnNumber, ": "),
      span(
        Utils.addSeparator(
          turnPlay.hitHints
            .map {
              case HitHint.Water =>
                span(color := "blue", "Water")
              case HitHint.ShipHit(shipId, destroyed) =>
                val shipName = Ship.allShipsNames(shipId)
                if (destroyed)
                  span(color := "red", b(shipName + " destroyed!"))
                else
                  span(shipName)
            },
          span(", ")
        )
      )
    ).render

  private def myMovesTabItem(nested: Binding.NestedInterceptor): Binding =
    nested(produceWithNested(presenter.selectedTabProperty) { case (selectedTab, nested2) =>
      div(
        `class` := "tab-pane fade" + (if (selectedTab == ScreenModel.myMovesTab) " show active"
                                      else ""),
        id := ScreenModel.myMovesTab,
        role := "tabpanel",
        ChatStyles.messagesWindow,
        nested2(
          repeat(
            presenter.gameStateProperty
              .transformToSeq(_.map(_.me.turnPlayHistory).getOrElse(Seq.empty))
          )(turnPlayProperty => turnPlaysToHtml(turnPlayProperty.get))
        )
      ).render
    })

  private def enemyMovesTabItem(nested: Binding.NestedInterceptor): Binding =
    nested(produceWithNested(presenter.selectedTabProperty) { case (selectedTab, nested2) =>
      div(
        `class` := "tab-pane fade" + (if (selectedTab == ScreenModel.enemyMovesTab) " show active"
                                      else ""),
        id := ScreenModel.enemyMovesTab,
        role := "tabpanel",
        ChatStyles.messagesWindow,
        nested2(
          repeat(
            presenter.gameStateProperty
              .transformToSeq(_.map(_.enemy.turnPlayHistory).getOrElse(Seq.empty))
          )(turnPlayProperty => turnPlaysToHtml(turnPlayProperty.get))
        )
      ).render
    })

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
                `class` := "col-8",
                if (username.nonEmpty)
                  span(s"Logged in as ", b(username)).render
                else
                  span("").render,
                nested(
                  produce(presenter.gameStateProperty.transform(_.map(_.enemy.username))) {
                    case Some(enemyUsername) if enemyUsername.nonEmpty =>
                      span(" - playing against ", b(enemyUsername)).render
                    case _ =>
                      span.render
                  }
                ),
                br,
                nested(produce(presenter.gameModeProperty) {
                  case Some(PreGameMode(iPlacedShips, enemyPlacedShips)) =>
                    val placeShipsBinding =
                      translatedDynamic(
                        if (iPlacedShips)
                          Translations.Game.placeShipsWaitEnemy
                        else if (enemyPlacedShips)
                          Translations.Game.placeShipsEnemyReady
                        else
                          Translations.Game.placeShips
                      )(_.apply())
                    span(color := "#FF0000", b(placeShipsBinding)).render
                  case Some(InGameMode(isFirstPlayer, halfTurns, _)) =>
                    val isMyTurn =
                      halfTurns % 2 == (if (isFirstPlayer) 1 else 0)
                    val currentTurn: Int =
                      (halfTurns + 1) / 2

                    val turnStrBinding: Binding =
                      translatedDynamic(
                        if (isMyTurn)
                          Translations.Game.yourTurn
                        else
                          Translations.Game.enemyTurn
                      )(_.apply())

                    span(
                      span("Turn "),
                      span(currentTurn),
                      span(": "),
                      span(color := "#FF0000", b(turnStrBinding))
                    ).render
                  case Some(GameOverMode(halfTurns, youWon)) =>
                    val currentTurn: Int =
                      (halfTurns + 1) / 2

                    val turnStrBinding: Binding =
                      translatedDynamic(
                        if (youWon)
                          Translations.Game.youWon
                        else
                          Translations.Game.enemyWon
                      )(_.apply())

                    span(
                      span("Turn "),
                      span(currentTurn),
                      span(": "),
                      span(color := "#FF0000", b(turnStrBinding))
                    ).render
                  case _ =>
                    span("").render
                })
              ),
              div(
                `class` := "col-4 container",
                div(
                  `class` := "row justify-content-end",
                  showIf(presenter.gameStateProperty.transform(_.nonEmpty))(
                    span(quitGameButton).render
                  )
                )
              )
            ).render
          })
        ),
        factory.body(_ =>
          Seq[Modifier](
            `class` := "p-0",
            canvasDiv
          )
        ),
        factory.footer(nested => nested(mainGameForm)),
        factory.body(messagesTab),
        factory.footer(nested => nested(msgForm))
      )
    )
  }

}
