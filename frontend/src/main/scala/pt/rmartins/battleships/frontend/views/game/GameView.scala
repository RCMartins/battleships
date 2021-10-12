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
import org.scalajs.dom.html.{Canvas, Div, LI}
import org.scalajs.dom.{MouseEvent, UIEvent, WheelEvent, window}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.css.ChatStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.chat.ChatMessage
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

  private val boardView: BoardView =
    new BoardView(gameModel, screenModel, presenter, myBoardCanvas)

  private def reloadBoardView(): Unit = {
    val canvasSize = screenModel.get.canvasSize
    if (myBoardCanvas.clientWidth != canvasSize.x || myBoardCanvas.clientHeight != canvasSize.y) {
      myBoardCanvas.setAttribute("width", canvasSize.x.toString)
      myBoardCanvas.setAttribute("height", canvasSize.y.toString)
    }

    if (screenModel.get.extraTurnText.isEmpty)
      screenModel
        .subProp(_.extraTurnText)
        .set(Some(span(translatedDynamic(Translations.Game.extraTurnPopup)(_.apply())).render))

    boardView.paint()
  }

  // TODO This is marking the screen refresh every time the one of the 'timeRemaining' values changes
  gameStateModel.listen(_ => reloadBoardView())
  gameModel.listen(_ => reloadBoardView())
  screenModel.subProp(_.canvasSize).listen(_ => reloadBoardView())

  myBoardCanvas.onmousemove = (mouseEvent: MouseEvent) => {
    val rect = myBoardCanvas.getBoundingClientRect()
    presenter.mouseMove(
      boardView,
      mouseEvent.clientX.toInt - rect.left.toInt, // TODO subtract AbsMargin?
      mouseEvent.clientY.toInt - rect.top.toInt
    )
  }

  myBoardCanvas.onmouseleave = (_: MouseEvent) => {
    presenter.mouseLeave()
  }

  myBoardCanvas.onmousedown = (mouseEvent: MouseEvent) => {
    presenter.mouseDown(boardView, mouseEvent.button)
    false // Prevent the mouse down from exiting the canvas
  }

  myBoardCanvas.onmouseup = (_: MouseEvent) => {
    presenter.mouseUp()
  }

  myBoardCanvas.onmousewheel = (wheelEvent: WheelEvent) => {
    presenter.mouseWheel(wheelEvent.deltaY.toInt / 100)
  }

  myBoardCanvas.oncontextmenu = (event: MouseEvent) => {
    event.preventDefault()
  }

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
      presenter.inGameModeProperty
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
      nested(produce(presenter.gameModeProperty.transform(_.map {
        case PreGameMode(iPlacedShips, _) => (iPlacedShips, false, false)
        case InGameMode(_, _, _, _, _)    => (false, true, false)
        case GameOverMode(_, _, _, _)     => (false, false, true)
      })) {
        case None =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", startGameVsBotButton),
            div(`class` := "mx-1", startGameVsPlayerButton)
          ).render
        case Some((false, false, false)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", confirmShipsButton),
            div(`class` := "mx-1", undoButton),
            div(`class` := "mx-1", resetButton),
            div(`class` := "mx-1", randomPlacementButton)
          ).render
        case Some((true, _, _)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", undoButton),
            div(`class` := "mx-1", resetButton)
          ).render
        case Some((_, true, _)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", launchAttackButton)
          ).render
        case Some((_, _, true)) =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", restartButton)
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

  private val chatMessagesProperty: ReadableSeqProperty[ChatMessage] =
    chatModel.subSeq(_.msgs)
  private val chatMessagesSizeProperty: ReadableProperty[Int] =
    chatModel.subSeq(_.msgs).transform(_.size)
  private val chatMessagesShowNotification: ReadableProperty[Option[Int]] =
    combine(
      chatMessagesSizeProperty,
      screenModel.subProp(_.lastSeenMessagesChat),
      screenModel.subProp(_.selectedTab)
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount).filter(_ > 0 && selectedTab != ScreenModel.chatTab)
    }

  private val myMovesHistoryProperty: ReadableSeqProperty[TurnPlay] =
    presenter.gameStateProperty
      .transformToSeq(_.map(_.me.turnPlayHistory).getOrElse(Seq.empty))
  private val myMovesHistorySizeProperty: ReadableProperty[Int] =
    myMovesHistoryProperty.transform(_.size)
  private val myMovesHistoryShowNotification: ReadableProperty[Option[Int]] =
    combine(
      myMovesHistorySizeProperty,
      screenModel.subProp(_.lastSeenMessagesMyMoves),
      screenModel.subProp(_.selectedTab)
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount).filter(_ > 0 && selectedTab != ScreenModel.myMovesTab)
    }

  private val enemyMovesHistoryProperty: ReadableSeqProperty[TurnPlay] =
    presenter.gameStateProperty
      .transformToSeq(_.map(_.enemy.turnPlayHistory).getOrElse(Seq.empty))
  private val enemyMovesHistorySizeProperty: ReadableProperty[Int] =
    enemyMovesHistoryProperty.transform(_.size)
  private val enemyMovesHistoryShowNotification: ReadableProperty[Option[Int]] =
    combine(
      enemyMovesHistorySizeProperty,
      screenModel.subProp(_.lastSeenMessagesEnemyMoves),
      screenModel.subProp(_.selectedTab)
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.enemyMovesTab)
    }

  combine(
    screenModel.subProp(_.selectedTab),
    chatMessagesSizeProperty,
    myMovesHistorySizeProperty,
    enemyMovesHistorySizeProperty
  ).listen { case (selectedTab, chatSize, myMovesSize, enemyMovesSize) =>
    if (selectedTab == ScreenModel.chatTab)
      screenModel.subProp(_.lastSeenMessagesChat).set(chatSize)
    else if (selectedTab == ScreenModel.myMovesTab)
      screenModel.subProp(_.lastSeenMessagesMyMoves).set(myMovesSize)
    else if (selectedTab == ScreenModel.enemyMovesTab)
      screenModel.subProp(_.lastSeenMessagesEnemyMoves).set(enemyMovesSize)
  }

  private val chatTabButton: UdashButton =
    UdashButton()(nested =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.chatTab) " active" else ""),
        data("bs-toggle") := "tab",
//        data("bs-target") := "#" + ScreenModel.chatTab,
        "Chat",
        nested(produce(chatMessagesShowNotification) {
          case None         => span.render
          case Some(number) => span(`class` := "badge badge-light ml-1", number).render
        })
      )
    )

  private val myMovesTabButton: UdashButton =
    UdashButton(
      disabled = presenter.inPreGameMode
    )(nested =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.myMovesTab) " active" else ""),
        data("bs-toggle") := "tab",
//        data("bs-target") := "#" + ScreenModel.myMovesTab,
        "My Moves",
        nested(produce(myMovesHistoryShowNotification) {
          case None         => span.render
          case Some(number) => span(`class` := "badge badge-light ml-1", number).render
        })
      )
    )

  private val enemyMovesTabButton: UdashButton =
    UdashButton(
      disabled = presenter.inPreGameMode
    )(nested =>
      Seq[Modifier](
        `class` := "nav-link btn-outline-primary" +
          (if (presenter.selectedTabProperty.get == ScreenModel.enemyMovesTab) " active" else ""),
        data("bs-toggle") := "tab",
//        data("bs-target") := "#" + ScreenModel.enemyMovesTab,
        "Enemy Moves",
        nested(produce(enemyMovesHistoryShowNotification) {
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

  private def messagesTab(nested: Binding.NestedInterceptor) = {
    val navTabs =
      nested(produce(screenModel.subProp(_.selectedTab)) { _ =>
        def makeNavItem(button: UdashButton): JsDom.TypedTag[LI] =
          li(
            `class` := "nav-item",
            role := "presentation",
            nested(button)
          )

        ul(
          `class` := "nav nav-tabs",
          `role` := "tablist",
          makeNavItem(chatTabButton),
          makeNavItem(myMovesTabButton),
          makeNavItem(enemyMovesTabButton)
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
        nested2(repeat(chatMessagesProperty) { msgProperty =>
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
      strong(turnPlay.turn.toTurnString, ": "),
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
          repeat(myMovesHistoryProperty)(turnPlayProperty => turnPlaysToHtml(turnPlayProperty.get))
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
          repeat(enemyMovesHistoryProperty)(turnPlayProperty =>
            turnPlaysToHtml(turnPlayProperty.get)
          )
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
    presenter.setSelectedTab(ScreenModel.chatTab)
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
          nested(produceWithNested(chatModel.subProp(_.username)) {
            case (Username(username), nested) =>
              div(
                `class` := "row justify-content-between",
                div(
                  `class` := "col-7",
                  if (username.nonEmpty)
                    span(s"Logged in as ", b(username)).render
                  else
                    span("").render,
                  nested(
                    produce(presenter.gameStateProperty.transform(_.map(_.enemy.username))) {
                      case Some(Username(enemyUsername)) if enemyUsername.nonEmpty =>
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
                    case Some(InGameMode(isMyTurn, turn, _, _, _)) =>
                      val turnStrBinding: Binding =
                        translatedDynamic(
                          if (isMyTurn)
                            Translations.Game.yourTurn
                          else
                            Translations.Game.enemyTurn
                        )(_.apply())

                      span(
                        "Turn ",
                        turn.toTurnString,
                        ": ",
                        span(color := "#FF0000", b(turnStrBinding))
                      ).render
                    case Some(GameOverMode(turn, youWon, _, _)) =>
                      val turnStrBinding: Binding =
                        translatedDynamic(
                          if (youWon)
                            Translations.Game.youWon
                          else
                            Translations.Game.enemyWon
                        )(_.apply())

                      span(
                        "Turn ",
                        turn.toTurnString,
                        ": ",
                        span(color := "#FF0000", b(turnStrBinding))
                      ).render
                    case _ =>
                      span.render
                  })
                ),
                nested(
                  produce(presenter.gameModeProperty) {
                    gameModeOpt =>
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

                      def showTime(
                          myTimeRemaining: TimeRemaining,
                          enemyTimeRemaining: TimeRemaining
                      ): Div = {
                        div(
                          `class` := "col-3 row",
                          div(
                            `class` := "col",
                            span("My Time:"),
                            br,
                            span(b(toTimeStr(myTimeRemaining.totalTimeRemainingMillis / 1000))),
                            span(
                              b(
                                toShortTimeStr(
                                  myTimeRemaining.turnTimeRemainingMillisOpt.map(_ / 1000)
                                )
                              )
                            )
                          ),
                          div(
                            `class` := "col",
                            span("Enemy Time:"),
                            br,
                            span(b(toTimeStr(enemyTimeRemaining.totalTimeRemainingMillis / 1000))),
                            span(
                              b(
                                toShortTimeStr(
                                  enemyTimeRemaining.turnTimeRemainingMillisOpt.map(_ / 1000)
                                )
                              )
                            )
                          )
                        ).render
                      }

                      gameModeOpt match {
                        case Some(
                              InGameMode(_, _, _, Some(myTimeRemaining), Some(enemyTimeRemaining))
                            ) =>
                          showTime(myTimeRemaining, enemyTimeRemaining)
                        case Some(
                              GameOverMode(_, _, Some(myTimeRemaining), Some(enemyTimeRemaining))
                            ) =>
                          showTime(myTimeRemaining, enemyTimeRemaining)
                        case _ =>
                          div.render
                      }
                  }
                ),
                div(
                  `class` := "col-2 container",
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

  window.setTimeout(
    () => presenter.onCanvasResize(canvasDiv),
    1
  )

}
