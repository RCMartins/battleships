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
import pt.rmartins.battleships.frontend.views.game.CanvasUtils.CanvasColor
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.css.ChatStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.chat.ChatMessage
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
    translationsService: TranslationsService
) extends View
    with CssView {

  import translationsService._

  private val myBoardCanvas: Canvas =
    canvas(
      id := "mainGameCanvas"
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

    window.setTimeout(
      () => boardView.paint(),
      1
    )
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

  private def usernameInput(factory: FormElementsFactory) =
    factory.input
      .formGroup(groupId = ComponentId("username")) { nested =>
        factory.input
          .textInput(preGameModel.subProp(_.username).bitransform(_.username)(Username(_)))(
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
      nested(produce(presenter.gameModeProperty.transform(_.map {
        case PreGameMode(iPlacedShips, _) => (iPlacedShips, false, false)
        case PlayingMode(_, _, _, _, _)   => (false, true, false)
        case GameOverMode(_, _, _, _)     => (false, false, true)
      })) {
        case None =>
          div(
            `class` := "row",
            div(`class` := "mx-2"),
            div(`class` := "mx-1", startGameVsBotButton),
            div(`class` := "ml-3", startGameVsPlayerButton),
            usernameInput(factory)
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
            div(`class` := "mx-1", rematchButton)
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
    presenter.startGameWith(preGameModel.subProp(_.username).get)
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

  rematchButton.listen { _ =>
    presenter.rematchGame()
  }

  private val chatMessagesProperty: ReadableSeqProperty[ChatMessage] =
    chatModel.subSeq(_.msgs)
  private val chatMessagesSizeProperty: ReadableProperty[Int] =
    chatModel.subSeq(_.msgs).transform(_.size)
  private val chatMessagesShowNotification: ReadableProperty[Option[Int]] =
    combine(
      chatMessagesSizeProperty,
      screenModel.subProp(_.lastSeenMessagesChat),
      presenter.selectedTabProperty
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
      presenter.selectedTabProperty
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
      presenter.selectedTabProperty
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.enemyMovesTab)
    }

  combine(
    presenter.selectedTabProperty,
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
        nested(translatedDynamic(Translations.Game.chatTab)(_.apply())),
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
        nested(translatedDynamic(Translations.Game.myMovesTab)(_.apply())),
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
        nested(translatedDynamic(Translations.Game.enemyMovesTab)(_.apply())),
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
      nested(produceWithNested(presenter.selectedTabProperty) { case (_, nested) =>
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
    nested(produceWithNested(presenter.selectedTabProperty) { case (selectedTab, nested) =>
      div(
        `class` := "tab-pane fade" + (if (selectedTab == ScreenModel.chatTab) " show active"
                                      else ""),
        id := ScreenModel.chatTab,
        role := "tabpanel",
        ChatStyles.messagesWindow,
        nested(repeat(chatMessagesProperty) { msgProperty =>
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

  private def turnPlaysToHtml(turnPlay: TurnPlay): Seq[dom.Element] = {
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

    def createShipCanvas(ship: Ship, destroyed: Boolean): Canvas = {
      val shipCanvas = canvas(`class` := "mr-3").render
      shipCanvas.setAttribute("width", canvasSize.x.toString)
      shipCanvas.setAttribute("height", canvasSize.y.toString)
      val renderingCtx = shipCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      val initialPosition = Coordinate(1, canvasSize.y / 2 - (ship.size.y * sqSize) / 2)
      ship.pieces.foreach { shipPiece =>
        CanvasUtils.drawBoardSquare(
          renderingCtx,
          initialPosition,
          shipPiece,
          sqSize,
          CanvasColor.Ship()
        )
        if (destroyed)
          CanvasUtils.drawCrosshair(
            renderingCtx,
            initialPosition,
            shipPiece,
            sqSize,
            lineWidth = 1.0,
            alpha = 0.8
          )
      }
      shipCanvas
    }

    def createWaterCanvas(): Canvas = {
      val shipCanvas = canvas(`class` := "mr-3").render
      shipCanvas.setAttribute("width", canvasSize.x.toString)
      shipCanvas.setAttribute("height", canvasSize.y.toString)
      val renderingCtx = shipCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      val initialPosition = Coordinate(1, canvasSize.y / 2 - (sqSize / 2))
      CanvasUtils.drawBoardSquare(
        renderingCtx,
        initialPosition,
        Coordinate.origin,
        sqSize,
        CanvasColor.Water()
      )
      shipCanvas
    }

    div(
      ChatStyles.turnContainer,
      div(
        minWidth := "40px",
        strong(`class` := "col-6 px-0", turnPlay.turn.toTurnString, ": ")
      ),
      span(
        turnPlay.hitHints
          .map {
            case HitHint.Water =>
              createWaterCanvas()
            case HitHint.ShipHit(shipId, destroyed) =>
              createShipCanvas(Ship.shipLongXMap(shipId), destroyed)
          }
      )
    ).render
  }

  private def myMovesTabItem(nested: Binding.NestedInterceptor): Binding =
    nested(produceWithNested(presenter.selectedTabProperty) { case (selectedTab, nested) =>
      div(
        `class` := "tab-pane fade" + (if (selectedTab == ScreenModel.myMovesTab) " show active"
                                      else ""),
        id := ScreenModel.myMovesTab,
        role := "tabpanel",
        ChatStyles.messagesWindow,
        nested(
          repeat(myMovesHistoryProperty)(turnPlayProperty => turnPlaysToHtml(turnPlayProperty.get))
        )
      ).render
    })

  private def enemyMovesTabItem(nested: Binding.NestedInterceptor): Binding =
    nested(produceWithNested(presenter.selectedTabProperty) { case (selectedTab, nested) =>
      div(
        `class` := "tab-pane fade" + (if (selectedTab == ScreenModel.enemyMovesTab) " show active"
                                      else ""),
        id := ScreenModel.enemyMovesTab,
        role := "tabpanel",
        ChatStyles.messagesWindow,
        nested(
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
      FontAwesome.Solid.signOutAlt
    )
  )

  quitGameButton.listen { _ =>
    presenter.gameStateProperty.get match {
      case None =>
        presenter.logout()
      case Some(_) =>
        presenter.quitCurrentGame()
    }
  }

  override def getTemplate: Modifier = div {
    UdashCard(componentId = ComponentId("game-panel"))(factory =>
      Seq(
        factory.header(nested =>
          div(
            `class` := "row justify-content-between",
            div(
              `class` := "col-7",
              span(
                nested(translatedDynamic(Translations.Game.loggedInAs)(_.apply())),
                " ",
                b(bind(chatModel.subProp(_.username)))
              ),
              nested(
                produceWithNested(presenter.gameStateProperty.transform(_.map(_.enemy.username))) {
                  case (Some(Username(enemyUsername)), nested) =>
                    span(
                      " - ",
                      nested(translatedDynamic(Translations.Game.playingAgainst)(_.apply())),
                      " ",
                      b(enemyUsername)
                    ).render
                  case _ =>
                    span.render
                }
              ),
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
                case (Some(GameOverMode(turn, youWon, _, _)), nested) =>
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
                  produceWithNested(combine(presenter.playingMode, presenter.inGameOverMode)) {
                    case ((true, _) | (_, true), nested) =>
                      span(nested(translatedDynamic(key)(_.apply())), ":").render
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
                  showTimeStr(nested, Translations.Game.myTime),
                  br,
                  nested(produce(gameModel.subProp(_.timeRemaining).transform(_.map(_._1))) {
                    case Some(myTimeRemaining) =>
                      showTime(myTimeRemaining)
                    case _ =>
                      div.render
                  })
                ),
                div(
                  `class` := "col px-0",
                  showTimeStr(nested, Translations.Game.enemyTime),
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
