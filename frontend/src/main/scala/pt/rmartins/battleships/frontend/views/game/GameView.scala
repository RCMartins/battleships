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
import org.scalajs.dom.{MouseEvent, WheelEvent}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.shared.css.ChatStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode.PreGameMode
import pt.rmartins.battleships.shared.model.game.{GameMode, GameState}

class GameView(
    gameModel: ModelProperty[GameModel],
    chatModel: ModelProperty[ChatModel],
    presenter: GamePresenter,
    translationsService: TranslationsService
) extends View
    with CssView {

  import scalatags.JsDom.all._
  import translationsService._

  private val MainGameCanvasId = "mainGameCanvas"

  private val myBoardCanvas =
    canvas(
      id := MainGameCanvasId
//      border := "1px solid #000000"
    ).render

  private val boardView = new BoardView(gameModel, presenter, myBoardCanvas)

  private def gameCanvas(nested: Binding.NestedInterceptor) =
    div(
      nested(produce(gameModel) { gameModel =>
        boardView.paint(gameModel)
        myBoardCanvas
      })
    )

  myBoardCanvas.onmousemove = (mouseEvent: MouseEvent) => {
    val rect = myBoardCanvas.getBoundingClientRect()
    presenter.moveMouse(
      mouseEvent.clientX.toInt - rect.left.toInt,
      mouseEvent.clientY.toInt - rect.top.toInt
    )
  }

  myBoardCanvas.onmouseleave = (_: MouseEvent) => {
    presenter.moveLeave()
  }

  myBoardCanvas.onmousedown = (_: MouseEvent) => {
    presenter.moveClick(boardView)
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

  private val mainGameForm = UdashForm(
    componentId = ComponentId("main-game-from")
  )(factory =>
    factory.disabled(presenter.hasWriteAccess.toProperty.transform(!_))(nested =>
      nested(produce(gameModel.subProp(_.myGameState)) {
        case None =>
          UdashInputGroup()(UdashInputGroup.appendButton(mainStartButton)).render
        case Some(GameState(_, _, _, PreGameMode(_))) =>
          UdashInputGroup()(
            Seq(
              UdashInputGroup.appendButton(confirmShipsButton),
              UdashInputGroup.appendButton(undoButton),
              UdashInputGroup.appendButton(resetButton),
              UdashInputGroup.appendButton(randomPlacementButton)
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
    presenter.resetPlaceShips()
  }

  randomPlacementButton.listen { _ =>
    presenter.randomPlacement()
  }

  private def messagesWindow(nested: Binding.NestedInterceptor) =
    div(
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

  override def getTemplate: Modifier = div {
    UdashCard(componentId = ComponentId("game-panel"))(factory =>
      Seq(
        factory.header(nested =>
          nested(produceWithNested(chatModel.subProp(_.username)) { case (username, nested) =>
            div(
              if (username.nonEmpty)
                span(s"Logged in as ", b(username)).render
              else
                span("").render,
              br,
              nested(produce(gameModel.subProp(_.myGameState)) {
                case Some(GameState(_, _, _, GameMode.PreGameMode(_))) =>
                  val placeShipsBinding =
                    translatedDynamic(Translations.Game.placeShips)(_.apply())
                  span(color := "#FF0000", b(placeShipsBinding)).render
                case Some(
                      GameState(_, me, _, GameMode.InGameMode(firstPlayerUsername, halfTurns))
                    ) =>
                  val isMyTurn =
                    halfTurns % 2 == (if (firstPlayerUsername == me.username) 1 else 0)

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
            ).render
          })
        ),
        factory.body(gameCanvas),
        factory.body(nested => nested(mainGameForm)),
        factory.header(nested =>
          nested(produce(chatModel.subProp(_.connectionsCount)) { connectionsCount =>
            span(s"Chat: $connectionsCount").render
          })
        ),
        factory.body(messagesWindow),
        factory.footer(nested => nested(msgForm))
      )
    )
  }
}
