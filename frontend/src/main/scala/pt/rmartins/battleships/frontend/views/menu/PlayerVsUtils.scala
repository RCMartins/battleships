package pt.rmartins.battleships.frontend.views.menu

import io.udash._
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n._
import io.udash.properties.single.Property
import org.scalajs.dom.html.Div
import org.scalajs.dom.{UIEvent, window}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game._
import pt.rmartins.battleships.frontend.views.model.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.frontend.views.model.ModeType._
import pt.rmartins.battleships.shared.css.GameStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode._
import scalatags.JsDom.all._

class PlayerVsUtils(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    presenter: GamePresenter,
    boardView: BoardView,
    preGameView: PreGameView,
    gameModals: GameModals,
    chatUtils: ChatUtils,
    translationsService: TranslationsService,
) extends CssView {

  import translationsService._

  val startGameVsBotButton: UdashButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("start-game-bot-button")
    )(nested =>
      Seq[Modifier](span(nested(translatedDynamic(Translations.Game.startGameVsBot)(_.apply()))))
    )

  startGameVsBotButton.listen { _ =>
    presenter.startGameWithBots()
  }

  val confirmRulesButton: UdashButton = {
    val isConfirmedProperty: ReadableProperty[Boolean] =
      preGameModel.subProp(_.inJoinedPreGame).transform {
        case Some(PlayingAgainstPlayer(_, true, _, _)) => true
        case _                                         => false
      }

    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = Property(true),
      componentId = ComponentId("confirm-rules-button"),
      disabled = isConfirmedProperty
    )(nested =>
      Seq[Modifier](
        span(nested(translatedDynamic(Translations.Game.confirmRulesButton)(_.apply())))
      )
    )
  }

  confirmRulesButton.listen { _ =>
    presenter.confirmRules()
  }

  private val confirmShipsButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("confirm-button"),
      disabled = gameModel.subProp(_.shipsLeftToPlace).transform(_.nonEmpty)
    )(nested => Seq(nested(translatedDynamic(Translations.Game.confirmButton)(_.apply()))))

  confirmShipsButton.listen { _ =>
    presenter.confirmShipPlacement()
  }

  private val undoButton =
    UdashButton(
      buttonStyle = Color.Secondary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("undo-button"),
      disabled = presenter.gameStateProperty.transform(!_.exists(_.me.myBoard.ships.nonEmpty))
    )(nested => Seq(nested(translatedDynamic(Translations.Game.undoButton)(_.apply()))))

  undoButton.listen { _ =>
    presenter.undoLastPlacedShip()
  }

  private val resetButton =
    UdashButton(
      buttonStyle = Color.Danger.toProperty,
      block = true.toProperty,
      componentId = ComponentId("reset-button"),
      disabled = presenter.gameStateProperty.transform(!_.exists(_.me.myBoard.ships.nonEmpty))
    )(nested => Seq(nested(translatedDynamic(Translations.Game.resetButton)(_.apply()))))

  resetButton.listen { _ =>
    presenter.resetPlacedShips()
  }

  private val randomPlacementButton =
    UdashButton(
      buttonStyle = Color.Secondary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("random-button"),
      disabled = gameModel.subProp(_.shipsLeftToPlace).transform(_.isEmpty)
    )(nested => Seq(nested(translatedDynamic(Translations.Game.randomButton)(_.apply()))))

  randomPlacementButton.listen { _ =>
    presenter.randomPlacement()
  }

  private val editRulesButton =
    UdashButton(
      buttonStyle = Color.Secondary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("edit-rules-button")
    )(nested => Seq(nested(translatedDynamic(Translations.Game.editRulesButton)(_.apply()))))

  editRulesButton.listen { _ =>
    presenter.requestEditRules()
  }

  def mainDiv(nested: NestedInterceptor): Div = {
    div(
      nested(produceWithNested(presenter.modeTypeProperty) {
        case (Some(PlacingGameModeType), nested) =>
          placingShipsDiv(nested)
        case (Some(PlayingModeType), nested) =>
          gameDiv(nested)
        case (Some(GameOverModeType), nested) =>
          div.render
        case _ =>
          div.render
      })
    ).render
  }

  private def placingShipsDiv(nested: NestedInterceptor): Div = {
    val gameDiv: Div =
      div(
        `class` := "d-flex justify-content-center",
        GameStyles.mainCardHeight,
        boardView.drawMyBoardDiv(nested),
        preGameView.createFleetPreview(nested)
      ).render

    window.onresize = (_: UIEvent) => {
      presenter.onCanvasResize(gameDiv, boardView.myBoardCanvas)
    }

    var handle: Int = 0
    handle = window.setInterval(
      () => {
        if (gameDiv.clientWidth != 0) {
          presenter.onCanvasResize(gameDiv, boardView.myBoardCanvas)
          window.clearTimeout(handle)
        }
      },
      timeout = 20
    )

    div(
      `class` := "card",
      div(
        `class` := "card-body p-0",
        div(
          gameDiv
        )
      ),
      placingShipsFooter(nested)
    ).render
  }

  private def placingShipsFooter(nested: NestedInterceptor): Div = {
    div(
      `class` := "card-footer",
      nested(
        produce(presenter.placingShipsModeProperty) {
          case Some(PlacingShipsMode(false, _)) =>
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
          case Some(PlacingShipsMode(true, _)) =>
            div(
              `class` := "row mx-0",
              div(`class` := "mx-2", undoButton),
              div(`class` := "mx-2", resetButton)
            ).render
          case _ =>
            div.render
        }
      )
    ).render
  }

  private def gameDiv(nested: NestedInterceptor): Div = {
    val gameDiv: Div =
      div(
        `class` := "d-flex justify-content-center",
        GameStyles.mainCardHeight,
        preGameView.createFleetPreview(nested),
        boardView.drawMyBoardDiv(nested),
        chatUtils.chatAndMovesDiv(nested)
      ).render

    window.onresize = (_: UIEvent) => {
      presenter.onCanvasResize(gameDiv, boardView.myBoardCanvas)
    }

    var handle: Int = 0
    handle = window.setInterval(
      () => {
        if (gameDiv.clientWidth != 0) {
          presenter.onCanvasResize(gameDiv, boardView.myBoardCanvas)
          window.clearTimeout(handle)
        }
      },
      timeout = 20
    )

    div(
      `class` := "card",
      div(
        `class` := "card-body p-0",
        div(
          gameDiv
        )
      ),
      placingShipsFooter(nested)
    ).render
  }

  private def gameFooter(nested: NestedInterceptor): Div = {
    div(
      `class` := "card-footer",
      nested(
        produce(presenter.placingShipsModeProperty) {
          case Some(PlacingShipsMode(false, _)) =>
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
          case Some(PlacingShipsMode(true, _)) =>
            div(
              `class` := "row mx-0",
              div(`class` := "mx-2", undoButton),
              div(`class` := "mx-2", resetButton)
            ).render
          case _ =>
            div.render
        }
      )
    ).render
  }

}
