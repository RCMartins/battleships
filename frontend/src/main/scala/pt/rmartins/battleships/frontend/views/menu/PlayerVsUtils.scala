package pt.rmartins.battleships.frontend.views.menu

import io.udash._
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n._
import io.udash.properties.single.Property
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.{UIEvent, html, window}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.CanvasUtils.CanvasImage
import pt.rmartins.battleships.frontend.views.game.Utils._
import pt.rmartins.battleships.frontend.views.game._
import pt.rmartins.battleships.frontend.views.model.AttacksQueuedStatus
import pt.rmartins.battleships.frontend.views.model.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.frontend.views.model.ModeType._
import pt.rmartins.battleships.shared.css.GameStyles._
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.GameMode._
import pt.rmartins.battleships.shared.model.game._
import scalatags.JsDom
import scalatags.JsDom.all.{span, _}

import scala.util.chaining.scalaUtilChainingOps

class PlayerVsUtils(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    chatModel: ModelProperty[ChatModel],
    presenter: GamePresenter,
    boardView: BoardView,
    preGameView: PreGameView,
    gameModals: GameModals,
    chatUtils: ChatUtils,
    translationsService: TranslationsService,
) extends CssView {

  import translationsService._

  private def getDestroyedShips(history: List[TurnPlay]): Int =
    history.map(_.hitHints.count(_.isDestroyed)).sum

  val myProgressProperty: ReadableProperty[Option[(Int, Int)]] =
    combine(presenter.fleetProperty, presenter.meProperty.transform(_.map(_.turnPlayHistory)))
      .transform {
        case (Some(gameFleet), Some(turnPlayHistory)) =>
          Some((getDestroyedShips(turnPlayHistory), gameFleet.shipsAmount))
        case _ =>
          None
      }

  val enemyProgressProperty: ReadableProperty[Option[(Int, Int)]] =
    combine(presenter.fleetProperty, presenter.enemyProperty.transform(_.map(_.turnPlayHistory)))
      .transform {
        case (Some(gameFleet), Some(turnPlayHistory)) =>
          Some((getDestroyedShips(turnPlayHistory), gameFleet.shipsAmount))
        case _ =>
          None
      }

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

  private val cancelQueuedAttacksButton: html.Element =
    UdashButton(
      buttonStyle = Color.Danger.toProperty,
      block = true.toProperty,
      componentId = ComponentId("cancel-queued-attacks-button")
    )(_ => Seq[Modifier](span(FontAwesome.Solid.times).render)).render

  gameModel
    .subProp(_.turnAttacksQueuedStatus)
    .listen(
      {
        case AttacksQueuedStatus.Queued =>
          cancelQueuedAttacksButton.classList.remove("invisible")
          cancelQueuedAttacksButton.classList.add("visible")
        case _ =>
          cancelQueuedAttacksButton.classList.remove("visible")
          cancelQueuedAttacksButton.classList.add("invisible")
      },
      initUpdate = true
    )

  cancelQueuedAttacksButton.onclick = _ => {
    presenter.cancelQueuedAttacks()
  }

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

  launchAttackButton.listen { _ =>
    presenter.launchAttack()
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

  hideMyBoardButton.listen { _ =>
    screenModel.subProp(_.hideMyBoard).set(!screenModel.get.hideMyBoard)
  }

  private val rematchButton =
    UdashButton(
      buttonStyle = Color.Primary.toProperty,
      block = true.toProperty,
      componentId = ComponentId("rematch-button")
    )(nested => Seq(nested(translatedDynamic(Translations.Game.rematchButton)(_.apply()))))

  rematchButton.listen { _ =>
    presenter.rematchGame()
  }

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

  revealEnemyBoardButton.listen { _ =>
    screenModel.subProp(_.revealEnemyBoard).set(!screenModel.get.revealEnemyBoard)
  }

  def createaMainDiv(nested: NestedInterceptor): Div =
    div(
      nested(produceWithNested(presenter.modeTypeProperty) {
        case (Some(PlacingGameModeType), nested) =>
          placingShipsDiv(nested)
        case (Some(PlayingModeType), nested) =>
          gameDiv(nested)
        case (Some(GameOverModeType), nested) =>
          gameOverDiv(nested)
        case _ =>
          div.render
      })
    ).render

  private def placingShipsDiv(nested: NestedInterceptor): Div = {
    val mainDiv: Div =
      div(
        `class` := "d-flex justify-content-center",
        mainCardHeight,
        boardView.drawMainBoardDiv(nested),
        boardView.createFleetPlacePreview(nested)
      ).render

    window.onresize = (_: UIEvent) => {
      presenter.onCanvasResize(mainDiv, boardView.mainBoardCanvas)
    }

    var handle: Int = 0
    handle = window.setInterval(
      () => {
        if (mainDiv.clientWidth != 0) {
          presenter.onCanvasResize(mainDiv, boardView.mainBoardCanvas)
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
          mainDiv
        )
      ),
      placingShipsFooter(nested)
    ).render
  }

  private def placingShipsFooter(nested: NestedInterceptor): Div =
    div(
      `class` := "card-footer",
      nested(
        produce(presenter.placingShipsModeProperty) {
          case Some(PlacingShipsMode(false, _)) =>
            div(
              `class` := "d-flex justify-content-around mx-0",
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
              `class` := "d-flex justify-content-center mx-0",
              div(`class` := "mx-2", undoButton),
              div(`class` := "mx-2", resetButton)
            ).render
          case _ =>
            div.render
        }
      )
    ).render

  private def gameDiv(nested: NestedInterceptor): Div = {
    val mainDiv: Div =
      div(
        `class` := "d-flex justify-content-center row m-0",
        mainCardHeight,
        div(
          `class` := "col-2 px-1",
          div(
            `class` := "row m-0",
            div(
              `class` := "col-12 pr-0",
              currentTurnDiv(nested)
            ),
            div(
              `class` := "col-12 pr-0",
              boardView.createFleetPreview(nested)
            )
          )
        ),
        div(
          `class` := "col-5 px-1",
          boardView.drawMainBoardDiv(nested),
        ),
        div(
          `class` := "col-3 px-1",
          chatUtils.chatAndMovesDiv(nested),
        ),
        div(
          `class` := "col-2 px-1",
          div(
            `class` := "row m-0",
            div(
              `class` := "col-12",
              gameTimeDiv(nested)
            ),
            div(
              `class` := "col-12",
              boardView.drawSmallBoardDiv(nested)
            ),
          ),
        ),
      ).render

    window.onresize = (_: UIEvent) => {
      presenter.onCanvasResize(mainDiv, boardView.mainBoardCanvas)
    }

    var handle: Int = 0
    handle = window.setInterval(
      () => {
        if (mainDiv.clientWidth != 0) {
          presenter.onCanvasResize(mainDiv, boardView.mainBoardCanvas)
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
          mainDiv
        )
      ),
      gameFooter(nested)
    ).render
  }

  private def gameFooter(nested: NestedInterceptor): Div =
    div(
      `class` := "card-footer",
      nested(
        produce(presenter.gameModeProperty) {
          case Some(PlayingMode(_, _, _, _, _)) =>
            div(
              `class` := "d-flex justify-content-around mx-0",
              div(
                `class` := "d-flex mx-0",
                div(`class` := "ml-2", cancelQueuedAttacksButton),
                div(`class` := "mx-1", launchAttackButton)
              ),
              div(`class` := "mx-2", hideMyBoardButton)
            ).render
          case _ =>
            div.render
        }
      )
    ).render

  private def gameOverDiv(nested: NestedInterceptor): Div = {
    val mainDiv: Div =
      div(
        `class` := "d-flex justify-content-center",
        mainCardHeight,
        boardView.drawMainBoardDiv(nested),
        chatUtils.chatAndMovesDiv(nested),
        div(
          `class` := "row",
          div(
            `class` := "col-12",
            gameTimeDiv(nested)
          ),
          div(
            `class` := "col-12",
            boardView.drawSmallBoardDiv(nested)
          ),
        ),
      ).render

    window.onresize = (_: UIEvent) => {
      presenter.onCanvasResize(mainDiv, boardView.mainBoardCanvas)
    }

    var handle: Int = 0
    handle = window.setInterval(
      () => {
        if (mainDiv.clientWidth != 0) {
          presenter.onCanvasResize(mainDiv, boardView.mainBoardCanvas)
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
          mainDiv
        )
      ),
      gameOverFooter(nested)
    ).render
  }

  private def gameOverFooter(nested: NestedInterceptor): Div =
    div(
      `class` := "card-footer",
      nested(
        produce(presenter.gameModeProperty) {
          case Some(GameOverMode(_, _, _, _, _)) =>
            div(
              `class` := "row justify-content-between mx-0",
              div(`class` := "mx-2", rematchButton),
              div(`class` := "mx-2", revealEnemyBoardButton)
            ).render
          case _ =>
            div.render
        }
      )
    ).render

  private def currentTurnDiv(nested: NestedInterceptor): Div =
    div(
      nested(
        produceWithNested(presenter.gameModeProperty) {
          case (
                Some(PlayingMode(isMyTurn, Turn(currentTurn, extraTurn), turnAttackTypes, _, _)),
                nested
              ) =>
            val isMyTurnDiv: JsDom.TypedTag[Div] =
              if (isMyTurn)
                div(
                  `class` := "m-0",
                  growAndShrink,
                  b(nested(translatedDynamic(Translations.Game.yourTurn)(_.apply()))),
                )
              else
                div(
                  `class` := "m-0",
                  turnTextSize,
                  b(nested(translatedDynamic(Translations.Game.enemyTurn)(_.apply()))),
                )

            val turnNumberDiv =
              div(
                `class` := "d-flex align-items-center",
                span(
                  `class` := "my-2 mr-2",
                  FontAwesome.Modifiers.Sizing.x2,
                  FontAwesome.Solid.infoCircle
                ),
                div(
                  h3(
                    `class` := "m-0",
                    span(nested(translatedDynamic(Translations.Game.turn)(_.apply()))),
                    span(nbsp),
                    span(currentTurn),
                    span(":"),
                  ),
                  isMyTurnDiv
                )
              )

            val extraTurnDiv: JsDom.TypedTag[Div] =
              extraTurn match {
                case Some(_) =>
                  div(b(nested(translatedDynamic(Translations.Game.extraTurn)(_.apply()))))
                case None =>
                  div()
              }

            val turnAttacksDiv: JsDom.TypedTag[Div] =
              createAttackTypesDiv(nested, turnAttackTypes)

            div(
              `class` := "row m-1",
              div(`class` := "col-12 p-0 mb-2", turnNumberDiv),
              div(`class` := "col-12 p-0", extraTurnDiv),
              div(`class` := "col-12 p-0", turnAttacksDiv)
            ).render
          case _ =>
            div.render
        }
      )
    ).render

  private def createAttackTypesDiv(
      nested: NestedInterceptor,
      attackTypes: List[AttackType]
  ): JsDom.TypedTag[Div] = {
    val imageSize = Coordinate.square(50)

    def createAttackTypeDiv(attackType: AttackType, amount: Int): JsDom.TypedTag[Div] =
      div(
        `class` := "mr-4 d-flex align-items-center",
        (1 to amount).map { _ =>
          CanvasUtils
            .createCanvasImage(CanvasImage.fromAttackType(attackType), imageSize)
            .tap { canvas =>
              canvas.classList.add("border")
              canvas.classList.add("border-dark")
            }
        }
      )

    div(
      `class` := "d-flex align-items-center",
      attackTypes.map { attackType => createAttackTypeDiv(attackType, 1) }
    )
  }

  private def gameTimeDiv(nested: NestedInterceptor): Div =
    div(
      `class` := "row m-1",
      div(
        `class` := "col-12 my-3",
        playerStatsSummaryDiv(
          nested,
          chatModel.subProp(_.username).transform(Some(_)),
          Translations.Game.myProgress,
          myProgressProperty,
          gameModel.subProp(_.timeRemaining).transform(_.map(_._1)),
        )
      ),
      div(
        `class` := "col-12 my-3",
        playerStatsSummaryDiv(
          nested,
          presenter.enemyUsernameProperty,
          Translations.Game.enemyProgress,
          enemyProgressProperty,
          gameModel.subProp(_.timeRemaining).transform(_.map(_._2)),
        )
      ),
    ).render

  private def playerStatsSummaryDiv(
      nested: NestedInterceptor,
      playerUsername: ReadableProperty[Option[Username]],
      playerProgressKey0: TranslationKey0,
      playerProgressProperty: ReadableProperty[Option[(Int, Int)]],
      timeRemainingProperty: ReadableProperty[Option[TimeRemaining]]
  ): Div =
    div(
      `class` := "row m-0",
      div(
        `class` := "col-12 d-flex justify-content-center my-1",
        nested(produce(playerUsername) {
          case Some(Username(username)) => span(b(username)).render
          case None                     => span.render
        })
      ),
      div(
        `class` := "col-12 border rounded border-dark p-2 my-1 d-flex justify-content-center",
        div(
          nested(translatedDynamic(playerProgressKey0)(_.apply())),
          nested(
            produce(playerProgressProperty) {
              case Some((shipsDestoyed, shipsAmount)) =>
                div(
                  `class` := "row m-1",
                  div(
                    `class` := "col-12 d-flex justify-content-center",
                    span(shipsDestoyed, " / ", b(shipsAmount))
                  )
                ).render
              case _ =>
                div.render
            }
          )
        )
      ),
      div(
        `class` := "col-12 border rounded border-dark p-2 my-1 d-flex justify-content-center",
        div(
          nested(translatedDynamic(Translations.Game.remainingTime)(_.apply())),
          nested(
            produceWithNested(timeRemainingProperty) {
              case (Some(timeRemaining), nested) =>
                def toTimeStr(seconds: Int): JsDom.TypedTag[Span] =
                  span("%02d:%02d".format(seconds / 60, seconds % 60))

                def toShortTimeStr(secondsOpt: Option[Int]): JsDom.TypedTag[Span] =
                  secondsOpt
                    .map {
                      case 0 =>
                        span(" + ", b(redTextStyle, "00"))
                      case seconds if seconds >= 60 =>
                        span(" + %02d:%02d".format(seconds / 60, seconds % 60))
                      case seconds =>
                        span(" + %02d".format(seconds))
                    }
                    .getOrElse(span())

                val textSpan: Span =
                  span(
                    toTimeStr(timeRemaining.totalTimeRemainingMillis / 1000),
                    toShortTimeStr(timeRemaining.turnTimeRemainingMillisOpt.map(_ / 1000))
                  ).render

                div(
                  `class` := "row m-1",
                  div(
                    `class` := "col-12 d-flex justify-content-center",
                    textSpan
                  )
                ).render
              case _ =>
                div.render
            }
          )
        )
      )
    ).render

}
