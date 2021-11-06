package pt.rmartins.battleships.frontend.views.game

import io.udash.bindings.modifiers.Binding
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.css._
import io.udash.i18n.translatedDynamic
import io.udash.{ModelProperty, bind, produce, toAttrPairOps}
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div, Select}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.CanvasUtils._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.{AttackType, Coordinate, RuleTimeLimit, Ship}
import scalatags.JsDom.all._

class PreGameView(
    preGameModel: ModelProperty[PreGameModel],
    screenModel: ModelProperty[ScreenModel],
    gamePresenter: GamePresenter,
    canvasUtils: CanvasUtils,
    viewUtils: ViewUtils,
    translationsService: TranslationsService
) extends CssView {

  import translationsService._

  private val sqSize = 14
  private val defaultHeight = 332
  private val fleetMaxSize: Coordinate = Ship.allShipsFleetMaxX.size
  private val canvasSize: Coordinate = fleetMaxSize * sqSize + Coordinate.square(4)

  def createComponents(divElement: Element, nested: NestedInterceptor): Div = {
    div(
      `class` := "d-flex align-items-start",
      div(
        `class` := "nav flex-column nav-pills me-3",
        role := "tablist",
        button(
          `class` := "nav-link active btn",
          attr("data-bs-toggle") := "pill",
          attr("data-bs-target") := "#nav-pregame-fleet",
          `type` := "button",
          role := "tab",
          nested(translatedDynamic(Translations.Game.fleet)(_.apply()))
        ),
        button(
          `class` := "nav-link btn",
          attr("data-bs-toggle") := "pill",
          attr("data-bs-target") := "#nav-pregame-options",
          `type` := "button",
          role := "tab",
          nested(translatedDynamic(Translations.Game.options)(_.apply()))
        )
      ),
      div(
        `class` := "tab-content",
        div(
          `class` := "tab-pane fade show active",
          id := "nav-pregame-fleet",
          role := "tabpanel",
          div(
            `class` := "row mx-0 my-2",
            height := s"${defaultHeight}px",
            div(
              `class` := "col-6 px-0",
              div(
                `class` := "d-flex flex-wrap",
                createAllShipElems
              )
            ),
            div(
              `class` := "col-6",
              createCanvasPreview(divElement, nested)
            )
          )
        ),
        div(
          `class` := "tab-pane fade",
          id := "nav-pregame-options",
          role := "tabpanel",
          div(
            `class` := "row my-2",
            height := s"${defaultHeight}px",
            createTimeLimitOptions(nested),
            createCustomShots(nested)
          )
        )
      )
    ).render
  }

  private def createAllShipElems: Modifier = {
    def createShipCanvas(ship: Ship): Canvas =
      viewUtils.createShipCanvas(canvasSize, sqSize, ship, destroyed = false)

    Ship.allShipsFleetMaxX.ships.map { ship =>
      val shipCountProperty =
        gamePresenter.getPreGameShipProperty(ship.shipId)

      val plusButton =
        button(
          `class` := "btn btn-primary p-1",
          span(style := "width: 1rem; height: 1rem;", FontAwesome.Solid.plus)
        ).render
      plusButton.onclick = _ => shipCountProperty.set(shipCountProperty.get + 1)

      val minusButton =
        button(
          `class` := "btn btn-primary p-1",
          span(style := "width: 1rem; height: 1rem;", FontAwesome.Solid.minus),
          disabled.attrIf(shipCountProperty.transform(_ == 0))
        ).render
      minusButton.onclick = _ => shipCountProperty.set(Math.max(0, shipCountProperty.get - 1))

      div(
        `class` := "m-1 row",
        div(
          `class` := "ml-2 mr-1 py-2 g-3 btn-group rounded-start",
          span(
            `class` := "rounded-start bg-primary text-white py-1 px-2",
            style := "user-select: none",
            bind(shipCountProperty)
          ),
          plusButton,
          minusButton
        ),
        createShipCanvas(ship)
      )
    }
  }

  def createCanvasPreview(divElement: Element, nested: NestedInterceptor): Binding =
    nested(
      produce(
        combine(
          preGameModel.subProp(_.boardSize),
          preGameModel.subProp(_.previewBoardOpt),
          screenModel.subProp(_.previewBoardTitle),
          screenModel.subProp(_.screenResized)
        )
      ) {
        case (boardSize, previewBoardOpt, previewBoardTitle, _) =>
          val usableWidth = Math.max(50, Math.min(290, divElement.clientWidth / 2 - 130))

          val boardPos = Coordinate(0, 20)
          val maxWidthPixels = usableWidth
          val sqSize = maxWidthPixels / boardSize.max
          val canvasSize = Coordinate.square(maxWidthPixels) + boardPos
          val previewCanvas: Canvas = canvas.render
          previewCanvas.setAttribute("width", canvasSize.x.toString)
          previewCanvas.setAttribute("height", canvasSize.y.toString)
          val renderingCtx = previewCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

          val backgroundColor =
            if (previewBoardOpt.isEmpty)
              Some(CanvasColor.Red())
            else
              Some(CanvasColor.Water())

          val boardSizeText: String =
            "%02dx%02d".format(boardSize.x, boardSize.y)

          val boardTitle: String =
            previewBoardTitle.innerText + " - " + boardSizeText

          canvasUtils.drawBoardLimits(
            renderingCtx,
            boardTitle,
            boardSize,
            boardPos,
            sqSize,
            backgroundColor = backgroundColor,
            None
          )

          previewBoardOpt.foreach { case (board, _) =>
            board.ships.foreach { ship =>
              ship.shipActualPieces.foreach { shipPiece =>
                canvasUtils.drawBoardSquare(
                  renderingCtx,
                  boardPos,
                  shipPiece,
                  sqSize,
                  CanvasColor.Ship()
                )
              }
            }
          }

          val percDouble: Double =
            previewBoardOpt
              .map(_._2)
              .map(n => n.toDouble / PreGameModel.MaxPreviewTries)
              .getOrElse(1.0)

          val perc: Int =
            Math.ceil(percDouble * 100).toInt

          val color =
            if (percDouble < PreGameModel.MinPreviewTriesPerc) "bg-danger"
            else if (percDouble < PreGameModel.WarningPreviewTriesPerc) "bg-warning"
            else "bg-success"

          val progressBars: Node =
            div(
              `class` := s"progress-bar progress-bar-striped $color",
              role := "progressbar",
              style := s"width: $perc%",
              s"$perc%"
            ).render

          val maxBoardSize = 20
          val minBoardSize = 6
          val plusButton =
            button(
              `class` := "btn btn-primary p-2 mx-1",
              span(style := "width: 1rem; height: 1rem;", FontAwesome.Solid.plus)
            ).render
          if (boardSize.x == maxBoardSize)
            plusButton.disabled = true
          plusButton.onclick = _ =>
            preGameModel
              .subProp(_.boardSize)
              .set(
                Coordinate.square(Math.min(maxBoardSize, boardSize.x + 1))
              )

          val minusButton =
            button(
              `class` := "btn btn-primary p-2 mx-1",
              span(style := "width: 1rem; height: 1rem;", FontAwesome.Solid.minus)
            ).render
          if (boardSize.x == minBoardSize)
            minusButton.disabled = true
          minusButton.onclick = _ =>
            preGameModel
              .subProp(_.boardSize)
              .set(
                Coordinate.square(Math.max(minBoardSize, boardSize.x - 1))
              )

          Seq[Node](
            div(
              `class` := "row",
              minusButton,
              div(
                maxWidth := maxWidthPixels,
                previewCanvas,
                div(
                  `class` := "progress",
                  progressBars
                ).render
              ),
              plusButton
            ).render
          )
        case _ =>
          Seq.empty
      }
    )

  def createTimeLimitOptions(nested: NestedInterceptor): Modifier = {
    def selectOptionToSeconds(str: String): Int = {
      val s"$minutes:$seconds" = str
      minutes.toInt * 60 + seconds.toInt
    }

    def selectSecondsToString(seconds: Int): String =
      s"%02d:%02d".format(seconds / 60, seconds % 60)

    val totalTimeLimitCheckBox =
      input(
        `class` := "form-control px-1",
        width := "32px",
        `type` := "checkbox",
        checked
      ).render

    val totalTimeLimitOptions: List[Int] =
      (preGameModel.get.timeLimit.map(_.initialTotalTimeSeconds).toList ++
        List(1, 3, 5, 10, 15, 20, 30, 60).map(_ * 60)).distinct.sorted
    val totalTimeLimit: Select =
      select(
        `class` := "px-2",
        id := "time-limit-input"
      ).render
    totalTimeLimitOptions.foreach(seconds =>
      totalTimeLimit.appendChild(option(selectSecondsToString(seconds)).render)
    )
    preGameModel.get.timeLimit
      .map(_.initialTotalTimeSeconds)
      .foreach(seconds => totalTimeLimit.value = selectSecondsToString(seconds))

    val turnTimeLimitCheckBox =
      input(
        `class` := "form-control px-1",
        width := "32px",
        `type` := "checkbox",
        checked
      ).render

    val turnTimeLimitOptions: List[Int] =
      (preGameModel.get.timeLimit.flatMap(_.additionalTurnTimeSeconds.map(_._1)).toList ++
        List(1, 3, 5, 10, 15, 20, 30, 60)).distinct.sorted
    val turnTimeLimit: Select =
      select(
        `class` := "form-select px-2",
        id := "turn-time-limit-input"
      ).render
    turnTimeLimitOptions.foreach(seconds =>
      turnTimeLimit.appendChild(option(selectSecondsToString(seconds)).render)
    )
    preGameModel.get.timeLimit
      .flatMap(_.additionalTurnTimeSeconds.map(_._1))
      .foreach(seconds => turnTimeLimit.value = selectSecondsToString(seconds))

    def generateRuleTimeLimitOpt: Option[RuleTimeLimit] =
      if (totalTimeLimitCheckBox.checked)
        Some(
          RuleTimeLimit(
            selectOptionToSeconds(totalTimeLimit.value),
            if (turnTimeLimitCheckBox.checked)
              Some((selectOptionToSeconds(turnTimeLimit.value), false))
            else
              None
          )
        )
      else
        None

    def updateRuleTimeLimit(): Unit =
      preGameModel
        .subProp(_.timeLimit)
        .set(generateRuleTimeLimitOpt)

    totalTimeLimitCheckBox.onchange = _ => {
      totalTimeLimit.disabled = !totalTimeLimitCheckBox.checked
      turnTimeLimitCheckBox.disabled = !totalTimeLimitCheckBox.checked
      turnTimeLimit.disabled = !totalTimeLimitCheckBox.checked || !turnTimeLimitCheckBox.checked
      updateRuleTimeLimit()
    }

    totalTimeLimit.onchange = _ => {
      updateRuleTimeLimit()
    }

    turnTimeLimitCheckBox.onchange = _ => {
      turnTimeLimit.disabled = !turnTimeLimitCheckBox.checked
      updateRuleTimeLimit()
    }

    turnTimeLimit.onchange = _ => {
      updateRuleTimeLimit()
    }

    div(
      `class` := "col-6",
      div(
        `class` := "d-flex flex-wrap",
        div(
          `class` := "btn-group m-3",
          totalTimeLimitCheckBox,
          label(
            `class` := "input-group-text",
            `for` := totalTimeLimit.id,
            nested(translatedDynamic(Translations.Game.timeLimit)(_.apply()))
          ),
          totalTimeLimit
        ),
        br,
        div(
          `class` := "btn-group m-3",
          turnTimeLimitCheckBox,
          label(
            `class` := "input-group-text",
            `for` := turnTimeLimit.id,
            nested(translatedDynamic(Translations.Game.turnTimeLimit)(_.apply()))
          ),
          turnTimeLimit
        )
      )
    )
  }

  def createCustomShots(nested: NestedInterceptor): Modifier = {
    val missilesSize = Coordinate(50, 50)
    val missilesDistance = 30
    val MaxMissiles = 5

    val customShotsCanvas = canvas.render
    customShotsCanvas.setAttribute(
      "width",
      (missilesSize.x + missilesDistance * (MaxMissiles - 1)).toString
    )
    customShotsCanvas.setAttribute("height", missilesSize.y.toString)

    def redrawCanvas(missileAmount: Int): Unit = {
      val renderingCtx = customShotsCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      renderingCtx.clearRect(0, 0, customShotsCanvas.width, customShotsCanvas.height)

      (0 until missileAmount).foreach { index =>
        drawImageAbs(
          renderingCtx,
          attackSimple.element,
          x = missilesDistance * index,
          y = 0,
          missilesSize.x,
          missilesSize.y,
          useAntiAliasing = true
        )
      }
    }

    val amountCustomShots: Select =
      select(
        `class` := "form-select px-3",
        id := "amount-custom-shots-input"
      ).render
    (1 to MaxMissiles).foreach(amount =>
      amountCustomShots.appendChild(option(amount.toString).render)
    )

    amountCustomShots.onchange = _ => {
      val missileAmount = amountCustomShots.value.toInt
      preGameModel
        .subProp(_.defaultTurnAttackTypes)
        .set(List.fill(missileAmount)(AttackType.Simple))
    }
    preGameModel
      .subProp(_.defaultTurnAttackTypes)
      .listen(
        { defaultTurnAttackTypes =>
          amountCustomShots.value = defaultTurnAttackTypes.size.toString
          redrawCanvas(amountCustomShots.value.toInt)
        },
        initUpdate = true
      )

    // TODO how to fix this?
    window.setTimeout(
      () => redrawCanvas(preGameModel.get.defaultTurnAttackTypes.size),
      100
    )

    div(
      `class` := "col-6",
      div(
        `class` := "d-flex flex-wrap",
        div(
          `class` := "btn-group m-3",
          customShotsCanvas,
          label(
            `class` := "input-group-text",
            `for` := amountCustomShots.id,
            nested(translatedDynamic(Translations.Game.timeLimit)(_.apply()))
          ),
          amountCustomShots
        )
      )
    )
  }

}
