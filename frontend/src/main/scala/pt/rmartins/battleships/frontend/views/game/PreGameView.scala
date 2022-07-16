package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import io.udash.bindings.modifiers.Binding
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.css._
import io.udash.i18n.translatedDynamic
import org.scalajs.dom._
import org.scalajs.dom.html.{Anchor, Canvas, Div, Select, Span}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.CanvasUtils._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.frontend.views.model.NamedRules
import pt.rmartins.battleships.shared.css.GameStyles
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game.RuleTimeLimit._
import pt.rmartins.battleships.shared.model.game._
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class PreGameView(
    preGameModel: ModelProperty[PreGameModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gamePresenter: GamePresenter,
    canvasUtils: CanvasUtils,
    viewUtils: ViewUtils,
    gameModals: GameModals,
    translationsService: TranslationsService
) extends CssView {

  import translationsService._

  private val MaxCustomNamedRules = 5

//  private val defaultHeight = 360
//  private val PreviewBoardHeight = 320
//  private val BonusEditorCardHeight = 285
  private val fleetMaxSize: Coordinate = Ship.allShipsFleetMaxX.maxSize
  private val sqSize: Int = Math.min(100 / fleetMaxSize.x, 50 / fleetMaxSize.y)
  private val canvasSize: Coordinate = fleetMaxSize * sqSize + Coordinate.square(4)

  private val MinBoardSize = 4
  private val MaxBoardSize = 20

  private val boardSizeProperty: Property[Coordinate] =
    gamePresenter.preGameRulesProperty.bitransform(_.boardSize)(boardSize =>
      gamePresenter.preGameRulesProperty.get.copy(boardSize = boardSize)
    )

  private val defaultTurnAttacksProperty: Property[List[AttackType]] =
    gamePresenter.preGameRulesProperty.bitransform(_.defaultTurnAttacks)(defaultTurnAttacks =>
      gamePresenter.preGameRulesProperty.get.copy(defaultTurnAttacks = defaultTurnAttacks)
    )

  private val timeLimitProperty: Property[RuleTimeLimit] =
    gamePresenter.preGameRulesProperty.bitransform(_.timeLimit)(timeLimit =>
      gamePresenter.preGameRulesProperty.get.copy(timeLimit = timeLimit)
    )

  def createComponents(nested: NestedInterceptor): Div = {
    div(
      `class` := "d-flex row",
      div(
        `class` := "col-12",
        div(
          `class` := "card col-12 p-0 mt-3",
          div(
            `class` := "card-header",
            createFleetSaveLoadDiv(nested)
          ),
          div(
            `class` := "row m-2 justify-content-between",
            div(
              `class` := "col-lg-7 col-md-12 px-0", // col-lg-6 col-md-12
              div(
//                GameStyles.flexContainer,
//                height := defaultHeight,
                div(
                  `class` := "row my-0 mx-1",
//                  GameStyles.hideScrollX,
                  createAllShipElems
                )
              )
            ),
            div(
              `class` := "col-lg-5 col-md-12",
              div(
                `class` := "d-flex justify-content-start",
                createCanvasPreview(nested),
                createFleetPreview(nested),
              )
            )
          )
        )
      ),
      div(
        `class` := "col-12",
        div(
          `class` := "row mx-0 my-3",
          createFirstColumnOptions(nested),
          createSecondColumnOptions(nested)
        )
      )
    ).render
  }

  private def createAllShipElems: Modifier = {
    def createShipCanvas(ship: Ship): Canvas =
      viewUtils.createShipCanvas(
        canvasSize,
        sqSize,
        ship,
        destroyed = false,
        centerXCanvas = true,
        centerYCanvas = true,
        drawRadar = false
      )

    Ship.allShipsFleetMaxX.shipsList.map { ship =>
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
        `class` := "col p-0",
        div(
          `class` := "mx-2 py-2 g-3 btn-group rounded-start",
          span(
            `class` := "rounded-start bg-primary text-white py-1 px-2",
            style := "user-select: none",
            bind(shipCountProperty)
          ),
          plusButton,
          minusButton
        ),
        div(
          `class` := "row m-0",
          div(
            `class` := "mx-2",
            createShipCanvas(ship)
          )
        )
      )
    }
  }

  private def createFleetSaveLoadDiv(nested: NestedInterceptor): Modifier =
    nested(
      produceWithNested(
        combine(
          preGameModel.subProp(_.customNamedRulesMap),
          preGameModel.subProp(_.selectedNamedRule),
          preGameModel.subProp(_.rules)
        )
      ) { case ((customNamedRulesMap, selectedNamedRuleOpt, rules), nested) =>
        val unsavedFleetName = "<no-name>"

        val customFleets: Seq[(String, Option[NamedRules])] =
          (
            selectedNamedRuleOpt match {
              case None    => Seq((unsavedFleetName, None))
              case Some(_) => Seq()
            }
          ) ++
            customNamedRulesMap.toList.map { case (name, namedRules) => (name, Some(namedRules)) }

        val activateSaveChanges: Boolean =
          selectedNamedRuleOpt match {
            case None =>
              true
            case Some(NamedRules(name, selectedNamedRule))
                if customNamedRulesMap.contains(name) && selectedNamedRule != rules =>
              true
            case _ =>
              false
          }

        val allSelectorItems =
          (if (customFleets.nonEmpty)
             Seq(
               option(
                 disabled,
                 "# ",
                 span(nested(translatedDynamic(Translations.PreGame.yourFleets)(_.apply()))),
                 ":"
               )
             )
           else
             Seq.empty) ++
            customFleets.map { case (fleetName, _) =>
              option(fleetName)
            } ++
            Seq(
              option(
                disabled,
                "# ",
                span(nested(translatedDynamic(Translations.PreGame.defaultFleets)(_.apply()))),
                ":"
              )
            ) ++
            PreGameModel.allDefaultNamedRules.map { case NamedRules(fleetName, _) =>
              option(fleetName)
            } ++ {
              if (customNamedRulesMap.sizeIs >= MaxCustomNamedRules)
                Seq.empty
              else
                Seq(
                  option(disabled, "-" * 20),
                  option(
                    span(nested(translatedDynamic(Translations.PreGame.createNewFleet)(_.apply())))
                  )
                )
            }

        val fleetSelector: Select =
          select(
            `class` := "custom-select",
            allSelectorItems
          ).render

        val editButton =
          button(
            `class` := "btn btn-info",
            span(FontAwesome.Solid.pencilAlt)
          ).render

        editButton.onclick = _ => {
          selectedNamedRuleOpt.foreach { case NamedRules(name, _) =>
            screenModel.subProp(_.namedRuleNameBefore).set(Some(name))
            screenModel.subProp(_.namedRuleName).set(name)
            Globals.modalToggle("fleet-name-modal")
          }
        }

        val saveButton =
          button(
            `class` := "btn btn-primary p-1",
            span(nested(translatedDynamic(Translations.PreGame.saveFleet)(_.apply())))
          ).render

        if (!activateSaveChanges)
          saveButton.classList.add("invisible")

        fleetSelector.onchange = _ => {
          if (
            fleetSelector.selectedIndex == allSelectorItems.size - 1 &&
            customNamedRulesMap.sizeIs < MaxCustomNamedRules
          ) {
            preGameModel.subProp(_.selectedNamedRule).set(None)
            preGameModel.subProp(_.selectedNamedRuleChanges).set(true)
            fleetSelector.value = unsavedFleetName
          } else
            customNamedRulesMap
              .get(fleetSelector.value)
              .orElse(
                PreGameModel.allDefaultNamedRules.find(_.name == fleetSelector.value)
              )
              .foreach { case namedRules @ NamedRules(_, rules) =>
                preGameModel.subProp(_.previewEnabled).set(false)
                preGameModel.subProp(_.rules).set(rules)
                gamePresenter.shipCounter.foreach(_._2.set(0))
                rules.gameFleet.shipCounterList.foreach { case (shipId, (counter, _)) =>
                  gamePresenter.getPreGameShipProperty(shipId).set(counter)
                }
                preGameModel.subProp(_.previewEnabled).set(true)
                preGameModel.subProp(_.selectedNamedRule).set(Some(namedRules))
                preGameModel.subProp(_.selectedNamedRuleChanges).set(false)
                saveButton.classList.add("invisible")
              }
        }

        fleetSelector.value = selectedNamedRuleOpt match {
          case None                      => unsavedFleetName
          case Some(NamedRules(name, _)) => name
        }

        saveButton.onclick = _ => {
          selectedNamedRuleOpt match {
            case None =>
              screenModel.subProp(_.namedRuleNameBefore).set(None)
              screenModel.subProp(_.namedRuleName).set("")
              Globals.modalToggle("fleet-name-modal")
            case Some(NamedRules(name, _)) =>
              screenModel.subProp(_.namedRuleName).set(name)
              gamePresenter.saveNewNamedRules()
          }
        }

        val clearButton =
          button(
            `class` := "btn btn-danger px-2 py-1",
            span(FontAwesome.Solid.trash),
            span(" "),
            span(nested(translatedDynamic(Translations.PreGame.clearAllShips)(_.apply())))
          ).render

        clearButton.onclick = _ => {
          preGameModel.subProp(_.previewEnabled).set(false)
          gamePresenter.shipCounter.foreach(_._2.set(0))
          preGameModel.subProp(_.previewEnabled).set(true)
        }

        val editAndSaveButtons: Seq[Modifier] =
          selectedNamedRuleOpt match {
            case None =>
              Seq(
                div(
                  `class` := "input-group-append",
                  saveButton
                ),
                div(
                  `class` := "input-group-append invisible",
                  editButton
                )
              )
            case Some(NamedRules(name, _)) if customNamedRulesMap.contains(name) =>
              Seq(
                div(
                  `class` := "input-group-append",
                  editButton
                ),
                div(
                  `class` := "input-group-append",
                  saveButton
                )
              )
            case _ =>
              Seq(
                div(
                  `class` := "input-group-append invisible",
                  editButton
                ),
                div(
                  `class` := "input-group-append",
                  saveButton
                )
              )
          }

        div(
          `class` := "input-group d-flex justify-content-start",
          div(
            `class` := "input-group-prepend",
            span(
              `class` := "input-group-text",
              style := "user-select: none",
              nested(translatedDynamic(Translations.PreGame.fleet)(_.apply()))
            )
          ),
          div(
            fleetSelector
          ),
          editAndSaveButtons,
          div(
            `class` := "ml-4",
            clearButton
          )
        ).render
      }
    )

  private def createCanvasPreview(nested: NestedInterceptor): Binding = {
    val plusButton =
      button(
        `class` := "btn btn-primary p-2 btn-block",
        b(nested(translatedDynamic(Translations.PreGame.size)(_.apply()))),
        " ",
        span(
          style := "width: 1rem; height: 1rem;",
          FontAwesome.Solid.plus
        )
      ).render

    val minusButton =
      button(
        `class` := "btn btn-primary p-2 btn-block",
        b(nested(translatedDynamic(Translations.PreGame.size)(_.apply()))),
        " ",
        span(
          style := "width: 1rem; height: 1rem;",
          FontAwesome.Solid.minus
        )
      ).render

    nested(
      produce(
        combine(
          preGameModel.subProp(_.rules).transform(_.boardSize),
          preGameModel.subProp(_.previewBoardOpt),
          translationsModel.subProp(_.previewBoardTitle),
          screenModel.subProp(_.screenResized),
          screenModel.subProp(_.mainBoardCanvasSize)
        )
      ) {
        case (boardSize, previewBoardOpt, previewBoardTitle, _, defaultCanvasSize) =>
//          val usableWidth = PreviewBoardHeight

          val boardMargin = Coordinate(1, 1)
          val maxWidthPixels = defaultCanvasSize.x
          val sqSize = maxWidthPixels / boardSize.max
          val canvasSize = Coordinate.square(maxWidthPixels) + boardMargin * 2
          val previewCanvas: Canvas = canvas.render
          previewCanvas.setAttribute("width", canvasSize.x.toString)
          previewCanvas.setAttribute("height", canvasSize.y.toString)
          val renderingCtx = previewCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

          val backgroundColor: Option[CanvasColor] =
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
//            boardTitle,
            boardSize,
            boardMargin,
            sqSize,
            backgroundColor = backgroundColor,
            None
          )

          previewBoardOpt.foreach { case (board, _) =>
            board.ships.foreach { ship =>
              ship.shipActualPieces.foreach { shipPiece =>
                canvasUtils.drawBoardSquare(
                  renderingCtx,
                  boardMargin,
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

          val progressBar: Div =
            div(
              `class` := s"progress-bar progress-bar-striped $color",
              role := "progressbar",
              style := s"width: $perc%",
              s"$perc%"
            ).render

          plusButton.disabled = boardSize.x == MaxBoardSize
          plusButton.onclick = _ =>
            boardSizeProperty.set(
              Coordinate.square(Math.min(MaxBoardSize, boardSize.x + 1))
            )

          minusButton.disabled = boardSize.x == MinBoardSize
          minusButton.onclick = _ =>
            boardSizeProperty.set(
              Coordinate.square(Math.max(MinBoardSize, boardSize.x - 1))
            )

          Seq[Node](
            div(
              `class` := "row",
              div(
                `class` := "col-12 p-0 mx-3",
                maxWidth := maxWidthPixels,
                previewCanvas,
                div(
                  `class` := "col-12 p-0 progress",
                  progressBar
                ),
                div(
                  `class` := "col-12 p-0",
                  div(
                    `class` := "row m-0",
                    div(
                      `class` := "col-6 p-1",
                      minusButton,
                    ),
                    div(
                      `class` := "col-6 p-1",
                      plusButton,
                    )
                  )
                )
              ),
            ).render
          )
        case _ =>
          Seq.empty
      }
    )
  }

  def createFleetPreview(nested: NestedInterceptor): Binding = {
    nested(
      produce(
        combine(
          preGameModel.subProp(_.rules).transform(_.gameFleet),
          screenModel.subProp(_.mainBoardCanvasSize)
        )
      ) { case (gameFleet, canvasSize) =>
        val fleetSorted: List[(Ship, Int)] =
          gameFleet.shipCounterList
            .filter(_._2._1 > 0)
            .map { case (shipId, (amount, rotation)) => (Ship.getShip(shipId, rotation), amount) }
            .sortBy { case (ship, _) => (ship.piecesSize, ship.shipId.id) }

        val maxTotalHeight1column = 40
        val totalFleetYSize = Math.max(10, fleetSorted.map(_._1.size.y).sum + fleetSorted.size)
        val (previewSqSize, twoColumns) =
          if (totalFleetYSize <= maxTotalHeight1column)
            (Math.max(7, canvasSize.y / totalFleetYSize - 2), false)
          else
            (Math.max(7, canvasSize.y / (totalFleetYSize / 2) - 2), true)

        def createShipCanvas(ship: Ship): Canvas = {
          val canvasSize: Coordinate = ship.size * previewSqSize + Coordinate.square(4)
          viewUtils.createShipCanvas(
            canvasSize,
            previewSqSize,
            ship,
            destroyed = false,
            centerXCanvas = true,
            centerYCanvas = true,
            drawRadar = false
          )
        }

        val fleetDivs: List[JsDom.TypedTag[Div]] =
          fleetSorted.map { case (ship, amount) =>
            div(
              `class` := (if (twoColumns) "col-6" else "col-12"),
              div(
                (1 to amount).map(_ => createShipCanvas(ship))
              )
            )
          }

        div(
          `class` := "d-flex align-items-start",
          div(
            `class` := "row mx-0 my-3",
            fleetDivs
          )
        ).render
      }
    )
  }

  private def createFirstColumnOptions(nested: NestedInterceptor): Modifier =
    div(
      `class` := "col-6",
      div(
        `class` := "row",
        createTimeLimitOptions(nested),
        createCustomShots(nested)
      )
    )

  private def createSecondColumnOptions(nested: NestedInterceptor): Modifier =
    div(
      `class` := "col-6",
      div(
        `class` := "row",
        createCustomBonusDiv(nested)
      )
    )

  private def createTimeLimitOptions(nested: NestedInterceptor): Modifier = {
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

    val timeLimitOpt: Option[WithRuleTimeLimit] = timeLimitProperty.get.toOption

    val totalTimeLimitOptions: List[Int] =
      (timeLimitOpt.map(_.initialTotalTimeSeconds).toList ++
        List(1, 3, 5, 10, 15, 20, 30, 60).map(_ * 60)).distinct.sorted
    val totalTimeLimit: Select =
      select(
        `class` := "px-2",
        id := "time-limit-input"
      ).render
    totalTimeLimitOptions.foreach(seconds =>
      totalTimeLimit.appendChild(option(selectSecondsToString(seconds)).render)
    )
    timeLimitOpt
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
      (timeLimitOpt.flatMap(_.additionalTurnTimeSeconds.map(_._1)).toList ++
        List(1, 3, 5, 10, 15, 20, 30, 60)).distinct.sorted
    val turnTimeLimit: Select =
      select(
        `class` := "form-select px-2",
        id := "turn-time-limit-input"
      ).render
    turnTimeLimitOptions.foreach(seconds =>
      turnTimeLimit.appendChild(option(selectSecondsToString(seconds)).render)
    )
    timeLimitOpt
      .flatMap(_.additionalTurnTimeSeconds.map(_._1))
      .foreach(seconds => turnTimeLimit.value = selectSecondsToString(seconds))

    def generateRuleTimeLimitOpt: RuleTimeLimit =
      if (totalTimeLimitCheckBox.checked)
        WithRuleTimeLimit(
          selectOptionToSeconds(totalTimeLimit.value),
          if (turnTimeLimitCheckBox.checked)
            Some((selectOptionToSeconds(turnTimeLimit.value), false))
          else
            None
        )
      else
        WithoutRuleTimeLimit

    def updateRuleTimeLimit(): Unit =
      timeLimitProperty.set(generateRuleTimeLimitOpt)

    timeLimitProperty.listen(
      {
        case WithoutRuleTimeLimit =>
          totalTimeLimitCheckBox.checked = false
          totalTimeLimit.disabled = true
          turnTimeLimitCheckBox.checked = false
          turnTimeLimitCheckBox.disabled = true
          turnTimeLimit.disabled = true
        case WithRuleTimeLimit(initialTotalTimeSeconds, additionalTurnTimeSecondsOpt) =>
          totalTimeLimitCheckBox.checked = true
          totalTimeLimit.disabled = false
          totalTimeLimit.value = selectSecondsToString(initialTotalTimeSeconds)
          turnTimeLimitCheckBox.checked = additionalTurnTimeSecondsOpt.nonEmpty
          turnTimeLimitCheckBox.disabled = false
          turnTimeLimit.disabled = additionalTurnTimeSecondsOpt.isEmpty
          additionalTurnTimeSecondsOpt.foreach { case (additionalTurnTimeSeconds, _) =>
            turnTimeLimit.value = selectSecondsToString(additionalTurnTimeSeconds)
          }
      },
      initUpdate = true
    )

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

    Seq[Modifier](
      div(
        `class` := "card col-12 p-0",
        h5(
          `class` := "card-header",
          nested(translatedDynamic(Translations.PreGame.timeLimit)(_.apply()))
        ),
        div(
          `class` := "row m-0",
          div(
            `class` := "col-lg-6 col-md-12 btn-group my-3",
            totalTimeLimitCheckBox,
            label(
              `class` := "input-group-text",
              `for` := totalTimeLimit.id,
              nested(translatedDynamic(Translations.PreGame.timeLimit)(_.apply()))
            ),
            totalTimeLimit
          ),
          div(
            `class` := "col-lg-6 col-md-12 btn-group my-3",
            turnTimeLimitCheckBox,
            label(
              `class` := "input-group-text",
              `for` := turnTimeLimit.id,
              nested(translatedDynamic(Translations.PreGame.turnTimeLimit)(_.apply()))
            ),
            turnTimeLimit
          )
        )
      )
    )
  }

  private def createEditTurnDivs(
      imageSize: Coordinate,
      updateDefaultTurnAttacksF: (Int, Int) => Unit,
      minimumMissiles: Int,
      maximumShots: Int
  ): (Seq[Modifier], List[AttackType] => Unit) = {

    val amountCustomShotsSimple: Select =
      select(
        `class` := "form-select px-2",
        id := "amount-custom-shots-simple-input"
      ).render
    (minimumMissiles to maximumShots).foreach(amount =>
      amountCustomShotsSimple.appendChild(option(b(amount.toString)).render)
    )

    val amountCustomShotsRadar: Select =
      select(
        `class` := "form-select px-2",
        id := "amount-custom-shots-radar-input"
      ).render
    (0 to maximumShots).foreach(amount =>
      amountCustomShotsRadar.appendChild(option(b(amount.toString)).render)
    )

    amountCustomShotsSimple.onchange = _ =>
      updateDefaultTurnAttacksF(
        amountCustomShotsSimple.value.toInt,
        amountCustomShotsRadar.value.toInt
      )
    amountCustomShotsRadar.onchange = _ =>
      updateDefaultTurnAttacksF(
        amountCustomShotsSimple.value.toInt,
        amountCustomShotsRadar.value.toInt
      )

    def reloadDefaultTurnAttacks(defaultTurnAttacks: List[AttackType]): Unit = {
      amountCustomShotsSimple.value = defaultTurnAttacks.count(_ == AttackType.Simple).toString
      amountCustomShotsRadar.value = defaultTurnAttacks.count(_ == AttackType.Radar).toString
    }

    val attackSimpleImageCanvas: Canvas =
      CanvasUtils.createCanvasImage(attackSimpleImage, imageSize)

    val radarImageCanvas: Canvas =
      CanvasUtils.createCanvasImage(radarImage, imageSize)

    (
      Seq(
        attackSimpleImageCanvas,
        amountCustomShotsSimple,
        div(`class` := "mx-3"),
        radarImageCanvas,
        amountCustomShotsRadar
      ),
      attackTypesList => reloadDefaultTurnAttacks(attackTypesList)
    )
  }

  private def createCustomShots(nested: NestedInterceptor): Modifier = {
    val imageSize = Coordinate.square(50)

    def updateDefaultTurnAttacks(simpleMissileAmount: Int, radarAmount: Int): Unit =
      defaultTurnAttacksProperty.set(
        List.fill(simpleMissileAmount)(AttackType.Simple) ++
          List.fill(radarAmount)(AttackType.Radar)
      )

    val (editTurnDivs, reloadMissilesF) = createEditTurnDivs(
      imageSize,
      (simpleShots, radarShots) => updateDefaultTurnAttacks(simpleShots, radarShots),
      minimumMissiles = 1,
      maximumShots = 5
    )

    defaultTurnAttacksProperty.listen(
      { defaultTurnAttacks => reloadMissilesF(defaultTurnAttacks) },
      initUpdate = true
    )

    div(
      `class` := "card col-12 p-0 mt-3",
      h5(
        `class` := "card-header",
        nested(translatedDynamic(Translations.PreGame.amountOfShots)(_.apply()))
      ),
      div(
        `class` := "row m-0",
        div(
          `class` := "col-12 btn-group my-3",
          editTurnDivs
        )
      )
    )
  }

  private def createCustomBonusDiv(nested: NestedInterceptor): Modifier = {
    val attackTypeOrder: Map[AttackType, Int] =
      AttackType.all.map { attackType =>
        (
          attackType,
          attackType match {
            case AttackType.Simple => 0
            case AttackType.Radar  => 1
          }
        )
      }.toMap

    def createExtraTurnDiv(attackTypes: List[AttackType]): JsDom.TypedTag[Div] = {
      val imageSize = Coordinate.square(25)

      def createAttackTypeDiv(attackType: AttackType, amount: Int): JsDom.TypedTag[Div] =
        div(
          `class` := "mr-4 d-flex align-items-center",
          (1 to amount).map { _ =>
            CanvasUtils
              .createCanvasImage(CanvasImage.fromAttackType(attackType), imageSize)
              .tap { canvas =>
                canvas.classList.add("border")
                canvas.classList.add("border-dark")
                canvas.style.marginLeft = "-11px"
              }
          }
        )

      div(
        `class` := "d-flex align-items-center",
        span(
          `class` := "align-middle mr-4",
          nested(translatedDynamic(Translations.PreGame.turnBonuses)(_.apply()))
        ),
        attackTypes
          .groupBy(identity)
          .toList
          .sortBy { case (attackType, _) => attackTypeOrder(attackType) }
          .map { case (attackType, list) => createAttackTypeDiv(attackType, list.size) }
      )
    }

    def createExtraTurnEditDiv(
        attackTypes: List[AttackType],
        updateBonusReward: Option[BonusReward] => Unit
    ): JsDom.TypedTag[Div] = {
      val imageSize = Coordinate.square(40)

      val (editTurnDivs, reloadMissilesF) = createEditTurnDivs(
        imageSize,
        (simpleMissileAmount, radarAmount) =>
          updateBonusReward(
            Some(
              BonusReward.ExtraTurn(
                List.fill(simpleMissileAmount)(AttackType.Simple) ++
                  List.fill(radarAmount)(AttackType.Radar)
              )
            )
          ),
        minimumMissiles = 0,
        maximumShots = 6
      )
      reloadMissilesF(attackTypes)

      div(
        `class` := "btn-group justify-content-between",
        editTurnDivs
      )
    }

    def createBonusRewardDiv(bonusReward: BonusReward): JsDom.TypedTag[Div] =
      div(
        bonusReward match {
          case BonusReward.ExtraTurn(attackTypes) =>
            createExtraTurnDiv(attackTypes)
        }
      )

    def createBonusRewardEditDiv(
        bonusReward: BonusReward,
        updateBonusReward: Option[BonusReward] => Unit
    ): JsDom.TypedTag[Div] = {
      val deleteSpan: Span =
        span(
          `class` := "rounded p-2 d-flex justify-content-center bg-danger text-white",
          style := "cursor: pointer",
          width := "32px",
          FontAwesome.Solid.trash
        ).render

      deleteSpan.onclick = _ => {
        updateBonusReward(None)
      }

      div(
        `class` := "card",
        div(
          `class` := "row m-2",
          div(`class` := "ml-3 mr-5 d-flex align-items-center", deleteSpan),
          bonusReward match {
            case BonusReward.ExtraTurn(attackTypes) =>
              createExtraTurnEditDiv(attackTypes, updateBonusReward)
          }
        )
      )
    }

    def createTurnBonusDiv(bonusType: BonusType): JsDom.TypedTag[Anchor] = {
      val deleteBonusSpan: Span =
        span(
          `class` := "rounded p-2 d-flex justify-content-center bg-danger text-white",
          style := "cursor: pointer",
          width := "32px",
          FontAwesome.Solid.times
        ).render

      deleteBonusSpan.onclick = _ => {
        preGameModel
          .subProp(_.rules)
          .set(
            preGameModel.subProp(_.rules).get.modify(_.gameBonuses).using {
              _.filterNot(_.bonusType == bonusType)
            }
          )
      }

      val editSpan: Span =
        span(
          `class` := "rounded p-2 bg-primary text-white",
          style := "cursor: pointer",
          FontAwesome.Solid.pencilAlt
        ).render

      editSpan.onclick = _ => {
        preGameModel.subProp(_.editGameBonusType).set(bonusType)
        preGameModel
          .subProp(_.editGameBonusRewards)
          .set(
            preGameModel
              .subProp(_.rules)
              .get
              .gameBonuses
              .find(_.bonusType == bonusType)
              .map(_.bonusRewardList)
              .getOrElse(Nil)
          )

        val updateBonusRewardF: Int => Option[BonusReward] => Unit =
          (index: Int) => {
            case None =>
              val original = preGameModel.subProp(_.editGameBonusRewards).get
              preGameModel
                .subProp(_.editGameBonusRewards)
                .set(
                  original.take(index) ++ original.drop(index + 1)
                )
            case Some(updatedBonusReward) =>
              preGameModel
                .subProp(_.editGameBonusRewards)
                .set(
                  preGameModel
                    .subProp(_.editGameBonusRewards)
                    .get
                    .updated(index, updatedBonusReward)
                )
          }

        preGameModel
          .subProp(_.editGameBonusDiv)
          .set(
            div(
              `class` := "my-2",
              nested(produce(preGameModel.subProp(_.editGameBonusRewards)) { bonusRewardList =>
                bonusRewardList.zipWithIndex.map { case (bonusReward, index) =>
                  createBonusRewardEditDiv(bonusReward, updateBonusRewardF(index)).render
                }
              })
            ).render
          )
        Globals.modalToggle(gameModals.editGameBonusModalId)
      }

      a(
        `class` := "list-group-item",
        div(
          `class` := "row",
          div(
            `class` := "col-2 d-flex align-items-center",
            deleteBonusSpan
          ),
          div(
            `class` := "col-8",
            nested(
              produce(preGameModel.subProp(_.rules).transform(_.gameBonuses)) { rulesTurnBonuses =>
                val bonusRewardOpt: Option[List[BonusReward]] =
                  rulesTurnBonuses
                    .find(_.bonusType == bonusType)
                    .map(_.bonusRewardList)

                bonusRewardOpt match {
                  case Some(_) => deleteBonusSpan.classList.remove("invisible")
                  case None    => deleteBonusSpan.classList.add("invisible")
                }

                val bonusTitleClass: String =
                  bonusRewardOpt match {
                    case Some(_) => "mr-3"
                    case None    => "mr-3 text-muted"
                  }

                (
                  div(
                    `class` := "d-flex w-100 justify-content-between",
                    h5(
                      `class` := bonusTitleClass,
                      nested(TranslationUtils.bonusTypeToText(bonusType))
                    )
                  ) +: bonusRewardOpt.getOrElse(Nil).map(createBonusRewardDiv)
                ).render
              }
            )
          ),
          div(
            `class` := "col-2 d-flex align-items-center",
            editSpan
          )
        )
      )
    }

    div(
      `class` := "col-12 pr-0",
      div(
        `class` := "card",
        h5(
          `class` := "card-header",
          nested(translatedDynamic(Translations.PreGame.gameBonusTitle)(_.apply()))
        ),
//        div(
//          height := "100%",
        div(
          `class` := "px-0",
          div(
            `class` := "list-group",
//              GameStyles.flexContainer,
//              height := BonusEditorCardHeight,
//            div(
//              BonusType.AllBonusType.map(createTurnBonusDiv)
//            )
            div(
              `class` := "row m-0",
              BonusType.AllBonusType.map { bonusType =>
                div(
                  `class` := "col-lg-6 col-md-12 p-0",
                  createTurnBonusDiv(bonusType)
                )
              }
            )
          )
        )
//      )
      )
    )
  }

}
