package pt.rmartins.battleships.frontend.views.game

import io.udash._
import io.udash.bindings.modifiers.Binding
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.css._
import io.udash.i18n.translatedDynamic
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div, Select}
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

class PreGameView(
    preGameModel: ModelProperty[PreGameModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gamePresenter: GamePresenter,
    canvasUtils: CanvasUtils,
    viewUtils: ViewUtils,
    translationsService: TranslationsService
) extends CssView {

  import translationsService._

  private val MaxCustomNamedRules = 5

  private val defaultHeight = 300
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
          nested(translatedDynamic(Translations.PreGame.fleet)(_.apply()))
        ),
        button(
          `class` := "nav-link btn",
          attr("data-bs-toggle") := "pill",
          attr("data-bs-target") := "#nav-pregame-options",
          `type` := "button",
          role := "tab",
          nested(translatedDynamic(Translations.PreGame.options)(_.apply()))
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
            height := "100%",
            div(
              `class` := "col-6 px-0 overflow-auto",
              div(
                createFleetSaveLoadPanel(nested)
              ),
              hr(`class` := "my-2"),
              div(
                GameStyles.flexContainer,
                height := defaultHeight,
                div(
                  `class` := "row my-0 mx-1",
                  GameStyles.hideScrollX,
                  createAllShipElems
                )
              )
            ),
            div(
              `class` := "col-5",
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
            height := defaultHeight,
            createTimeLimitOptions(nested),
            createCustomShots(nested)
          )
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

  def createFleetSaveLoadPanel(nested: NestedInterceptor): Modifier =
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
            `class` := "btn btn-danger p-1",
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
          `class` := "row",
          div(
            `class` := "input-group d-flex justify-content-center",
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
          )
        ).render
      }
    )

  def createCanvasPreview(divElement: Element, nested: NestedInterceptor): Binding =
    nested(
      produce(
        combine(
          preGameModel.subProp(_.rules).transform(_.boardSize),
          preGameModel.subProp(_.previewBoardOpt),
          translationsModel.subProp(_.previewBoardTitle),
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

          val plusButton =
            button(
              `class` := "btn btn-primary p-2 mx-1",
              span(style := "width: 1rem; height: 1rem;", FontAwesome.Solid.plus)
            ).render
          if (boardSize.x == MaxBoardSize)
            plusButton.disabled = true
          plusButton.onclick = _ =>
            boardSizeProperty.set(
              Coordinate.square(Math.min(MaxBoardSize, boardSize.x + 1))
            )

          val minusButton =
            button(
              `class` := "btn btn-primary p-2 mx-1",
              span(style := "width: 1rem; height: 1rem;", FontAwesome.Solid.minus)
            ).render
          if (boardSize.x == MinBoardSize)
            minusButton.disabled = true
          minusButton.onclick = _ =>
            boardSizeProperty.set(
              Coordinate.square(Math.max(MinBoardSize, boardSize.x - 1))
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
            nested(translatedDynamic(Translations.PreGame.timeLimit)(_.apply()))
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
            nested(translatedDynamic(Translations.PreGame.turnTimeLimit)(_.apply()))
          ),
          turnTimeLimit
        )
      )
    )
  }

  def createCustomShots(nested: NestedInterceptor): Modifier = {
    val imageSize = Coordinate.square(50)
    val missilesDistance = 30
    val MaxMissiles = 5
    val DistanceBetweenMissileTypes = 20
    val radarBetweenDist = 5

    val canvasMaxWidth: Int =
      (imageSize.x + missilesDistance * (MaxMissiles - 1)) +
        DistanceBetweenMissileTypes +
        imageSize.x * MaxMissiles + radarBetweenDist * (MaxMissiles - 1)
    val customShotsCanvas = canvas.render
    customShotsCanvas.setAttribute("width", canvasMaxWidth.toString)
    customShotsCanvas.setAttribute("height", imageSize.y.toString)

    val amountCustomShotsSimple: Select =
      select(
        `class` := "form-select px-2",
        id := "amount-custom-shots-simple-input"
      ).render
    (1 to MaxMissiles).foreach(amount =>
      amountCustomShotsSimple.appendChild(option(b(amount.toString)).render)
    )

    val amountCustomShotsRadar: Select =
      select(
        `class` := "form-select px-2",
        id := "amount-custom-shots-radar-input"
      ).render
    (0 to MaxMissiles).foreach(amount =>
      amountCustomShotsRadar.appendChild(option(b(amount.toString)).render)
    )

    def updateDefaultTurnAttacks(): Unit = {
      val simpleMissileAmount = amountCustomShotsSimple.value.toInt
      val radarAmount = amountCustomShotsRadar.value.toInt
      defaultTurnAttacksProperty.set(
        List.fill(simpleMissileAmount)(AttackType.Simple) ++
          List.fill(radarAmount)(AttackType.Radar)
      )
    }

    amountCustomShotsSimple.onchange = _ => updateDefaultTurnAttacks()
    amountCustomShotsRadar.onchange = _ => updateDefaultTurnAttacks()

    def reloadDefaultTurnAttacks(defaultTurnAttacks: List[AttackType]): Unit = {
      amountCustomShotsSimple.value = defaultTurnAttacks.count(_ == AttackType.Simple).toString
      amountCustomShotsRadar.value = defaultTurnAttacks.count(_ == AttackType.Radar).toString
    }

    defaultTurnAttacksProperty.listen(
      { defaultTurnAttacks => reloadDefaultTurnAttacks(defaultTurnAttacks) },
      initUpdate = true
    )

    val attackSimpleImageCanvas: Canvas =
      CanvasUtils.createCanvasImage(attackSimpleImage, imageSize)

    val radarImageCanvas: Canvas =
      CanvasUtils.createCanvasImage(radarImage, imageSize)

    div(
      `class` := "col-6",
      div(
        `class` := "d-flex flex-wrap",
        div(
          `class` := "row",
          div(
            `class` := "col btn-group m-3",
            label(
              `class` := "input-group-text",
              `for` := amountCustomShotsSimple.id,
              nested(translatedDynamic(Translations.PreGame.amountOfShots)(_.apply()))
            ),
            attackSimpleImageCanvas,
            amountCustomShotsSimple,
            div(`class` := "mx-3"),
            radarImageCanvas,
            amountCustomShotsRadar
          )
        )
      )
    )
  }

}
