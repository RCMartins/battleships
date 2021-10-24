package pt.rmartins.battleships.frontend.views.game

import io.udash.bindings.modifiers.Binding
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.css._
import io.udash.{ModelProperty, bind, produce, toAttrPairOps}
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div}
import pt.rmartins.battleships.frontend.views.game.CanvasUtils.CanvasColor
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.model.game.{Coordinate, Ship}
import scalatags.JsDom.all._

class PreGameView(
    preGameModel: ModelProperty[PreGameModel],
    screenModel: ModelProperty[ScreenModel],
    gamePresenter: GamePresenter,
    canvasUtils: CanvasUtils,
    viewUtils: ViewUtils
) extends CssView {

  private val sqSize = 15
  private val fleetMaxSize: Coordinate = Ship.allShipsFleetMaxX.size
  private val canvasSize: Coordinate = fleetMaxSize * sqSize + Coordinate.square(4)

  def createComponents(divElement: Element, nested: NestedInterceptor): Div = {
    div(
      `class` := "row mx-0 my-2",
      div(
        `class` := "col-6",
        div(
          `class` := "d-flex flex-wrap",
          createAllShipElems
        )
      ),
      div(
        `class` := "col-6",
        createCanvasPreview(divElement, nested)
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
          val usableWidth = Math.max(50, Math.min(290, divElement.clientWidth / 2 - 85))

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
            backgroundColor = backgroundColor
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

}
