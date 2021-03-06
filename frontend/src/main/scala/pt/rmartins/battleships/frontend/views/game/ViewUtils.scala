package pt.rmartins.battleships.frontend.views.game

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import pt.rmartins.battleships.frontend.views.game.CanvasUtils._
import pt.rmartins.battleships.shared.model.game.{Coordinate, Ship}
import scalatags.JsDom.all._

class ViewUtils(canvasUtils: CanvasUtils) {

  val defaultMargin: Coordinate = Coordinate.square(2)

  def createEmptyCanvas(x: Int, y: Int): Canvas = {
    val emptyCanvas = canvas.render
    emptyCanvas.setAttribute("width", x.toString)
    emptyCanvas.setAttribute("height", y.toString)
    emptyCanvas
  }

  def createShipCanvas(
      canvasSize: Coordinate,
      sqSize: Int,
      ship: Ship,
      destroyed: Boolean,
      centerXCanvas: Boolean,
      centerYCanvas: Boolean,
      drawRadar: Boolean,
      margin: Coordinate = defaultMargin
  ): Canvas = {
    val shipCanvas = canvas.render
    val sizeWithMargin = canvasSize + margin
    shipCanvas.setAttribute("width", sizeWithMargin.x.toString)
    shipCanvas.setAttribute("height", sizeWithMargin.y.toString)
    val renderingCtx = shipCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val realShipSize = ship.size * sqSize
    val baseInitialPosition: Coordinate =
      margin / 2 +
        Coordinate(
          if (centerXCanvas) canvasSize.x / 2 - realShipSize.x / 2 else 0,
          if (centerYCanvas) canvasSize.y / 2 - realShipSize.y / 2 else 0
        )
    val initialPositionShip: Coordinate =
      baseInitialPosition +
        (if (drawRadar) Coordinate(sqSize * 2 + margin.x, 0) else Coordinate.origin)
    ship.pieces.foreach { shipPiece =>
      canvasUtils.drawBoardSquare(
        renderingCtx,
        initialPositionShip,
        shipPiece,
        sqSize,
        CanvasColor.Ship()
      )
      if (destroyed)
        CanvasUtils.drawCrosshair(
          renderingCtx,
          initialPositionShip,
          shipPiece,
          sqSize,
          lineWidth = 1.5,
          alpha = 1.0
        )
    }
    if (drawRadar) {
      val initialPositionRadar: Coordinate =
        baseInitialPosition + Coordinate(0, -sqSize / 2)

      drawImageAbs(
        renderingCtx,
        radarImage.element,
        initialPositionRadar.x,
        initialPositionRadar.y,
        sqSize * 2,
        sqSize * 2,
        useAntiAliasing = true
      )
    }
    shipCanvas
  }

  def createWaterCanvas(
      canvasSize: Coordinate,
      sqSize: Int,
      centerXCanvas: Boolean,
      centerYCanvas: Boolean,
      drawRadar: Boolean,
      margin: Coordinate = defaultMargin
  ): Canvas = {
    val waterCanvas = canvas.render
    val sizeWithMargin = canvasSize + margin
    waterCanvas.setAttribute("width", sizeWithMargin.x.toString)
    waterCanvas.setAttribute("height", sizeWithMargin.y.toString)
    val renderingCtx = waterCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val baseInitialPosition: Coordinate =
      margin / 2 +
        Coordinate(
          if (centerXCanvas) canvasSize.x / 2 - sqSize / 2 else 0,
          if (centerYCanvas) canvasSize.y / 2 - sqSize / 2 else 0
        )
    val initialPosition: Coordinate =
      baseInitialPosition +
        (if (drawRadar) Coordinate(sqSize * 2 + margin.x, 0) else Coordinate.origin)
    canvasUtils.drawBoardSquare(
      renderingCtx,
      initialPosition,
      Coordinate.origin,
      sqSize,
      CanvasColor.Water()
    )
    if (drawRadar) {
      val initialPositionRadar: Coordinate =
        baseInitialPosition + Coordinate(0, -sqSize / 2)

      drawImageAbs(
        renderingCtx,
        radarImage.element,
        initialPositionRadar.x,
        initialPositionRadar.y,
        sqSize * 2,
        sqSize * 2,
        useAntiAliasing = true
      )
    }
    waterCanvas
  }

}
