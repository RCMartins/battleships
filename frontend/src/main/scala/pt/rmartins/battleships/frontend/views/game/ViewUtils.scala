package pt.rmartins.battleships.frontend.views.game

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import pt.rmartins.battleships.frontend.views.game.CanvasUtils.CanvasColor
import pt.rmartins.battleships.shared.model.game.{Coordinate, Ship}
import scalatags.JsDom.all._

class ViewUtils(canvasUtils: CanvasUtils) {

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
      centerInCanvas: Boolean = false,
      margin: Coordinate = Coordinate.square(2)
  ): Canvas = {
    val shipCanvas = canvas.render
    shipCanvas.setAttribute("width", canvasSize.x.toString)
    shipCanvas.setAttribute("height", canvasSize.y.toString)
    val renderingCtx = shipCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val realShipSize = ship.size * sqSize
    val initialPosition =
      margin + (if (centerInCanvas) canvasSize / 2 - realShipSize / 2 else Coordinate.origin)
    ship.pieces.foreach { shipPiece =>
      canvasUtils.drawBoardSquare(
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
          lineWidth = 1.5,
          alpha = 1.0
        )
    }
    shipCanvas
  }

  def createWaterCanvas(canvasSize: Coordinate, sqSize: Int): Canvas = {
    val waterCanvas = canvas.render
    waterCanvas.setAttribute("width", canvasSize.x.toString)
    waterCanvas.setAttribute("height", canvasSize.y.toString)
    val renderingCtx = waterCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val initialPosition = Coordinate(1, canvasSize.y / 2 - (sqSize / 2))
    canvasUtils.drawBoardSquare(
      renderingCtx,
      initialPosition,
      Coordinate.origin,
      sqSize,
      CanvasColor.Water()
    )
    waterCanvas
  }

}
