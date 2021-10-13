package pt.rmartins.battleships.frontend.views.game

import org.scalajs.dom.CanvasRenderingContext2D
import pt.rmartins.battleships.frontend.views.game.CanvasUtils.CanvasBorder._
import pt.rmartins.battleships.shared.model.game.{Coordinate, Turn}

object CanvasUtils {

  sealed trait CanvasBorder {
    val lineColor: String
    val lineWidth: Double
    val alpha: Double
  }

  object CanvasBorder {

    case object NoBorder extends CanvasBorder {
      val lineColor: String = ""
      val lineWidth: Double = 0
      val alpha: Double = 0
    }

    case class Standard(alpha: Double = 1.0) extends CanvasBorder {
      val lineColor: String = "0, 0, 0"
      val lineWidth: Double = 1.0
    }

    case class Red(alpha: Double = 1.0) extends CanvasBorder {
      val lineColor: String = "255, 0, 0"
      val lineWidth: Double = 1.0
    }

    case class RedBold(alpha: Double = 1.0) extends CanvasBorder {
      val lineColor: String = "255, 0, 0"
      val lineWidth: Double = 2.0
    }

  }

  sealed trait CanvasColor {
    val fillColor: String
    val alpha: Double
    val border: CanvasBorder
  }

  object CanvasColor {

    case class White(border: CanvasBorder = Standard(), alpha: Double = 0.0) extends CanvasColor {
      val fillColor: String = "0, 0, 0"
    }

    case class Ship(border: CanvasBorder = Standard(), alpha: Double = 1.0) extends CanvasColor {
      val fillColor: String = "160, 94, 39"
    }

    case class ShipDarker(border: CanvasBorder = Standard(), alpha: Double = 1.0)
        extends CanvasColor {
      val fillColor: String = "139, 69, 14"
    }

    case class Water(border: CanvasBorder = Standard(), alpha: Double = 1.0) extends CanvasColor {
      val fillColor: String = "0, 206, 209"
    }

    case class WaterDarker(border: CanvasBorder = Standard(), alpha: Double = 1.0)
        extends CanvasColor {
      val fillColor: String = "0, 166, 169"
    }

    case class Red(border: CanvasBorder = Standard(), alpha: Double = 1.0) extends CanvasColor {
      val fillColor: String = "255, 0, 0"
    }

  }

  def drawBoardSquare(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      canvasColor: CanvasColor
  ): Unit =
    drawSquareAbs(renderingCtx, boardPosition + coor * size, size, canvasColor)

  def drawSquareAbs(
      renderingCtx: CanvasRenderingContext2D,
      coor: Coordinate,
      size: Int,
      canvasColor: CanvasColor
  ): Unit = {
    if (canvasColor.fillColor.nonEmpty) {
      renderingCtx.fillStyle = s"rgb(${canvasColor.fillColor}, ${canvasColor.alpha})"
      renderingCtx.fillRect(coor.x, coor.y, size, size)
    }
    if (canvasColor.border.lineColor.nonEmpty) {
      renderingCtx.strokeStyle =
        s"rgb(${canvasColor.border.lineColor}, ${canvasColor.border.alpha})"
      renderingCtx.lineWidth = canvasColor.border.lineWidth
      renderingCtx.beginPath()
      renderingCtx.rect(coor.x, coor.y, size, size)
      renderingCtx.stroke()
    }
  }

  def drawTurnNumberCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      turn: Turn,
      textSize: Int,
      textColor: String = "0, 0, 0"
  ): Unit = {
    val relCoor = boardPosition + coor * size + Coordinate(size / 2, size / 2 + 2)
    renderingCtx.fillStyle = s"rgb($textColor)"
    renderingCtx.font = s"${textSize}px serif"
    renderingCtx.textBaseline = "middle"
    renderingCtx.textAlign = "center"
    renderingCtx.fillText(turn.toTurnString, relCoor.x, relCoor.y)
  }

  def drawCrosshair(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coordinate: Coordinate,
      size: Int,
      lineWidth: Double,
      alpha: Double
  ): Unit = {
    drawCrosshairAbs(
      renderingCtx,
      boardPosition + coordinate * size,
      size,
      lineWidth,
      alpha
    )
  }

  def drawCrosshairAbs(
      renderingCtx: CanvasRenderingContext2D,
      coordinate: Coordinate,
      size: Int,
      lineWidth: Double,
      alpha: Double
  ): Unit = {
    renderingCtx.strokeStyle = s"rgb(255, 0, 0, $alpha)"
    renderingCtx.lineWidth = lineWidth

    renderingCtx.beginPath()
    renderingCtx.moveTo(coordinate.x, coordinate.y)
    renderingCtx.lineTo(coordinate.x + size, coordinate.y + size)
    renderingCtx.stroke()

    renderingCtx.beginPath()
    renderingCtx.moveTo(coordinate.x + size, coordinate.y)
    renderingCtx.lineTo(coordinate.x, coordinate.y + size)
    renderingCtx.stroke()
  }

}
