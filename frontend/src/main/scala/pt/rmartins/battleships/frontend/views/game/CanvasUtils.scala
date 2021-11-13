package pt.rmartins.battleships.frontend.views.game

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Image
import org.scalajs.dom.raw.HTMLImageElement
import pt.rmartins.battleships.frontend.views.game.BoardView.MinTextSize
import pt.rmartins.battleships.frontend.views.game.CanvasUtils.CanvasColor
import pt.rmartins.battleships.shared.model.game.{Coordinate, Turn}

class CanvasUtils(gamePresenter: GamePresenter) {

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
      canvasColor.border.lineDash.foreach { lineDash =>
        renderingCtx.setLineDash(scalajs.js.Array(lineDash: _*))
        renderingCtx.lineDashOffset = gamePresenter.lineDashOffset.get
      }
      renderingCtx.strokeStyle =
        s"rgb(${canvasColor.border.lineColor}, ${canvasColor.border.alpha})"
      renderingCtx.lineWidth = canvasColor.border.lineWidth
      renderingCtx.beginPath()
      renderingCtx.rect(coor.x, coor.y, size, size)
      renderingCtx.stroke()

      renderingCtx.setLineDash(scalajs.js.Array())
      renderingCtx.lineDashOffset = 0
    }
  }

  def drawBoardLimits(
      renderingCtx: CanvasRenderingContext2D,
      boardTitle: String,
      boardSize: Coordinate,
      boardPosition: Coordinate,
      squareSize: Int,
      backgroundColor: Option[CanvasColor],
      drawAsSelectedTick: Option[Int]
  ): Unit = {
    backgroundColor.foreach(canvasColor =>
      drawSquareAbs(
        renderingCtx,
        boardPosition,
        boardSize.x * squareSize,
        canvasColor
      )
    )

    renderingCtx.strokeStyle = s"rgb(0, 0, 0)"
    renderingCtx.lineWidth = 1.0

    for (x <- 0 to boardSize.x) {
      renderingCtx.beginPath()
      renderingCtx.moveTo(boardPosition.x + x * squareSize, boardPosition.y)
      renderingCtx.lineTo(
        boardPosition.x + x * squareSize,
        boardPosition.y + boardSize.y * squareSize
      )
      renderingCtx.stroke()
    }
    for (y <- 0 to boardSize.y) {
      renderingCtx.beginPath()
      renderingCtx.moveTo(boardPosition.x, boardPosition.y + y * squareSize)
      renderingCtx.lineTo(
        boardPosition.x + boardSize.x * squareSize,
        boardPosition.y + y * squareSize
      )
      renderingCtx.stroke()
    }

    drawAsSelectedTick.foreach { tick =>
      val tickState = 7 - tick % 7
      renderingCtx.beginPath()
      renderingCtx.rect(
        boardPosition.x - tickState,
        boardPosition.y - tickState,
        boardSize.x * squareSize + tickState * 2,
        boardSize.y * squareSize + tickState * 2
      )
      renderingCtx.stroke()

      val tickState2 = Math.max(0, tickState - 3)
      renderingCtx.beginPath()
      renderingCtx.rect(
        boardPosition.x - tickState2,
        boardPosition.y - tickState2,
        boardSize.x * squareSize + tickState2 * 2,
        boardSize.y * squareSize + tickState2 * 2
      )
      renderingCtx.stroke()
    }

    val fontSize = MinTextSize
    renderingCtx.fillStyle = s"rgb(0, 0, 0)"
    renderingCtx.font = s"${fontSize}px serif"
    renderingCtx.textBaseline = "bottom"
    renderingCtx.textAlign = "left"
    renderingCtx.fillText(boardTitle, boardPosition.x, boardPosition.y - 2)
  }

}

object CanvasUtils {

  sealed trait CanvasBorder {

    def lineColor: String
    def lineWidth: Double
    def alpha: Double
    def lineDash: Option[Seq[Double]] = None
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

    case class Bold(alpha: Double = 1.0) extends CanvasBorder {
      val lineColor: String = "0, 0, 0"
      val lineWidth: Double = 2.0
    }

    case class Red(alpha: Double = 1.0) extends CanvasBorder {
      val lineColor: String = "255, 0, 0"
      val lineWidth: Double = 1.0
    }

    case class RedBold(alpha: Double = 1.0) extends CanvasBorder {
      val lineColor: String = "255, 0, 0"
      val lineWidth: Double = 3.0
    }

    case class DashRed(alpha: Double = 1.0, lineWidth: Double = 3.0) extends CanvasBorder {
      val lineColor: String = "255, 0, 0"
      override val lineDash: Option[Seq[Double]] = Some(Seq(4, 2))
    }

    case class DashBlue(alpha: Double = 1.0, lineWidth: Double = 3.0) extends CanvasBorder {
      val lineColor: String = "0, 0, 255"
      override val lineDash: Option[Seq[Double]] = Some(Seq(4, 2))
    }

  }

  sealed trait CanvasColor {
    val fillColor: String
    val alpha: Double
    val border: CanvasBorder
  }

  object CanvasColor {

    import CanvasBorder.Standard

    case class White(border: CanvasBorder = Standard(), alpha: Double = 0.0) extends CanvasColor {
      val fillColor: String = "255, 255, 255"
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
      alpha: Double,
      red: Boolean = true
  ): Unit = {
    renderingCtx.strokeStyle = if (red) s"rgb(255, 0, 0, $alpha)" else s"rgb(0, 0, 255, $alpha)"
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

  class CanvasImage(src: String) {
    val element: HTMLImageElement =
      dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    element.src = src
  }

  val attackSimpleImage: CanvasImage =
    new CanvasImage("icons/missile-simple.png")

  val fillWaterImage: CanvasImage =
    new CanvasImage("icons/fill-water.png")

  def drawImageAbs(
      renderingCtx: CanvasRenderingContext2D,
      image: Image,
      x: Int,
      y: Int,
      width: Int,
      height: Int,
      useAntiAliasing: Boolean
  ): Unit = {
    renderingCtx.imageSmoothingEnabled = useAntiAliasing
    renderingCtx.drawImage(image, 0, 0, 500, 500, x, y, width, height)
  }

}
