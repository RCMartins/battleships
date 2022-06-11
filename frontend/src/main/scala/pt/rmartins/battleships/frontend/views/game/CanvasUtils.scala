package pt.rmartins.battleships.frontend.views.game

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, Event}
import org.scalajs.dom.html.{Canvas, Image}
import org.scalajs.dom.raw.HTMLImageElement
import pt.rmartins.battleships.frontend.views.game.BoardView.MinTextSize
import pt.rmartins.battleships.frontend.views.game.CanvasUtils.CanvasColor
import pt.rmartins.battleships.shared.model.game.{AttackType, Coordinate, Turn}
import scalatags.JsDom.all.canvas

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
  ): Unit =
    CanvasUtils.drawSquareAbs(
      renderingCtx,
      coor,
      size,
      canvasColor,
      gamePresenter.lineDashOffset.get
    )

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

    case class DarkGreen(border: CanvasBorder = Standard(), alpha: Double = 1.0)
        extends CanvasColor {
      val fillColor: String = "51, 204, 51"
    }

  }

  def drawSquareAbs(
      renderingCtx: CanvasRenderingContext2D,
      coor: Coordinate,
      size: Int,
      canvasColor: CanvasColor,
      lineDashOffset: Int
  ): Unit = {
    if (canvasColor.fillColor.nonEmpty) {
      renderingCtx.fillStyle = s"rgb(${canvasColor.fillColor}, ${canvasColor.alpha})"
      renderingCtx.fillRect(coor.x, coor.y, size, size)
    }
    if (canvasColor.border.lineColor.nonEmpty) {
      canvasColor.border.lineDash.foreach { lineDash =>
        renderingCtx.setLineDash(scalajs.js.Array(lineDash: _*))
        renderingCtx.lineDashOffset = lineDashOffset
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

  object CanvasImage {

    def fromAttackType(attackType: AttackType): CanvasImage =
      attackType match {
        case AttackType.Simple => attackSimpleImage
        case AttackType.Radar  => radarImage
      }

  }

  val attackSimpleImage: CanvasImage =
    new CanvasImage("icons/missile-simple.png")

  val fillWaterImage: CanvasImage =
    new CanvasImage("icons/fill-water.png")

  val radarImage: CanvasImage =
    new CanvasImage("icons/radar2.png")

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

  def createEmptyCanvas(size: Coordinate): Canvas = {
    val newImageCanvas: Canvas = canvas.render
    newImageCanvas.setAttribute("width", size.x.toString)
    newImageCanvas.setAttribute("height", size.y.toString)

    val renderingCtx = newImageCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    renderingCtx.fillStyle = "white"
    renderingCtx.fillRect(0, 0, size.x, size.y)

    newImageCanvas
  }

  def drawCanvasImage(
      canvas: Canvas,
      position: Coordinate,
      canvasImage: CanvasImage,
      size: Coordinate,
      drawBeforeF: () => Unit = () => ()
  ): Canvas = {
    val renderingCtx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    def draw(): Unit =
      if (!canvasImage.element.complete) {
        val before = Option(canvasImage.element.onload)
        canvasImage.element.onload = { (event: Event) =>
          before.foreach(_(event))
          draw()
        }
      } else {
        drawBeforeF()
        drawImageAbs(
          renderingCtx,
          canvasImage.element,
          x = position.x,
          y = position.y,
          size.x,
          size.y,
          useAntiAliasing = true
        )
      }

    draw()
    canvas
  }

  def createCanvasImage(canvasImage: CanvasImage, size: Coordinate): Canvas =
    drawCanvasImage(createEmptyCanvas(size), Coordinate(1, 1), canvasImage, size - Coordinate(2, 2))

  def createStackedShotsCanvas(
      canvasImage: CanvasImage,
      amount: Int,
      size: Int,
      stackedDist: Int
  ): Canvas = {
    assume(amount >= 1)
    val canvas =
      CanvasUtils.createEmptyCanvas(Coordinate(size + (amount - 1) * stackedDist + 2, size + 2))
    val renderingCtx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    (0 until amount).foreach { index =>
      val position = Coordinate(1 + index * stackedDist, 1)

      drawCanvasImage(
        canvas,
        position,
        canvasImage,
        Coordinate.square(size - 2),
        drawBeforeF = () => {
//          drawSquareAbs(
//            renderingCtx,
//            position,
//            size,
//            CanvasColor.White(bo alpha = 1.0),
//            0
//          )
        }
      )
    }
    canvas
  }

}
