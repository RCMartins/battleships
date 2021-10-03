package pt.rmartins.battleships.frontend.views.game

import io.udash.{ModelProperty, ReadableProperty}
import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.{CanvasRenderingContext2D, html}
import pt.rmartins.battleships.frontend.views.game.BoardView.ToPlaceShip
import pt.rmartins.battleships.shared.model.game.GameMode.{InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._

class BoardView(
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    gamePresenter: GamePresenter,
    myBoardCanvas: Canvas
) {

  private val DefaultSquareSize: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform(size => if (size.x < 1000) 20 else 30)
  private val PlaceShipsSize: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform(size => if (size.x < 1000) 12 else 15)
  private val MyBoardInGameSize: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform(size => if (size.x < 1000) 14 else 20)

  private val AbsInitial: Coordinate = Coordinate(1, 1)
  private val MyBoardPosPreGame: Coordinate = Coordinate(20, 20)
  private val MyBoardPosInGame: ReadableProperty[Coordinate] =
    screenModel.subProp(_.canvasSize).combine(MyBoardInGameSize) {
      case (canvasSize, myBoardInGameSize) =>
        Coordinate(canvasSize.x - myBoardInGameSize * 11, myBoardInGameSize)
    }
  private val EnemyBoardPos: Coordinate = Coordinate(20, 20)

  private val PlaceShipBoardMargin = Coordinate(20, 20)

  class Image(src: String) {
    private var ready: Boolean = false

    val element: html.Image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    element.onload = (e: dom.Event) => ready = true
    element.src = src

    def isReady: Boolean = ready
  }

//  private val crosshair: Image =
//    new Image("icons/crosshair1.png")

  private val attackSimple: Image =
    new Image("icons/missile-simple.png")
//    new Image("icons/crosshairs.ai")

  val bothShipSizes: ReadableProperty[(Int, Int)] =
    DefaultSquareSize.combine(PlaceShipsSize) { case (defaultSquareSize, placeShipsSize) =>
      (defaultSquareSize, placeShipsSize)
    }

  val getAllShipsCoordinates: ReadableProperty[List[ToPlaceShip]] =
    gameModel.subProp(_.myGameState).combine(bothShipSizes) {
      case (
            Some(GameState(_, me, _, PreGameMode(shipsToPlace, _, _))),
            (defaultSquareSize, placeShipsSize)
          ) =>
        val Board(boardSize, _) = me.myBoard

        val initialPlaceShipsX =
          ((boardSize.x + 1) * (defaultSquareSize.toDouble / placeShipsSize)).toInt
        val initialPlaceShipsY =
          0

        val shipsLeftToPlace: Map[Int, Int] =
          me.shipsLeftToPlace.groupBy(_.shipId).map { case (shipId, list) => shipId -> list.size }

        val shipsListList: List[List[Ship]] =
          shipsToPlace
            .groupBy(_.shipId)
            .toList
            .sortBy { case (id, list) =>
              (-list.head.piecesSize, id)
            }
            .map(_._2)

        val shipsPlaced: Map[Int, Int] =
          shipsLeftToPlace.map { case (shipId, shipLeftToPlace) =>
            shipId -> (shipsToPlace.count(_.shipId == shipId) - shipLeftToPlace)
          }

        def drawShipsToPlace(
            posX: Int,
            posY: Int,
            ships: List[List[(Ship, Int)]],
            shipBefore: Option[Ship]
        ): List[ToPlaceShip] =
          ships match {
            case ((ship, index) :: next) :: nextList =>
              ToPlaceShip(
                ship,
                ship.pieces.map(_ + Coordinate(posX, posY)),
                index,
                shipsPlaced.getOrElse(ship.shipId, Int.MaxValue) > index
              ) ::
                drawShipsToPlace(posX + ship.size.x + 1, posY, next :: nextList, Some(ship))
            case Nil :: nextList =>
              drawShipsToPlace(
                initialPlaceShipsX,
                posY + shipBefore.map(_.size.y + 1).getOrElse(0),
                nextList,
                None
              )
            case Nil =>
              Nil
          }

        drawShipsToPlace(
          posX = initialPlaceShipsX,
          posY = initialPlaceShipsY,
          shipsListList.map(_.zipWithIndex),
          None
        )
      case _ =>
        Nil
    }

  val myBoardMouseCoordinate: ReadableProperty[Option[Coordinate]] =
    gameModel.combine(DefaultSquareSize) {
      case (
            GameModel(Some(GameState(_, me, _, PreGameMode(_, _, _))), Some(mousePosition), _),
            defaultSquareSize
          ) =>
        val relativeBoardCoor = mousePosition - AbsInitial - MyBoardPosPreGame
        Some(relativeBoardCoor)
          .filter(coor =>
            coor >= -PlaceShipBoardMargin &&
              coor <= (me.myBoard.boardSize * defaultSquareSize + PlaceShipBoardMargin)
          )
          .map(_ / defaultSquareSize)
          .map(_.roundTo(me.myBoard.boardSize))
      case _ =>
        None
    }

  val enemyBoardMouseCoordinate: ReadableProperty[Option[Coordinate]] =
    gameModel.combine(DefaultSquareSize) {
      case (
            GameModel(Some(GameState(_, _, enemy, InGameMode(_, _, _))), Some(mousePosition), _),
            defaultSquareSize
          ) =>
        val relativeBoardCoor = mousePosition - AbsInitial - EnemyBoardPos
        Some(relativeBoardCoor)
          .filter(coor =>
            coor >= -PlaceShipBoardMargin &&
              coor <= (enemy.boardSize * defaultSquareSize + PlaceShipBoardMargin)
          )
          .map(_ / defaultSquareSize)
          .map(_.roundTo(enemy.boardSize))
      case _ =>
        None
    }

  def getShipInCoor(
      shipsToPlaceCoor: List[ToPlaceShip],
      mousePosition: Coordinate
  ): Option[ToPlaceShip] = {
    val relativeBoardCoor = mousePosition - AbsInitial - MyBoardPosPreGame
    shipsToPlaceCoor
      .find { case ToPlaceShip(_, pieces, _, alreadyPlaced) =>
        !alreadyPlaced && pieces.exists(sqCoor =>
          relativeBoardCoor >= sqCoor * PlaceShipsSize.get &&
            relativeBoardCoor <= (sqCoor + Coordinate(1, 1)) * PlaceShipsSize.get
        )
      }
  }

  def drawShipSquareCoor(
      myBoardCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit =
    drawShipSquareAbsCoor(myBoardCtx, boardPosition + coor * size, size, alpha = alpha)

  def drawDarkerShipSquareCoor(
      myBoardCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit =
    drawShipSquareAbsCoor(
      myBoardCtx,
      boardPosition + coor * size,
      size,
      shipColor = "140, 74, 19",
      alpha = alpha
    )

  def drawShipRedSquareCoor(
      myBoardCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit =
    drawShipSquareAbsCoor(
      myBoardCtx,
      boardPosition + coor * size,
      size,
      shipColor = "255, 0, 0",
      alpha = alpha
    )

  def drawShipSquareAbsCoor(
      myBoardCtx: CanvasRenderingContext2D,
      coor: Coordinate,
      size: Int,
      shipColor: String = "160, 94, 39",
      alpha: Double = 1.0
  ): Unit = {
    myBoardCtx.fillStyle = s"rgb($shipColor, $alpha)"
    myBoardCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
    myBoardCtx.lineWidth = 1.0
    myBoardCtx.fillRect(AbsInitial.x + coor.x, AbsInitial.y + coor.y, size, size)
    myBoardCtx.beginPath()
    myBoardCtx.rect(AbsInitial.x + coor.x, AbsInitial.y + coor.y, size, size)
    myBoardCtx.stroke()
  }

  def drawSelectedShipSquareCoor(
      myBoardCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit = {
    val relCoor = AbsInitial + boardPosition + coor * size
    myBoardCtx.fillStyle = s"rgb(255, 0, 0, $alpha)"
    myBoardCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
    myBoardCtx.lineWidth = 3.0
    myBoardCtx.fillRect(relCoor.x, relCoor.y, size, size)
    myBoardCtx.beginPath()
    myBoardCtx.rect(relCoor.x, relCoor.y, size, size)
    myBoardCtx.stroke()
  }

  def drawWaterCoor(
      myBoardCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int
  ): Unit = {
    val relCoor = AbsInitial + boardPosition + coor * size
    myBoardCtx.fillStyle = s"rgb(0, 206, 209)"
    myBoardCtx.strokeStyle = s"rgb(0, 0, 0)"
    myBoardCtx.lineWidth = 1.0
    myBoardCtx.fillRect(relCoor.x, relCoor.y, size, size)
    myBoardCtx.beginPath()
    myBoardCtx.rect(relCoor.x, relCoor.y, size, size)
    myBoardCtx.stroke()
  }

  def drawDarkerWaterCoor(
      myBoardCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int
  ): Unit = {
    val relCoor = AbsInitial + boardPosition + coor * size
    myBoardCtx.fillStyle = s"rgb(0, 166, 169)"
    myBoardCtx.strokeStyle = s"rgb(0, 0, 0)"
    myBoardCtx.lineWidth = 1.0
    myBoardCtx.fillRect(relCoor.x, relCoor.y, size, size)
    myBoardCtx.beginPath()
    myBoardCtx.rect(relCoor.x, relCoor.y, size, size)
    myBoardCtx.stroke()
  }

  def drawTurnNumberCoor(
      myBoardCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      turnNumber: Int,
      textSize: Int
  ): Unit = {
    val relCoor = AbsInitial + boardPosition + coor * size + Coordinate(size / 2, size / 2 + 2)
    myBoardCtx.strokeStyle = s"rgb(0, 0, 0)"
    myBoardCtx.font = s"${textSize}px serif"
    myBoardCtx.textBaseline = "middle"
    myBoardCtx.textAlign = "center"
    myBoardCtx.lineWidth = 2.0
    myBoardCtx.strokeText(turnNumber.toString, relCoor.x, relCoor.y)
  }

  def paint(gameModel: GameModel): Unit = {
    val GameModel(gameState, mousePositionOpt, selectedShipOpt) = gameModel

    val myBoardCtx = myBoardCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    myBoardCtx.clearRect(0, 0, myBoardCanvas.width, myBoardCanvas.height)

    gameState match {
      case Some(GameState(_, me, enemy, gameMode)) =>
        gameMode match {
          case GameMode.PreGameMode(_, _, _) =>
            drawMyBoard(
              myBoardCtx,
              me,
              enemy,
              mousePositionOpt,
              selectedShipOpt,
              MyBoardPosPreGame,
              DefaultSquareSize.get,
              fillEmptySquares = false
            )
          case GameMode.InGameMode(isFirstPlayer, halfTurns, turnAttacks) =>
            drawMyBoard(
              myBoardCtx,
              me,
              enemy,
              None,
              None,
              MyBoardPosInGame.get,
              MyBoardInGameSize.get,
              fillEmptySquares = true
            )

            drawEnemyBoard(
              myBoardCtx,
              me,
              enemy,
              mousePositionOpt,
              turnAttacks,
              EnemyBoardPos,
              DefaultSquareSize.get
            )
        }
      case None =>
    }
  }

  def drawMyBoard(
      myBoardCtx: CanvasRenderingContext2D,
      me: Player,
      enemy: SimplePlayer,
      mousePositionOpt: Option[Coordinate],
      selectedShipOpt: Option[Ship],
      boardPosition: Coordinate,
      squareSize: Int,
      fillEmptySquares: Boolean
  ): Unit = {
    val boardSize = me.myBoard.boardSize

    drawBoardLimits(myBoardCtx, boardSize, boardPosition, squareSize)

    val hoverShipOpt: Option[ToPlaceShip] =
      mousePositionOpt.flatMap(getShipInCoor(getAllShipsCoordinates.get, _))

    val preSquareSize = PlaceShipsSize.get
    getAllShipsCoordinates.get.foreach { case ToPlaceShip(ship, pieces, _, alreadyPlaced) =>
      if (alreadyPlaced)
        pieces.foreach(
          drawShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize, alpha = 0.4)
        )
      else
        (selectedShipOpt, hoverShipOpt) match {
          case (Some(selectedShip), _) if selectedShip.shipId == ship.shipId =>
            pieces.foreach(
              drawSelectedShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize)
            )
          case (_, Some(ToPlaceShip(hoverShip, _, _, _))) if hoverShip.shipId == ship.shipId =>
            pieces.foreach(
              drawSelectedShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize)
            )
          case _ =>
            pieces.foreach(drawShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize))
        }
    }

    val water: Array[Array[Boolean]] = Array.fill(boardSize.x, boardSize.y)(fillEmptySquares)
    if (!fillEmptySquares)
      me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
        ship.pieces
          .map(_ + position)
          .foreach { case Coordinate(x, y) =>
            for (dx <- -1 to 1; dy <- -1 to 1)
              Some(Coordinate(x + dx, y + dy)).filter(_.isInside(boardSize)).foreach {
                case Coordinate(cx, cy) => water(cx)(cy) = true
              }
          }
      }

    for (x <- 0 until boardSize.x; y <- 0 until boardSize.y)
      if (water(x)(y))
        drawWaterCoor(myBoardCtx, boardPosition, Coordinate(x, y), size = squareSize)

    me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
      ship.pieces
        .map(_ + position)
        .foreach(drawShipSquareCoor(myBoardCtx, boardPosition, _, size = squareSize))
    }

    enemy.turnPlayHistory.foreach { case TurnPlay(turnNumber, turnAttacks, _) =>
      turnAttacks.flatMap(_.coordinateOpt).foreach { coor =>
        drawTurnNumberCoor(
          myBoardCtx,
          boardPosition,
          coor,
          size = squareSize,
          turnNumber,
          textSize = (squareSize * 0.6).toInt
        )
      }
    }

    (mousePositionOpt, selectedShipOpt) match {
      case (Some(mousePosition), Some(ship)) =>
        myBoardMouseCoordinate.get match {
          case Some(boardCoor) =>
            val roundedBoardCoor =
              boardCoor.roundTo(boardSize - ship.size + Coordinate(1, 1))

            val drawCoordinate: Coordinate => Unit = {
              val alpha: Double =
                if (gamePresenter.canPlace(me.myBoard, ship, roundedBoardCoor))
                  0.9
                else
                  0.75
              drawShipRedSquareCoor(
                myBoardCtx,
                boardPosition,
                _,
                size = squareSize,
                alpha = alpha
              )
            }

            ship.pieces.map(_ + roundedBoardCoor).foreach(drawCoordinate)
          case _ =>
            val center = ship.size * (squareSize / 2)

            ship.pieces
              .map(_ * squareSize + mousePosition - center)
              .foreach(drawShipSquareAbsCoor(myBoardCtx, _, size = squareSize, alpha = 0.5))
        }
      case _ =>
    }
  }

  def drawBoardLimits(
      myBoardCtx: CanvasRenderingContext2D,
      boardSize: Coordinate,
      boardPosition: Coordinate,
      squareSize: Int
  ): Unit = {
    myBoardCtx.strokeStyle = s"rgb(0, 0, 0)"

    for (x <- 0 to boardSize.x) {
      myBoardCtx.beginPath()
      myBoardCtx.moveTo(
        AbsInitial.x + boardPosition.x + x * squareSize,
        AbsInitial.y + boardPosition.y
      )
      myBoardCtx.lineTo(
        AbsInitial.x + boardPosition.x + x * squareSize,
        AbsInitial.y + boardPosition.y + boardSize.y * squareSize
      )
      myBoardCtx.stroke()
    }
    for (y <- 0 to boardSize.y) {
      myBoardCtx.beginPath()
      myBoardCtx.moveTo(
        AbsInitial.x + boardPosition.x,
        AbsInitial.y + boardPosition.y + y * squareSize
      )
      myBoardCtx.lineTo(
        AbsInitial.x + boardPosition.x + boardSize.x * squareSize,
        AbsInitial.y + boardPosition.y + y * squareSize
      )
      myBoardCtx.stroke()
    }
  }

  def drawEnemyBoard(
      myBoardCtx: CanvasRenderingContext2D,
      me: Player,
      enemy: SimplePlayer,
      mousePositionOpt: Option[Coordinate],
      turnAttacks: List[Attack],
      boardPosition: Coordinate,
      squareSize: Int
  ): Unit = {
    drawBoardLimits(myBoardCtx, enemy.boardSize, boardPosition, squareSize)

    def drawCrosshair(coordinate: Coordinate, alpha: Double): Unit = {
      myBoardCtx.strokeStyle = s"rgb(255, 0, 0, $alpha)"
      myBoardCtx.lineWidth = 2.0

      myBoardCtx.beginPath()
      myBoardCtx.moveTo(
        AbsInitial.x + boardPosition.x + coordinate.x * squareSize,
        AbsInitial.y + boardPosition.y + coordinate.y * squareSize
      )
      myBoardCtx.lineTo(
        AbsInitial.x + boardPosition.x + (coordinate.x + 1) * squareSize,
        AbsInitial.y + boardPosition.y + (coordinate.y + 1) * squareSize
      )
      myBoardCtx.stroke()

      myBoardCtx.beginPath()
      myBoardCtx.moveTo(
        AbsInitial.x + boardPosition.x + (coordinate.x + 1) * squareSize,
        AbsInitial.y + boardPosition.y + coordinate.y * squareSize
      )
      myBoardCtx.lineTo(
        AbsInitial.x + boardPosition.x + coordinate.x * squareSize,
        AbsInitial.y + boardPosition.y + (coordinate.y + 1) * squareSize
      )
      myBoardCtx.stroke()
    }

    enemyBoardMouseCoordinate.get.foreach { enemyBoardCoor =>
      if (turnAttacks.exists(!_.isPlaced) && gamePresenter.isValidCoordinateTarget(enemyBoardCoor))
        drawCrosshair(enemyBoardCoor, alpha = 0.5)
    }

    turnAttacks.foreach {
      case Attack(_, Some(coordinate)) =>
        drawCrosshair(coordinate, alpha = 1.0)
      case _ =>
    }

    turnAttacks.zipWithIndex.foreach {
      case (Attack(AttackType.Simple, coorOpt), index) =>
        if (coorOpt.nonEmpty)
          myBoardCtx.globalAlpha = 0.25
        else
          myBoardCtx.globalAlpha = 1.0

        myBoardCtx.drawImage(
          attackSimple.element,
          0,
          0,
          500,
          500,
          AbsInitial.x + boardPosition.x + enemy.boardSize.x * squareSize + squareSize,
          AbsInitial.y + boardPosition.y + index * (squareSize * 2.25),
          squareSize * 2,
          squareSize * 2
        )

        myBoardCtx.globalAlpha = 1.0
      case _ =>
    }

    me.enemyBoardMarksWithCoor.foreach { case (coor, turnNumberOpt, mark) =>
      mark match {
        case BoardMark.Empty =>
        case BoardMark.Miss =>
          drawDarkerWaterCoor(myBoardCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ShipHit =>
          drawDarkerShipSquareCoor(myBoardCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ManualWater =>
          drawWaterCoor(myBoardCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ManualShip =>
          drawShipSquareCoor(myBoardCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ManualQuestionShip  =>
        case BoardMark.ManualQuestionWater =>
      }
      turnNumberOpt.foreach { turnNumber =>
        drawTurnNumberCoor(
          myBoardCtx,
          boardPosition,
          coor,
          size = squareSize,
          turnNumber,
          textSize = (DefaultSquareSize.get * 0.6).toInt
        )
      }
    }

//    val hoverShipOpt: Option[ToPlaceShip] =
//      mousePositionOpt.flatMap(getShipInCoor(getAllShipsCoordinates.get, _))
//
//    getAllShipsCoordinates.get.foreach { case ToPlaceShip(ship, pieces, _, alreadyPlaced) =>
//      if (alreadyPlaced)
//        pieces.foreach(
//          drawShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize, alpha = 0.4)
//        )
//      else
//        (selectedShipOpt, hoverShipOpt) match {
//          case (Some(selectedShip), _) if selectedShip.shipId == ship.shipId =>
//            pieces.foreach(
//              drawSelectedShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize)
//            )
//          case (_, Some(ToPlaceShip(hoverShip, _, _, _))) if hoverShip.shipId == ship.shipId =>
//            pieces.foreach(
//              drawSelectedShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize)
//            )
//          case _ =>
//            pieces.foreach(drawShipSquareCoor(myBoardCtx, boardPosition, _, size = preSquareSize))
//        }
//    }
//
//    val water: Array[Array[Boolean]] = Array.fill(boardSize.x, boardSize.y)(false)
//    me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
//      ship.pieces
//        .map(_ + position)
//        .foreach { case Coordinate(x, y) =>
//          for (dx <- -1 to 1; dy <- -1 to 1)
//            Some(Coordinate(x + dx, y + dy)).filter(_.isInside(boardSize)).foreach {
//              case Coordinate(cx, cy) => water(cx)(cy) = true
//            }
//        }
//    }
//
//    for (x <- 0 until boardSize.x; y <- 0 until boardSize.y)
//      if (water(x)(y))
//        drawWaterCoor(myBoardCtx, boardPosition, Coordinate(x, y), size = squareSize)
//
//    me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
//      ship.pieces
//        .map(_ + position)
//        .foreach(drawShipSquareCoor(myBoardCtx, boardPosition, _, size = squareSize))
//    }
//
//    (mousePositionOpt, selectedShipOpt) match {
//      case (Some(mousePosition), Some(ship)) =>
//        myBoardMouseCoordinate.get match {
//          case Some(boardCoor) =>
//            val roundedBoardCoor =
//              boardCoor.roundTo(boardSize - ship.size + Coordinate(1, 1))
//
//            val drawCoordinate: Coordinate => Unit = {
//              val alpha: Double =
//                if (gamePresenter.canPlace(me.myBoard, ship, roundedBoardCoor))
//                  0.9
//                else
//                  0.75
//              drawShipRedSquareCoor(
//                myBoardCtx,
//                boardPosition,
//                _,
//                size = squareSize,
//                alpha = alpha
//              )
//            }
//
//            ship.pieces.map(_ + roundedBoardCoor).foreach(drawCoordinate)
//          case _ =>
//            val center = ship.size * (squareSize / 2)
//
//            ship.pieces
//              .map(_ * squareSize + mousePosition - center)
//              .foreach(drawShipSquareAbsCoor(myBoardCtx, _, size = squareSize, alpha = 0.5))
//        }
//      case _ =>
//    }
  }

}

object BoardView {

  case class ToPlaceShip(ship: Ship, pieces: List[Coordinate], index: Int, alreadyPlaced: Boolean)

  val CanvasSize: Coordinate = Coordinate(1000, 340)

}
