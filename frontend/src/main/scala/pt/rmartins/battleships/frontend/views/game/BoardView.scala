package pt.rmartins.battleships.frontend.views.game

import io.udash.{ModelProperty, ReadableProperty}
import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.{CanvasRenderingContext2D, html}
import pt.rmartins.battleships.frontend.views.game.BoardView.ToPlaceShip
import pt.rmartins.battleships.frontend.views.game.PropertiesUtils.combine
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game._

class BoardView(
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    gamePresenter: GamePresenter,
    myBoardCanvas: Canvas
) {

  // TODO * 10 hardcoded everywhere... instead of a real (Max Board Width/Height) / boardSize

  val AbsMargin: Coordinate = Coordinate(1, 1)

  private val SquareSizeBig: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform(size => if (size.x < 680) 20 else 30)
  private val SquareSizeMedium: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform(size => if (size.x < 680) 15 else 20)
  private val SquareSizeSmall: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform(size => if (size.x < 680) 12 else 15)

  private val BothShipSizes: ReadableProperty[(Int, Int)] = // TODO find better name
    combine(SquareSizeBig, SquareSizeSmall)

  private val EnemyBoardSqSize = SquareSizeBig
  private val EnemyBoardMargin = SquareSizeMedium

  private val MyBoardPosPreGame: ReadableProperty[Coordinate] =
    SquareSizeMedium.transform(size => AbsMargin + Coordinate(size, size))
  private val MyBoardPosInGame: ReadableProperty[Coordinate] =
    screenModel.subProp(_.canvasSize).combine(SquareSizeMedium) {
      case (canvasSize, myBoardInGameSize) =>
        AbsMargin + Coordinate(canvasSize.x - myBoardInGameSize * 11, myBoardInGameSize)
    }
  private val MyBoardPosGameOver: ReadableProperty[Coordinate] =
    combine(screenModel.subProp(_.canvasSize), SquareSizeBig, SquareSizeMedium)
      .transform { case (canvasSize, myBoardGameOverSize, marginSize) =>
        Coordinate(canvasSize.x - myBoardGameOverSize * 10 - marginSize, marginSize)
      }
  private val EnemyBoardPos: ReadableProperty[Coordinate] =
    EnemyBoardMargin.transform(size => AbsMargin + Coordinate(size, size))

  private val BoardMarksSelectorPos: ReadableProperty[Coordinate] =
    combine(EnemyBoardPos, EnemyBoardSqSize).transform { case (enemyBoardPos, enemyBoardSize) =>
      enemyBoardPos + Coordinate(0, enemyBoardSize * 10 + enemyBoardSize)
    }
  private val BoardMarksSelectorSize = SquareSizeBig
  private val BoardMarksSelectorMargin = SquareSizeBig.transform(_ / 2)
  private val BoardMarksSelectorCombined: ReadableProperty[(Coordinate, Int, Int)] =
    combine(BoardMarksSelectorPos, BoardMarksSelectorSize, BoardMarksSelectorMargin)

  private val BoardMarksSelectorOrder: List[BoardMark] =
    List(BoardMark.Empty, BoardMark.ManualShip, BoardMark.ManualWater)

  private val PlaceShipBoardMargin = Coordinate.square(20)

  private class Image(src: String) {
//    private var ready: Boolean = false

    val element: html.Image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
//    element.onload = (e: dom.Event) => ready = true
    element.src = src

//    def isReady: Boolean = ready
  }

  private val attackSimple: Image =
    new Image("icons/missile-simple.png")

  val getAllShipsCoordinates: ReadableProperty[List[ToPlaceShip]] =
    gamePresenter.gameStateProperty.combine(BothShipSizes) {
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
    combine(
      gameModel.subProp(_.mousePosition),
      gamePresenter.gameStateProperty,
      SquareSizeBig,
      MyBoardPosPreGame
    ).transform {
      case (
            Some(mousePosition),
            Some(GameState(_, me, _, PreGameMode(_, _, _))),
            defaultSquareSize,
            myBoardPosPreGame
          ) =>
        val relativeBoardCoor = mousePosition - myBoardPosPreGame
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
    combine(
      gameModel.subProp(_.mousePosition),
      gamePresenter.gameStateProperty,
      EnemyBoardPos,
      EnemyBoardSqSize
    ).transform {
      case (
            Some(mousePosition),
            Some(GameState(_, _, enemy, InGameMode(_, _, _))),
            enemyBoardPos,
            defaultSquareSize
          ) =>
        val relativeBoardCoor = mousePosition - enemyBoardPos
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

  private val BoardMarksSelectorAllPositions: ReadableProperty[List[(BoardMark, Coordinate)]] =
    combine(
      gamePresenter.inGameModeProperty,
      BoardMarksSelectorCombined
    ).transform {
      case (
            Some(InGameMode(_, _, _)),
            (boardMarksSelectorPos, boardMarksSelectorSize, boardMarksSelectorMargin)
          ) =>
        BoardMarksSelectorOrder.zipWithIndex
          .map { case (boardMark, index) =>
            (
              boardMark,
              boardMarksSelectorPos +
                Coordinate(index * (boardMarksSelectorSize + boardMarksSelectorMargin), 0)
            )
          }
      case _ =>
        Nil
    }

  val boardMarkHover: ReadableProperty[Option[BoardMark]] =
    combine(
      gameModel.subProp(_.mousePosition),
      gamePresenter.inGameModeProperty,
      BoardMarksSelectorAllPositions,
      BoardMarksSelectorSize
    ).transform {
      case (
            Some(mousePosition),
            Some(InGameMode(_, _, _)),
            boardMarksSelectorAllPositions,
            boardMarksSelectorSize
          ) =>
        boardMarksSelectorAllPositions
          .find { case (_, position) =>
            mousePosition >= position &&
              mousePosition <= position + Coordinate.square(boardMarksSelectorSize)
          }
          .map(_._1)
      case _ =>
        None
    }

  def getShipInCoor(
      shipsToPlaceCoor: List[ToPlaceShip],
      mousePosition: Coordinate
  ): Option[ToPlaceShip] = {
    val relativeBoardCoor = mousePosition - MyBoardPosPreGame.get
    shipsToPlaceCoor
      .find { case ToPlaceShip(_, pieces, _, alreadyPlaced) =>
        !alreadyPlaced && pieces.exists(sqCoor =>
          relativeBoardCoor >= sqCoor * SquareSizeSmall.get &&
            relativeBoardCoor <= (sqCoor + Coordinate(1, 1)) * SquareSizeSmall.get
        )
      }
  }

  def drawShipSquareCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit =
    drawShipSquareAbsCoor(renderingCtx, boardPosition + coor * size, size, alpha = alpha)

  def drawDarkerShipSquareCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit =
    drawShipSquareAbsCoor(
      renderingCtx,
      boardPosition + coor * size,
      size,
      shipColor = "140, 74, 19",
      alpha = alpha
    )

  def drawShipRedSquareCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit =
    drawShipSquareAbsCoor(
      renderingCtx,
      boardPosition + coor * size,
      size,
      shipColor = "255, 0, 0",
      alpha = alpha
    )

  def drawShipSquareAbsCoor(
      renderingCtx: CanvasRenderingContext2D,
      coor: Coordinate,
      size: Int,
      shipColor: String = "160, 94, 39",
      alpha: Double = 1.0
  ): Unit = {
    renderingCtx.fillStyle = s"rgb($shipColor, $alpha)"
    renderingCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
    renderingCtx.lineWidth = 1.0
    renderingCtx.fillRect(coor.x, coor.y, size, size)
    renderingCtx.beginPath()
    renderingCtx.rect(coor.x, coor.y, size, size)
    renderingCtx.stroke()
  }

  def drawSelectedShipSquareCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit = {
    val relCoor = boardPosition + coor * size
    renderingCtx.fillStyle = s"rgb(255, 0, 0, $alpha)"
    renderingCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
    renderingCtx.lineWidth = 3.0
    renderingCtx.fillRect(relCoor.x, relCoor.y, size, size)
    renderingCtx.beginPath()
    renderingCtx.rect(relCoor.x, relCoor.y, size, size)
    renderingCtx.stroke()
  }

  def drawEmptyCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit = {
    val relCoor = boardPosition + coor * size
    renderingCtx.fillStyle = s"rgb(255, 255, 255, $alpha)"
    renderingCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
    renderingCtx.lineWidth = 1.0
    renderingCtx.fillRect(relCoor.x, relCoor.y, size, size)
    renderingCtx.beginPath()
    renderingCtx.rect(relCoor.x, relCoor.y, size, size)
    renderingCtx.stroke()
  }

  def drawWaterCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit = {
    val relCoor = boardPosition + coor * size
    renderingCtx.fillStyle = s"rgb(0, 206, 209, $alpha)"
    renderingCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
    renderingCtx.lineWidth = 1.0
    renderingCtx.fillRect(relCoor.x, relCoor.y, size, size)
    renderingCtx.beginPath()
    renderingCtx.rect(relCoor.x, relCoor.y, size, size)
    renderingCtx.stroke()
  }

  def drawDarkerWaterCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int
  ): Unit = {
    val relCoor = boardPosition + coor * size
    renderingCtx.fillStyle = s"rgb(0, 166, 169)"
    renderingCtx.strokeStyle = s"rgb(0, 0, 0)"
    renderingCtx.lineWidth = 1.0
    renderingCtx.fillRect(relCoor.x, relCoor.y, size, size)
    renderingCtx.beginPath()
    renderingCtx.rect(relCoor.x, relCoor.y, size, size)
    renderingCtx.stroke()
  }

  def drawTurnNumberCoor(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      turnNumber: Int,
      textSize: Int
  ): Unit = {
    val relCoor = boardPosition + coor * size + Coordinate(size / 2, size / 2 + 2)
    renderingCtx.strokeStyle = s"rgb(0, 0, 0)"
    renderingCtx.font = s"${textSize}px serif"
    renderingCtx.textBaseline = "middle"
    renderingCtx.textAlign = "center"
    renderingCtx.lineWidth = 2.0
    renderingCtx.strokeText(turnNumber.toString, relCoor.x, relCoor.y)
  }

  def drawSquareRedBorder(
      renderingCtx: CanvasRenderingContext2D,
      boardPosition: Coordinate,
      coor: Coordinate,
      size: Int,
      alpha: Double = 1.0
  ): Unit = {
    val relCoor = boardPosition + coor * size
    renderingCtx.strokeStyle = s"rgb(255, 0, 0, $alpha)"
    renderingCtx.lineWidth = 3.0
    renderingCtx.beginPath()
    renderingCtx.rect(relCoor.x, relCoor.y, size, size)
    renderingCtx.stroke()
  }

  def paint(): Unit = {
    val GameModel(mousePositionOpt, selectedShipOpt, turnAttacks, _, selectedBoardMarkOpt) =
      gameModel.get

    val renderingCtx = myBoardCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    renderingCtx.clearRect(0, 0, myBoardCanvas.width, myBoardCanvas.height)

    gamePresenter.gameStateProperty.get match {
      case Some(GameState(_, me, enemy, gameMode)) =>
        gameMode match {
          case PreGameMode(_, _, _) =>
            drawMyBoard(
              renderingCtx,
              me,
              enemy,
              mousePositionOpt,
              selectedShipOpt,
              MyBoardPosPreGame.get,
              SquareSizeBig.get,
              fillEmptySquares = false
            )
          case InGameMode(_, _, _) =>
            drawMyBoard(
              renderingCtx,
              me,
              enemy,
              None,
              None,
              MyBoardPosInGame.get,
              SquareSizeMedium.get,
              fillEmptySquares = true
            )

            drawEnemyBoard(
              renderingCtx,
              me,
              enemy,
              turnAttacks,
              EnemyBoardPos.get,
              EnemyBoardSqSize.get,
              selectedBoardMarkOpt
            )

            drawBoardMarksSelector(
              renderingCtx,
              selectedBoardMarkOpt
            )
          case GameOverMode(_, _) =>
            drawMyBoard(
              renderingCtx,
              me,
              enemy,
              None,
              None,
              MyBoardPosGameOver.get,
              SquareSizeBig.get,
              fillEmptySquares = true
            )

            drawEnemyBoard(
              renderingCtx,
              me,
              enemy,
              Nil,
              EnemyBoardPos.get,
              EnemyBoardSqSize.get,
              selectedBoardMarkOpt
            )
        }
      case None =>
    }
  }

  def drawMyBoard(
      renderingCtx: CanvasRenderingContext2D,
      me: Player,
      enemy: SimplePlayer,
      mousePositionOpt: Option[Coordinate],
      selectedShipOpt: Option[Ship],
      boardPosition: Coordinate,
      squareSize: Int,
      fillEmptySquares: Boolean
  ): Unit = {
    val boardSize = me.myBoard.boardSize

    drawBoardLimits(renderingCtx, "My board", boardSize, boardPosition, squareSize)

    val hoverShipOpt: Option[ToPlaceShip] =
      mousePositionOpt.flatMap(getShipInCoor(getAllShipsCoordinates.get, _))

    val preSquareSize = SquareSizeSmall.get
    getAllShipsCoordinates.get.foreach { case ToPlaceShip(ship, pieces, _, alreadyPlaced) =>
      if (alreadyPlaced)
        pieces.foreach(
          drawShipSquareCoor(renderingCtx, boardPosition, _, size = preSquareSize, alpha = 0.4)
        )
      else
        (selectedShipOpt, hoverShipOpt) match {
          case (Some(selectedShip), _) if selectedShip.shipId == ship.shipId =>
            pieces.foreach(
              drawSelectedShipSquareCoor(renderingCtx, boardPosition, _, size = preSquareSize)
            )
          case (_, Some(ToPlaceShip(hoverShip, _, _, _))) if hoverShip.shipId == ship.shipId =>
            pieces.foreach(
              drawSelectedShipSquareCoor(renderingCtx, boardPosition, _, size = preSquareSize)
            )
          case _ =>
            pieces.foreach(drawShipSquareCoor(renderingCtx, boardPosition, _, size = preSquareSize))
        }
    }

    val water: Array[Array[Boolean]] = Array.fill(boardSize.x, boardSize.y)(fillEmptySquares)
    if (!fillEmptySquares)
      me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
        ship.pieces
          .map(_ + position)
          .foreach { case Coordinate(x, y) =>
            for (dx <- -1 to 1; dy <- -1 to 1)
              Some(Coordinate(x + dx, y + dy)).filter(_.isInsideBoard(boardSize)).foreach {
                case Coordinate(cx, cy) => water(cx)(cy) = true
              }
          }
      }

    for (x <- 0 until boardSize.x; y <- 0 until boardSize.y)
      if (water(x)(y))
        drawWaterCoor(renderingCtx, boardPosition, Coordinate(x, y), size = squareSize)

    me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
      ship.pieces
        .map(_ + position)
        .foreach(drawShipSquareCoor(renderingCtx, boardPosition, _, size = squareSize))
    }

    enemy.turnPlayHistory.foreach { case TurnPlay(turnNumber, turnAttacks, _) =>
      turnAttacks.flatMap(_.coordinateOpt).foreach { coor =>
        drawTurnNumberCoor(
          renderingCtx,
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
                renderingCtx,
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
              .foreach(drawShipSquareAbsCoor(renderingCtx, _, size = squareSize, alpha = 0.5))
        }
      case _ =>
    }
  }

  def drawBoardLimits(
      renderingCtx: CanvasRenderingContext2D,
      boardTitle: String,
      boardSize: Coordinate,
      boardPosition: Coordinate,
      squareSize: Int
  ): Unit = {
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

    val fontSize = Math.max(11, squareSize / 2)
    renderingCtx.strokeStyle = s"rgb(0, 0, 0)"
    renderingCtx.font = s"${fontSize}px serif"
    renderingCtx.textBaseline = "bottom"
    renderingCtx.textAlign = "left"
    renderingCtx.lineWidth = 1.0
    renderingCtx.strokeText(
      boardTitle,
      boardPosition.x,
      boardPosition.y - 2
    )
  }

  def drawEnemyBoard(
      renderingCtx: CanvasRenderingContext2D,
      me: Player,
      enemy: SimplePlayer,
      turnAttacks: List[Attack],
      boardPosition: Coordinate,
      squareSize: Int,
      selectedBoardMarkOpt: Option[BoardMark]
  ): Unit = {
    drawBoardLimits(renderingCtx, "Enemy board", enemy.boardSize, boardPosition, squareSize)

    def drawCrosshair(coordinate: Coordinate, alpha: Double): Unit = {
      renderingCtx.strokeStyle = s"rgb(255, 0, 0, $alpha)"
      renderingCtx.lineWidth = 2.0

      renderingCtx.beginPath()
      renderingCtx.moveTo(
        boardPosition.x + coordinate.x * squareSize,
        boardPosition.y + coordinate.y * squareSize
      )
      renderingCtx.lineTo(
        boardPosition.x + (coordinate.x + 1) * squareSize,
        boardPosition.y + (coordinate.y + 1) * squareSize
      )
      renderingCtx.stroke()

      renderingCtx.beginPath()
      renderingCtx.moveTo(
        boardPosition.x + (coordinate.x + 1) * squareSize,
        boardPosition.y + coordinate.y * squareSize
      )
      renderingCtx.lineTo(
        boardPosition.x + coordinate.x * squareSize,
        boardPosition.y + (coordinate.y + 1) * squareSize
      )
      renderingCtx.stroke()
    }

    turnAttacks.zipWithIndex.foreach {
      case (Attack(AttackType.Simple, coorOpt), index) =>
        if (coorOpt.nonEmpty)
          renderingCtx.globalAlpha = 0.25
        else
          renderingCtx.globalAlpha = 1.0

        renderingCtx.drawImage(
          attackSimple.element,
          0,
          0,
          500,
          500,
          boardPosition.x + enemy.boardSize.x * squareSize + squareSize * 0.5,
          boardPosition.y + index * (squareSize * 2.25),
          squareSize * 2,
          squareSize * 2
        )

        renderingCtx.globalAlpha = 1.0
      case _ =>
    }

    val boardMarksWithCoor: Seq[(Coordinate, Option[Int], BoardMark)] =
      me.enemyBoardMarksWithCoor

    boardMarksWithCoor.foreach { case (coor, turnNumberOpt, mark) =>
      mark match {
        case BoardMark.Empty =>
        case BoardMark.Miss =>
          drawDarkerWaterCoor(renderingCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ShipHit =>
          drawDarkerShipSquareCoor(renderingCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ManualWater =>
          drawWaterCoor(renderingCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ManualShip =>
          drawShipSquareCoor(renderingCtx, boardPosition, coor, size = squareSize)
        case BoardMark.ManualQuestionShip  =>
        case BoardMark.ManualQuestionWater =>
      }
      turnNumberOpt.foreach { turnNumber =>
        drawTurnNumberCoor(
          renderingCtx,
          boardPosition,
          coor,
          size = squareSize,
          turnNumber,
          textSize = (SquareSizeBig.get * 0.6).toInt
        )
      }
    }

    enemyBoardMouseCoordinate.get.foreach { enemyBoardCoor =>
      selectedBoardMarkOpt match {
        case Some(boardMark) if !boardMarksWithCoor.exists { case (coor, _, currentBoardMark) =>
              coor == enemyBoardCoor && currentBoardMark.isPermanent
            } =>
          boardMark match {
            case BoardMark.Empty =>
              drawEmptyCoor(
                renderingCtx,
                boardPosition,
                enemyBoardCoor,
                squareSize,
                alpha = 0.5
              )
            case BoardMark.ManualWater =>
              drawWaterCoor(
                renderingCtx,
                boardPosition,
                enemyBoardCoor,
                squareSize,
                alpha = 0.5
              )
            case BoardMark.ManualShip =>
              drawShipSquareCoor(
                renderingCtx,
                boardPosition,
                enemyBoardCoor,
                squareSize,
                alpha = 0.5
              )
            case _ =>
          }
        case None
            if turnAttacks.exists(!_.isPlaced) &&
              gamePresenter.isValidCoordinateTarget(enemyBoardCoor) =>
          drawCrosshair(enemyBoardCoor, alpha = 0.5)
        case _ =>
      }
    }

    turnAttacks.foreach {
      case Attack(_, Some(coordinate)) =>
        drawCrosshair(coordinate, alpha = 1.0)
      case _ =>
    }
  }

  def drawBoardMarksSelector(
      renderingCtx: CanvasRenderingContext2D,
      selectedBoardMarkOpt: Option[BoardMark]
  ): Unit = {
    val boardMarksPos = BoardMarksSelectorPos.get
    val boardMarksSize = BoardMarksSelectorSize.get
    val boardMarksMargin = BoardMarksSelectorMargin.get

    val selected = selectedBoardMarkOpt.orElse(boardMarkHover.get)

    BoardMarksSelectorAllPositions.get.foreach { case (boardMark, position) =>
      boardMark match {
        case BoardMark.Empty =>
          drawEmptyCoor(renderingCtx, position, Coordinate.origin, boardMarksSize)
        case BoardMark.ManualWater =>
          drawWaterCoor(renderingCtx, position, Coordinate.origin, boardMarksSize)
        case BoardMark.ManualShip =>
          drawShipSquareCoor(renderingCtx, position, Coordinate.origin, boardMarksSize)
        case _ =>
      }
      if (selected.contains(boardMark))
        drawSquareRedBorder(renderingCtx, position, Coordinate.origin, boardMarksSize)
    }
  }

}

object BoardView {

  case class ToPlaceShip(ship: Ship, pieces: List[Coordinate], index: Int, alreadyPlaced: Boolean)

  val CanvasSize: Coordinate = Coordinate(1000, 400)

}
