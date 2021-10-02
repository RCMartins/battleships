package pt.rmartins.battleships.frontend.views.game

import io.udash.{ModelProperty, ReadableProperty}
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import pt.rmartins.battleships.frontend.views.game.BoardView.ToPlaceShip
import pt.rmartins.battleships.shared.model.game.GameMode.PreGameMode
import pt.rmartins.battleships.shared.model.game._

class BoardView(
    gameModel: ModelProperty[GameModel],
    gamePresenter: GamePresenter,
    myBoardCanvas: Canvas
) {

  private val DefaultSquareSize = 30
  private val PlaceShipsSize = 15

  private val absInitial = Coordinate(1, 1)
  private val myBoardInitial = Coordinate(DefaultSquareSize, DefaultSquareSize)

  private val placeShipBoardMargin = Coordinate(DefaultSquareSize, DefaultSquareSize)

  val getAllShipsCoordinates: ReadableProperty[List[ToPlaceShip]] =
    gameModel.subProp(_.myGameState).transform {
      case Some(GameState(_, me, _, PreGameMode(shipsToPlace))) =>
        val Board(boardSize, _, _) = me.myBoard

        val initialPlaceShipsX =
          (boardSize.x + 1) * (DefaultSquareSize / PlaceShipsSize)
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
    gameModel.transform {
      case GameModel(Some(GameState(_, me, _, PreGameMode(_))), Some(mousePosition), _) =>
        val relativeBoardCoor = mousePosition - absInitial - myBoardInitial
        Some(relativeBoardCoor)
          .filter(coor =>
            coor >= -placeShipBoardMargin &&
              coor <= (me.myBoard.boardSize * DefaultSquareSize + placeShipBoardMargin)
          )
          .map(_ / DefaultSquareSize)
          .map(_.roundTo(me.myBoard.boardSize))
      case _ =>
        None
    }

  def getShipInCoor(
      shipsToPlaceCoor: List[ToPlaceShip],
      mousePosition: Coordinate
  ): Option[ToPlaceShip] = {
    val relativeBoardCoor = mousePosition - absInitial - myBoardInitial
    shipsToPlaceCoor
      .find { case ToPlaceShip(_, pieces, _, alreadyPlaced) =>
        !alreadyPlaced && pieces.exists(sqCoor =>
          relativeBoardCoor >= sqCoor * PlaceShipsSize &&
            relativeBoardCoor <= (sqCoor + Coordinate(1, 1)) * PlaceShipsSize
        )
      }
  }

  def paint(gameModel: GameModel): Unit = {
    val GameModel(gameState, mousePositionOpt, selectedShipOpt) = gameModel
//    println(s"Paint! $selectedShip")

    myBoardCanvas.setAttribute("width", "1000")
    myBoardCanvas.setAttribute(
      "height",
      (myBoardInitial.y + DefaultSquareSize * 10 + DefaultSquareSize).toString
    )

    val myBoardCtx = myBoardCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    myBoardCtx.clearRect(0, 0, myBoardCanvas.width, myBoardCanvas.height)

    def drawShipSquareCoor(coor: Coordinate, size: Int, alpha: Double = 1.0): Unit =
      drawShipSquareAbsCoor(
        myBoardInitial + coor * size,
        size,
        alpha = alpha
      )

    def drawShipRedSquareCoor(coor: Coordinate, size: Int, alpha: Double = 1.0): Unit =
      drawShipSquareAbsCoor(
        myBoardInitial + coor * size,
        size,
        shipColor = "255, 0, 0",
        alpha = alpha
      )

    def drawShipSquareAbsCoor(
        coor: Coordinate,
        size: Int,
        shipColor: String = "160, 94, 39",
        alpha: Double = 1.0
    ): Unit = {
      myBoardCtx.fillStyle = s"rgb($shipColor, $alpha)"
      myBoardCtx.fillRect(absInitial.x + coor.x, absInitial.y + coor.y, size, size)
      myBoardCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
      myBoardCtx.lineWidth = 1.0
      myBoardCtx.beginPath()
      myBoardCtx.rect(absInitial.x + coor.x, absInitial.y + coor.y, size, size)
      myBoardCtx.stroke()
    }

    def drawSelectedShipSquareCoor(coor: Coordinate, size: Int, alpha: Double = 1.0): Unit = {
      val relCoor = absInitial + myBoardInitial + coor * size
      myBoardCtx.fillStyle = s"rgb(255, 0, 0, $alpha)"
      myBoardCtx.fillRect(relCoor.x, relCoor.y, size, size)
      myBoardCtx.strokeStyle = s"rgb(0, 0, 0, $alpha)"
      myBoardCtx.lineWidth = 3.0
      myBoardCtx.beginPath()
      myBoardCtx.rect(relCoor.x, relCoor.y, size, size)
      myBoardCtx.stroke()
    }

    def drawWaterCoor(coor: Coordinate, size: Int): Unit = {
      val relCoor = absInitial + myBoardInitial + coor * size
      myBoardCtx.fillStyle = s"rgb(0, 206, 209)"
      myBoardCtx.fillRect(relCoor.x, relCoor.y, size, size)
      myBoardCtx.strokeStyle = s"rgb(0, 0, 0)"
      myBoardCtx.lineWidth = 1.0
      myBoardCtx.beginPath()
      myBoardCtx.rect(relCoor.x, relCoor.y, size, size)
      myBoardCtx.stroke()
    }

    gameState match {
      case Some(GameState(_, me, _, gameMode)) =>
        gameMode match {
          case GameMode.PreGameMode(_) =>
            val boardSize = me.myBoard.boardSize

            for (x <- 0 to boardSize.x) {
              myBoardCtx.moveTo(
                absInitial.x + myBoardInitial.x + x * DefaultSquareSize,
                absInitial.y + myBoardInitial.y
              )
              myBoardCtx.lineTo(
                absInitial.x + myBoardInitial.x + x * DefaultSquareSize,
                absInitial.y + myBoardInitial.y + boardSize.y * DefaultSquareSize
              )
              myBoardCtx.stroke()
            }
            for (y <- 0 to boardSize.y) {
              myBoardCtx.moveTo(
                absInitial.x + myBoardInitial.x,
                absInitial.y + myBoardInitial.y + y * DefaultSquareSize
              )
              myBoardCtx.lineTo(
                absInitial.x + myBoardInitial.x + boardSize.x * DefaultSquareSize,
                absInitial.y + myBoardInitial.y + y * DefaultSquareSize
              )
              myBoardCtx.stroke()
            }

            val hoverShipOpt: Option[ToPlaceShip] =
              mousePositionOpt.flatMap(getShipInCoor(getAllShipsCoordinates.get, _))

            getAllShipsCoordinates.get.foreach { case ToPlaceShip(ship, pieces, _, alreadyPlaced) =>
              if (alreadyPlaced)
                pieces.foreach(drawShipSquareCoor(_, size = PlaceShipsSize, alpha = 0.4))
              else
                (selectedShipOpt, hoverShipOpt) match {
                  case (Some(selectedShip), _) if selectedShip.shipId == ship.shipId =>
                    pieces.foreach(drawSelectedShipSquareCoor(_, size = PlaceShipsSize))
                  case (_, Some(ToPlaceShip(hoverShip, _, _, _)))
                      if hoverShip.shipId == ship.shipId =>
                    pieces.foreach(drawSelectedShipSquareCoor(_, size = PlaceShipsSize))
                  case _ =>
                    pieces.foreach(drawShipSquareCoor(_, size = PlaceShipsSize))
                }
            }

            val water: Array[Array[Boolean]] = Array.fill(boardSize.x, boardSize.y)(false)

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
                drawWaterCoor(Coordinate(x, y), size = DefaultSquareSize)

            me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
              ship.pieces
                .map(_ + position)
                .foreach(drawShipSquareCoor(_, size = DefaultSquareSize))
            }

            (mousePositionOpt, selectedShipOpt) match {
              case (Some(mousePosition), Some(ship)) =>
                myBoardMouseCoordinate.get match {
                  case Some(boardCoor) =>
                    val roundedBoardCoor =
                      boardCoor.roundTo(boardSize - ship.size + Coordinate(1, 1))

                    val drawCoordinate: Coordinate => Unit =
                      if (gamePresenter.canPlace(me.myBoard, ship, roundedBoardCoor))
                        drawShipSquareCoor(_, size = DefaultSquareSize, alpha = 0.9)
                      else
                        drawShipRedSquareCoor(_, size = DefaultSquareSize, alpha = 0.75)

                    ship.pieces.map(_ + roundedBoardCoor).foreach(drawCoordinate)
                  case _ =>
                    val center = ship.size * (DefaultSquareSize / 2)

                    ship.pieces
                      .map(_ * DefaultSquareSize + mousePosition - center)
                      .foreach(drawShipSquareAbsCoor(_, size = DefaultSquareSize, alpha = 0.5))
                }
              case _ =>
            }
          case GameMode.InGameMode(firstPlayerUsername, halfTurns) =>
        }
      case None =>
    }

  }

}

object BoardView {

  case class ToPlaceShip(ship: Ship, pieces: List[Coordinate], index: Int, alreadyPlaced: Boolean)

}
