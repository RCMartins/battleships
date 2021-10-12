package pt.rmartins.battleships.frontend.views.game

import io.udash.{ModelProperty, ReadableProperty}
import org.scalajs.dom
import org.scalajs.dom.html.{Canvas, Span}
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.{CanvasRenderingContext2D, html}
import pt.rmartins.battleships.frontend.views.game.BoardView._
import pt.rmartins.battleships.frontend.views.game.CanvasUtils._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, InGameMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game.HitHint.ShipHit
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.Utils.canPlaceInBoard

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

  private val MyBoardPreGameSqSize = SquareSizeBig
  private val MyBoardInGameSqSize = SquareSizeMedium
  private val MyBoardGameOverSqSize = SquareSizeBig
  private val MyBoardMargin = SquareSizeMedium

  private val EnemyBoardSqSize = SquareSizeBig
  private val EnemyBoardMargin = SquareSizeMedium

  private val MyBoardPreGamePos: ReadableProperty[Coordinate] =
    MyBoardMargin.transform(size => AbsMargin + Coordinate(size, size))
  private val MyBoardInGamePos: ReadableProperty[Coordinate] =
    screenModel.subProp(_.canvasSize).combine(MyBoardInGameSqSize) {
      case (canvasSize, myBoardInGameSize) =>
        AbsMargin + Coordinate(canvasSize.x - myBoardInGameSize * 11, myBoardInGameSize)
    }
  private val MyBoardGameOverPos: ReadableProperty[Coordinate] =
    combine(screenModel.subProp(_.canvasSize), MyBoardGameOverSqSize, MyBoardMargin)
      .transform { case (canvasSize, myBoardGameOverSqSize, myBoardMargin) =>
        Coordinate(canvasSize.x - myBoardGameOverSqSize * 10 - myBoardMargin, myBoardMargin)
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

  private val MissilesInicialPos: ReadableProperty[Coordinate] =
    combine(EnemyBoardPos, SquareSizeBig).transform { case (enemyBoardPos, squareSizeBig) =>
      enemyBoardPos + Coordinate(10 * squareSizeBig + squareSizeBig / 2, 0)
    }
  private val MissilesSqSize: ReadableProperty[Int] =
    SquareSizeBig.transform(squareSizeBig => squareSizeBig * 2)

  private val MissilesPosDiff: ReadableProperty[Coordinate] =
    combine(MissilesSqSize, SquareSizeBig).transform { case (missilesSqSize, squareSizeBig) =>
      Coordinate(0, (missilesSqSize + squareSizeBig * 0.25).toInt)
    }

  private val PlaceShipsPos: ReadableProperty[Coordinate] =
    combine(MyBoardPreGamePos, MyBoardPreGameSqSize).transform {
      case (myBoardPreGamePos, myBoardPreGameSqSize) =>
        myBoardPreGamePos +
          Coordinate(10 * myBoardPreGameSqSize + myBoardPreGameSqSize, 0)
    }

  private val PlaceShipsSqSize: ReadableProperty[Int] = SquareSizeSmall

  private val DestructionSummaryPos: ReadableProperty[Coordinate] =
    combine(MissilesInicialPos, MissilesSqSize, SquareSizeBig).transform {
      case (missilesPos, missilesSize, squareSizeBig) =>
        missilesPos + Coordinate(missilesSize + squareSizeBig, 0)
    }
  private val DestructionSummarySqSize: ReadableProperty[Int] = SquareSizeSmall

  private val PlaceShipBoardMargin = Coordinate.square(20)

  private class Image(src: String) {
    val element: html.Image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    element.src = src
  }

  private val attackSimple: Image =
    new Image("icons/missile-simple.png")

  private val shipsSummaryRelCoordinates: ReadableProperty[List[(Int, List[ViewShip])]] =
    gamePresenter.rulesProperty.transform {
      case Some(Rules(shipsInThisGame, _, _, _)) =>
        def getShipsToPlacePos(
            posX: Int,
            posY: Int,
            ships: List[List[Ship]],
            currentList: List[ViewShip],
            shipBefore: Option[Ship]
        ): List[List[ViewShip]] =
          ships match {
            case (ship :: next) :: nextList =>
              getShipsToPlacePos(
                posX + ship.size.x + 1,
                posY,
                next :: nextList,
                ViewShip(ship, ship.pieces.map(_ + Coordinate(posX, posY))) :: currentList,
                Some(ship)
              )
            case Nil :: nextList =>
              currentList.reverse ::
                getShipsToPlacePos(
                  0,
                  posY + shipBefore.map(_.size.y + 1).getOrElse(0),
                  nextList,
                  Nil,
                  None
                )
            case Nil if currentList.nonEmpty =>
              currentList.reverse :: Nil
            case Nil =>
              Nil
          }

        val shipsListList: List[List[Ship]] =
          shipsInThisGame
            .groupBy(_.shipId)
            .toList
            .sortBy { case (id, list) =>
              (-list.head.piecesSize, id)
            }
            .map(_._2)

        getShipsToPlacePos(
          posX = 0,
          posY = 0,
          shipsListList,
          Nil,
          None
        ).map { viewShipList => viewShipList.head.ship.shipId -> viewShipList }
      case _ =>
        Nil
    }

  private val allShipsToPlaceCoordinates: ReadableProperty[List[ToPlaceShip]] =
    combine(
      shipsSummaryRelCoordinates,
      gamePresenter.meProperty.transform(_.map(_.shipsLeftToPlace)),
      gamePresenter.gameModeProperty,
      PlaceShipsPos,
      PlaceShipsSqSize
    ).transform {
      case (
            shipsSummary,
            Some(shipsLeftToPlace),
            Some(PreGameMode(_, _)),
            placeShipsPos,
            placeShipsSqSize
          ) =>
        val shipsLeftToPlaceMap: Map[Int, Int] =
          shipsLeftToPlace.groupBy(_.shipId).map { case (shipId, list) => shipId -> list.size }

        val shipsPlaced: Map[Int, Int] =
          shipsLeftToPlaceMap.map { case (shipId, shipLeftToPlace) =>
            shipId ->
              shipsSummary
                .find(_._1 == shipId)
                .map(_._2.size - shipLeftToPlace)
                .getOrElse(0)
          }

        shipsSummary.flatMap { case (shipId, viewShipList) =>
          viewShipList.zipWithIndex.map { case (viewShip, index) =>
            ToPlaceShip(
              viewShip.ship,
              viewShip.pieces.map(relPieceCoor => placeShipsPos + relPieceCoor * placeShipsSqSize),
              shipsPlaced.getOrElse(shipId, Int.MaxValue) > index
            )
          }
        }
      case _ =>
        Nil
    }

  val shipToPlaceHover: ReadableProperty[Option[ToPlaceShip]] =
    combine(gamePresenter.mousePositionProperty, allShipsToPlaceCoordinates, PlaceShipsSqSize)
      .transform {
        case (Some(mousePosition), shipsSummary, placeShipsSqSize) =>
          val sizeCoor = Coordinate.square(placeShipsSqSize)
          shipsSummary.find { case ToPlaceShip(_, pieces, alreadyPlaced) =>
            !alreadyPlaced && pieces.exists(sqCoor =>
              mousePosition >= sqCoor && mousePosition <= sqCoor + sizeCoor
            )
          }
        case _ =>
          None
      }

  val allShipsSummaryCoordinates: ReadableProperty[List[SummaryShip]] =
    combine(
      shipsSummaryRelCoordinates,
      gamePresenter.meProperty.transform(_.map(_.turnPlayHistory)),
      gamePresenter.gameModeProperty,
      DestructionSummaryPos,
      DestructionSummarySqSize
    ).transform {
      case (
            shipsSummary,
            Some(turnPlayHistory),
            Some(InGameMode(_, _, _, _, _) | GameOverMode(_, _, _, _)),
            destructionSummaryPos,
            destructionSummarySqSize
          ) =>
        val shipsDestroyed: Map[Int, Int] =
          turnPlayHistory
            .flatMap(_.hitHints.collect { case ShipHit(shipId, true) => shipId })
            .groupBy(identity)
            .map { case (shipId, list) => shipId -> list.size }

        shipsSummary.flatMap { case (shipId, viewShipList) =>
          viewShipList.zipWithIndex.map { case (viewShip, index) =>
            SummaryShip(
              viewShip.ship,
              viewShip.pieces.map(relPieceCoor =>
                destructionSummaryPos + relPieceCoor * destructionSummarySqSize
              ),
              shipsDestroyed.getOrElse(shipId, 0) > index
            )
          }
        }
      case _ =>
        Nil
    }

  val myBoardMouseCoordinate: ReadableProperty[Option[Coordinate]] =
    combine(
      gamePresenter.mousePositionProperty,
      gamePresenter.meProperty.transform(_.map(_.myBoard.boardSize)),
      gamePresenter.gameModeProperty,
      SquareSizeBig,
      MyBoardPreGamePos
    ).transform {
      case (
            Some(mousePosition),
            Some(boardSize),
            Some(PreGameMode(_, _)),
            defaultSquareSize,
            myBoardPosPreGame
          ) =>
        val relativeBoardCoor = mousePosition - myBoardPosPreGame
        Some(relativeBoardCoor)
          .filter(coor =>
            coor >= -PlaceShipBoardMargin &&
              coor <= (boardSize * defaultSquareSize + PlaceShipBoardMargin)
          )
          .map(_ / defaultSquareSize)
          .map(_.roundTo(boardSize))
      case _ =>
        None
    }

  val enemyBoardMouseCoordinate: ReadableProperty[Option[Coordinate]] =
    combine(
      gamePresenter.mousePositionProperty,
      gamePresenter.gameStateProperty, // TODO improve
      EnemyBoardPos,
      EnemyBoardSqSize
    ).transform {
      case (
            Some(mousePosition),
            Some(GameState(_, _, _, enemy, InGameMode(_, _, _, _, _) | GameOverMode(_, _, _, _))),
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
      gamePresenter.gameModeProperty,
      BoardMarksSelectorCombined
    ).transform {
      case (
            Some(InGameMode(_, _, _, _, _) | GameOverMode(_, _, _, _)),
            (boardMarksSelectorPos, boardMarksSelectorSize, boardMarksSelectorMargin)
          ) =>
        BoardMarksSelectorOrder.zipWithIndex.map { case (boardMark, index) =>
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
      gamePresenter.mousePositionProperty,
      gamePresenter.gameModeProperty,
      BoardMarksSelectorAllPositions,
      BoardMarksSelectorSize
    ).transform {
      case (
            Some(mousePosition),
            Some(InGameMode(_, _, _, _, _) | GameOverMode(_, _, _, _)),
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

  def paint(): Unit = {
    val GameModel(mousePositionOpt, _, selectedShipOpt, turnAttacks, _, selectedBoardMarkOpt) =
      gameModel.get

    val renderingCtx = myBoardCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    renderingCtx.clearRect(0, 0, myBoardCanvas.width, myBoardCanvas.height)

    gamePresenter.gameStateProperty.get match {
      case Some(GameState(_, _, me, enemy, PreGameMode(_, _))) =>
        drawMyBoard(
          renderingCtx,
          me,
          enemy,
          mousePositionOpt,
          selectedShipOpt,
          MyBoardPreGamePos.get,
          SquareSizeBig.get,
          fillEmptySquares = false
        )
      case Some(GameState(_, _, me, enemy, _: InGameMode)) =>
        drawMyBoard(
          renderingCtx,
          me,
          enemy,
          None,
          None,
          MyBoardInGamePos.get,
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

        val ScreenModel(_, _, _, _, _, missilesPopupMillisOpt, extraTurnPopup, extraTurnText) =
          screenModel.get
        drawMissiles(renderingCtx, turnAttacks, missilesPopupMillisOpt)
        drawExtraTurnPopup(renderingCtx, turnAttacks, extraTurnPopup, extraTurnText)
        drawDestructionSummary(renderingCtx)
        drawBoardMarksSelector(renderingCtx, selectedBoardMarkOpt)
      case Some(GameState(_, _, me, enemy, _: GameOverMode)) =>
        drawMyBoard(
          renderingCtx,
          me,
          enemy,
          None,
          None,
          MyBoardGameOverPos.get,
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

        drawBoardMarksSelector(renderingCtx, selectedBoardMarkOpt)
      case _ =>
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

    val shipToPlaceHoverOpt: Option[ToPlaceShip] = shipToPlaceHover.get
    val placeShipsSqSize = PlaceShipsSqSize.get
    allShipsToPlaceCoordinates.get.foreach { case ToPlaceShip(ship, pieces, alreadyPlaced) =>
      if (alreadyPlaced)
        pieces.foreach(
          drawSquareAbs(
            renderingCtx,
            _,
            placeShipsSqSize,
            CanvasColor.Ship(CanvasBorder.Standard(alpha = 0.4), alpha = 0.4)
          )
        )
      else
        (selectedShipOpt, shipToPlaceHoverOpt) match {
          case (Some(selectedShip), _) if selectedShip.shipId == ship.shipId =>
            pieces.foreach(
              drawSquareAbs(
                renderingCtx,
                _,
                placeShipsSqSize,
                CanvasColor.Ship(CanvasBorder.RedBold())
              )
            )
          case (_, Some(ToPlaceShip(hoverShip, _, _))) if hoverShip.shipId == ship.shipId =>
            pieces.foreach(
              drawSquareAbs(
                renderingCtx,
                _,
                placeShipsSqSize,
                CanvasColor.Ship(CanvasBorder.RedBold())
              )
            )
          case _ =>
            pieces.foreach(drawSquareAbs(renderingCtx, _, placeShipsSqSize, CanvasColor.Ship()))
        }
    }

    val water: Array[Array[Boolean]] =
      Array.fill(boardSize.x, boardSize.y)(fillEmptySquares) // TODO property
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
        drawBoardSquare(
          renderingCtx,
          boardPosition,
          Coordinate(x, y),
          squareSize,
          CanvasColor.Water()
        )

    me.myBoard.ships.foreach { case ShipInGame(ship, position) =>
      ship.pieces
        .map(_ + position)
        .foreach(drawBoardSquare(renderingCtx, boardPosition, _, squareSize, CanvasColor.Ship()))
    }

    enemy.turnPlayHistory.zipWithIndex.foreach { case (TurnPlay(turn, turnAttacks, _), index) =>
      turnAttacks.flatMap(_.coordinateOpt).foreach { coor =>
        drawTurnNumberCoor(
          renderingCtx = renderingCtx,
          boardPosition = boardPosition,
          coor = coor,
          size = squareSize,
          turn = turn,
          textSize = (squareSize * 0.6).toInt
        )
        if (index == 0)
          drawBoardSquare(
            renderingCtx,
            boardPosition,
            coor,
            squareSize,
            CanvasColor.White(CanvasBorder.RedBold())
          )
      }
    }

    (mousePositionOpt, selectedShipOpt) match {
      case (Some(mousePosition), Some(ship)) =>
        myBoardMouseCoordinate.get match {
          case Some(boardCoor) =>
            val roundedBoardCoor =
              boardCoor.roundTo(boardSize - ship.size + Coordinate(1, 1))

            def drawCoordinate(coor: Coordinate): Unit =
              if (canPlaceInBoard(me.myBoard, ship, roundedBoardCoor))
                drawBoardSquare(
                  renderingCtx,
                  boardPosition,
                  coor,
                  squareSize,
                  CanvasColor.Ship(CanvasBorder.Standard(alpha = 0.9), alpha = 0.9)
                )
              else
                drawBoardSquare(
                  renderingCtx,
                  boardPosition,
                  coor,
                  squareSize,
                  CanvasColor.Red(CanvasBorder.Standard(alpha = 0.75), alpha = 0.75)
                )

            ship.pieces.map(_ + roundedBoardCoor).foreach(drawCoordinate)
          case _ =>
            val center = ship.size * (squareSize / 2)

            ship.pieces
              .map(_ * squareSize + mousePosition - center)
              .foreach(drawSquareAbs(renderingCtx, _, squareSize, CanvasColor.Ship(alpha = 0.5)))
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

    val fontSize = Math.max(15, squareSize / 2)
    renderingCtx.fillStyle = s"rgb(0, 0, 0)"
    renderingCtx.font = s"${fontSize}px serif"
    renderingCtx.textBaseline = "bottom"
    renderingCtx.textAlign = "left"
    renderingCtx.fillText(boardTitle, boardPosition.x, boardPosition.y - 2)
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

    val boardMarksWithCoor: Seq[(Coordinate, Option[Turn], BoardMark)] =
      me.enemyBoardMarksWithCoor

    boardMarksWithCoor.foreach { case (coor, turnNumberOpt, mark) =>
      val canvasColor: CanvasColor =
        mark match {
          case BoardMark.Empty               => CanvasColor.White()
          case BoardMark.Water               => CanvasColor.WaterDarker()
          case BoardMark.ShipHit             => CanvasColor.ShipDarker()
          case BoardMark.ManualWater         => CanvasColor.Water()
          case BoardMark.ManualShip          => CanvasColor.Ship()
          case BoardMark.ManualQuestionShip  => CanvasColor.Water()
          case BoardMark.ManualQuestionWater => CanvasColor.Ship()
        }
      drawBoardSquare(renderingCtx, boardPosition, coor, squareSize, canvasColor)
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

    me.turnPlayHistory.headOption.map(_.turn).foreach { lastTurn =>
      boardMarksWithCoor.foreach {
        case (coor, Some(turn), _) if turn == lastTurn =>
          drawBoardSquare(
            renderingCtx,
            boardPosition,
            coor,
            squareSize,
            CanvasColor.White(CanvasBorder.RedBold())
          )
        case _ =>
      }
    }

    enemyBoardMouseCoordinate.get.foreach { enemyBoardCoor =>
      selectedBoardMarkOpt match {
        case Some(boardMark) if !boardMarksWithCoor.exists { case (coor, _, currentBoardMark) =>
              coor == enemyBoardCoor && currentBoardMark.isPermanent
            } =>
          val canvasColor: CanvasColor =
            boardMark match {
              case BoardMark.Empty       => CanvasColor.White(alpha = 0.5)
              case BoardMark.ManualWater => CanvasColor.Water(alpha = 0.5)
              case BoardMark.ManualShip  => CanvasColor.Ship(alpha = 0.5)
              case _                     => CanvasColor.White()
            }
          drawBoardSquare(renderingCtx, boardPosition, enemyBoardCoor, squareSize, canvasColor)
        case None
            if turnAttacks.exists(!_.isPlaced) &&
              gamePresenter.isValidCoordinateTarget(enemyBoardCoor) =>
          drawCrosshair(
            renderingCtx,
            boardPosition,
            enemyBoardCoor,
            squareSize,
            lineWidth = 2.0,
            alpha = 0.5
          )
        case _ =>
      }
    }

    turnAttacks.foreach {
      case Attack(_, Some(enemyBoardCoor)) =>
        drawCrosshair(
          renderingCtx,
          boardPosition,
          enemyBoardCoor,
          squareSize,
          lineWidth = 2.0,
          alpha = 1.0
        )
      case _ =>
    }
  }

  def drawMissiles(
      renderingCtx: CanvasRenderingContext2D,
      turnAttacks: List[Attack],
      missilesPopupMillisOpt: Option[Int]
  ): Unit = {
    val missilesPos = MissilesInicialPos.get
    val missilesSize = MissilesSqSize.get
    val missilesPosDiff = MissilesPosDiff.get

    val currentMissilesSize: Int =
      missilesPopupMillisOpt match {
        case None =>
          missilesSize
        case Some(timeMillis) if timeMillis > MissilesFastPopupTime =>
          val perc: Double =
            (timeMillis - MissilesFastPopupTime).toDouble / MissilesInitialPopupTime
          (missilesSize * (1 + MissilesMaxOversize * (MissilesFastPerc + (1 - MissilesFastPerc) * perc))).toInt
        case Some(timeMillis) =>
          val perc: Double = timeMillis.toDouble / MissilesFastPopupTime
          (missilesSize * (1 + MissilesMaxOversize * MissilesFastPerc * perc)).toInt
      }
    val currentMissilesDiff: Coordinate =
      Coordinate.square((currentMissilesSize - missilesSize) / 2)

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
          missilesPos.x + index * missilesPosDiff.x - currentMissilesDiff.x,
          missilesPos.y + index * missilesPosDiff.y - currentMissilesDiff.y,
          currentMissilesSize,
          currentMissilesSize
        )

        renderingCtx.globalAlpha = 1.0
      case _ =>
    }
  }

  def drawExtraTurnPopup(
      renderingCtx: CanvasRenderingContext2D,
      turnAttacks: List[Attack],
      extraTurnPopupOpt: Option[Int],
      extraTurnPopupTextOpt: Option[Span]
  ): Unit = {
    (extraTurnPopupOpt, extraTurnPopupTextOpt.map(_.innerText)) match {
      case (Some(timeRemaining), Some(extraTurnText)) =>
        val middleX = myBoardCanvas.width / 2
        val bottomY = myBoardCanvas.height - SquareSizeMedium.get
        val textSize = (SquareSizeBig.get * 1.6).toInt
        val fadeAlpha =
          if (timeRemaining > ExtraTurnPopupTimeFade)
            1.0
          else
            timeRemaining.toDouble / ExtraTurnPopupTimeFade
        val extraTurnPopupMissileSize = (SquareSizeBig.get * 1.0).toInt
        val missilesDiff: Coordinate =
          Coordinate(extraTurnPopupMissileSize, 0)
        val missilesInitialPos: Coordinate =
          Coordinate(
            (-missilesDiff.x * (turnAttacks.size / 2.0)).toInt,
            -textSize - extraTurnPopupMissileSize
          )

        if (ExtraTurnAppear(timeRemaining)) {
          renderingCtx.fillStyle = s"rgb(0, 0, 0, $fadeAlpha)"
          renderingCtx.font = s"${textSize}px serif"
          renderingCtx.textBaseline = "bottom"
          renderingCtx.textAlign = "center"
          renderingCtx.fillText(extraTurnText, middleX, bottomY)

          renderingCtx.globalAlpha = fadeAlpha
          turnAttacks.zipWithIndex.foreach { case (Attack(_, _), index) =>
            renderingCtx.drawImage(
              attackSimple.element,
              0,
              0,
              500,
              500,
              middleX + missilesInitialPos.x + (missilesDiff * index).x,
              bottomY + missilesInitialPos.y + (missilesDiff * index).y,
              extraTurnPopupMissileSize,
              extraTurnPopupMissileSize
            )
          }
          renderingCtx.globalAlpha = 1.0
        }
      case _ =>
    }
  }

  def drawDestructionSummary(renderingCtx: CanvasRenderingContext2D): Unit = {
    val sqSize = DestructionSummarySqSize.get
    allShipsSummaryCoordinates.get.foreach { case SummaryShip(_, pieces, destroyed) =>
      pieces.foreach(drawSquareAbs(renderingCtx, _, sqSize, CanvasColor.Ship()))

      if (destroyed)
        pieces.foreach(drawCrosshairAbs(renderingCtx, _, sqSize, lineWidth = 2.0, alpha = 1.0))
    }
  }

  def drawBoardMarksSelector(
      renderingCtx: CanvasRenderingContext2D,
      selectedBoardMarkOpt: Option[BoardMark]
  ): Unit = {
    val boardMarksSize = BoardMarksSelectorSize.get

    val selected = selectedBoardMarkOpt.orElse(boardMarkHover.get)

    BoardMarksSelectorAllPositions.get.foreach { case (boardMark, position) =>
      val canvasBorder: CanvasBorder =
        if (selected.contains(boardMark))
          CanvasBorder.RedBold()
        else
          CanvasBorder.Standard()

      val canvasColor =
        boardMark match {
          case BoardMark.Empty       => CanvasColor.White(canvasBorder)
          case BoardMark.ManualWater => CanvasColor.Water(canvasBorder)
          case BoardMark.ManualShip  => CanvasColor.Ship(canvasBorder)
          case _                     => CanvasColor.White(canvasBorder)
        }
      drawBoardSquare(renderingCtx, position, Coordinate.origin, boardMarksSize, canvasColor)
    }
  }

}

object BoardView {

  case class ViewShip(ship: Ship, pieces: List[Coordinate])

  case class ToPlaceShip(ship: Ship, pieces: List[Coordinate], alreadyPlaced: Boolean)

  case class SummaryShip(ship: Ship, pieces: List[Coordinate], destroyed: Boolean)

  val CanvasSize: Coordinate = Coordinate(1000, 400)

  val MissilesInitialPopupTime: Int = 2000
  val MissilesFastPopupTime: Int = 600
  val MissilesMaxOversize: Double = 0.5
  val MissilesFastPerc: Double = 0.9

  val ExtraTurnPopupTime: Int = 6800
  def ExtraTurnAppear(timeRemaining: Int): Boolean =
    timeRemaining < 4000 || ((timeRemaining / 400) % 2 == 0)
  val ExtraTurnPopupTimeFade: Int = 2000

}
