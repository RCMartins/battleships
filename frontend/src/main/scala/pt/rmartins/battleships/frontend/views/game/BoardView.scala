package pt.rmartins.battleships.frontend.views.game

import io.udash.css.CssView
import io.udash.{ModelProperty, ReadableProperty, any2Property}
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div}
import pt.rmartins.battleships.frontend.views.game.BoardView._
import pt.rmartins.battleships.frontend.views.game.CanvasUtils._
import pt.rmartins.battleships.frontend.views.game.ModeType._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.css.GameStyles
import pt.rmartins.battleships.shared.model.game.GameMode.{GameOverMode, PlayingMode, PreGameMode}
import pt.rmartins.battleships.shared.model.game.HitHint.ShipHit
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils.canPlaceInBoard
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class BoardView(
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    gamePresenter: GamePresenter,
    canvasUtils: CanvasUtils
) extends CssView {

  import canvasUtils._

  val myBoardCanvas: Canvas =
    canvas(
      GameStyles.canvasWithoutBorder,
      id := "mainGameCanvas"
    ).render

  screenModel.get.canvasSize.pipe { canvasSize =>
    myBoardCanvas.setAttribute("width", canvasSize.x.toString)
    myBoardCanvas.setAttribute("height", canvasSize.y.toString)
  }

  myBoardCanvas.onkeypress = (event: KeyboardEvent) => {
    gamePresenter.keyDown(event.key)
  }

  val canvasDiv: Div =
    div(id := "canvas-div", myBoardCanvas).render

  myBoardCanvas.onmousemove = (mouseEvent: MouseEvent) => {
    val rect = myBoardCanvas.getBoundingClientRect()
    gamePresenter.mouseMove(
      this,
      mouseEvent.clientX.toInt - rect.left.toInt,
      mouseEvent.clientY.toInt - rect.top.toInt
    )
  }

  myBoardCanvas.onmouseleave = (_: MouseEvent) => {
    gamePresenter.mouseLeave()
  }

  myBoardCanvas.onmousedown = (mouseEvent: MouseEvent) => {
    gamePresenter.mouseDown(this, mouseEvent.button)
    false // Prevent the mouse down from exiting the canvas
  }

  myBoardCanvas.onmouseup = (_: MouseEvent) => {
    gamePresenter.mouseUp()
  }

  myBoardCanvas.onmousewheel = (wheelEvent: WheelEvent) => {
    gamePresenter.mouseWheel(this, wheelEvent.deltaY.toInt / 100)
  }

  myBoardCanvas.oncontextmenu = (event: MouseEvent) => {
    event.preventDefault()
  }

  private val BoardSizeProperty: ReadableProperty[Int] =
    gamePresenter.rulesProperty.transform {
      case None        => 1
      case Some(rules) => rules.boardSize.x
    }

  private val squareSizesProperty: ReadableProperty[IndexedSeq[Int]] =
    BoardSizeProperty.transform { boardSize =>
      IndexedSeq(70, 100, 120, 150, 200, 300).map(_ / boardSize)
    }

  private val sizes: IndexedSeq[Int] = IndexedSeq(7, 10, 12, 15, 20, 30)

  private def checkSize(sizes: IndexedSeq[Int], screenSize: Int, defaultSizeIndex: Int): Int =
    sizes((if (screenSize < 500) 0 else if (screenSize < 680) 1 else 2) + defaultSizeIndex)

  private val SizeBig: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform { canvasSize =>
      checkSize(sizes, canvasSize.x, 3)
    }

  private val SizeMedium: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform { canvasSize =>
      checkSize(sizes, canvasSize.x, 2)
    }

  private val SizeSmall: ReadableProperty[Int] =
    screenModel.subProp(_.canvasSize).transform { canvasSize =>
      checkSize(sizes, canvasSize.x, 1)
    }

  private val SquareSizeBig: ReadableProperty[Int] =
    combine(screenModel.subProp(_.canvasSize), squareSizesProperty).transform {
      case (canvasSize, sizes) =>
        checkSize(sizes, canvasSize.x, 3)
    }
  private val SquareSizeMedium: ReadableProperty[Int] =
    combine(screenModel.subProp(_.canvasSize), squareSizesProperty).transform {
      case (canvasSize, sizes) =>
        checkSize(sizes, canvasSize.x, 2)
    }
  private val SquareSizeSmall: ReadableProperty[Int] =
    combine(screenModel.subProp(_.canvasSize), squareSizesProperty).transform {
      case (canvasSize, sizes) =>
        checkSize(sizes, canvasSize.x, 1)
    }

  private val MyBoardPreGameSqSize = SquareSizeBig
  private val MyBoardInGameSqSize = SquareSizeMedium
  private val MyBoardGameOverSqSize = SquareSizeBig
  private val MyBoardMargin = SizeMedium

  private val EnemyBoardSqSize = SquareSizeBig
  private val EnemyBoardMargin = SizeMedium

  private val MyBoardPreGamePos: ReadableProperty[Coordinate] =
    MyBoardMargin.transform(size => Coordinate(size, size))
  private val MyBoardInGamePos: ReadableProperty[Coordinate] =
    combine(
      screenModel.subProp(_.canvasSize),
      BoardSizeProperty,
      MyBoardInGameSqSize,
      MyBoardMargin
    ).transform { case (canvasSize, boardSize, myBoardInGameSize, myBoardMargin) =>
      Coordinate(
        canvasSize.x - myBoardInGameSize * boardSize - myBoardMargin,
        myBoardMargin
      )
    }
  private val MyBoardGameOverPos: ReadableProperty[Coordinate] =
    combine(
      screenModel.subProp(_.canvasSize),
      BoardSizeProperty,
      MyBoardGameOverSqSize,
      MyBoardMargin
    )
      .transform { case (canvasSize, boardSize, myBoardGameOverSqSize, myBoardMargin) =>
        Coordinate(
          canvasSize.x - myBoardGameOverSqSize * boardSize - myBoardMargin,
          myBoardMargin
        )
      }
  private val EnemyBoardPos: ReadableProperty[Coordinate] =
    EnemyBoardMargin.transform(size => Coordinate.square(size))

  private val BoardMarksSelectorSize = BoardView.BoardMarkSize.toProperty
  private val BoardMarksSelectorMargin = BoardView.BoardMarkMargin.toProperty
  private val BoardMarksSelectorPos: ReadableProperty[Coordinate] =
    combine(
      BoardSizeProperty,
      EnemyBoardPos,
      EnemyBoardSqSize,
      BoardMarksSelectorSize,
      BoardMarksSelectorMargin
    )
      .transform {
        case (
              boardSize,
              enemyBoardPos,
              enemyBoardSize,
              boardMarksSelectorSize,
              boardMarksSelectorMargin
            ) =>
          enemyBoardPos +
            Coordinate(
              (enemyBoardSize * boardSize) / 2 -
                (MarksSelectorOrder.size * boardMarksSelectorSize +
                  (MarksSelectorOrder.size - 1) * boardMarksSelectorMargin) / 2,
              enemyBoardSize * boardSize + boardMarksSelectorMargin
            )
      }
  private val BoardMarksSelectorCombined: ReadableProperty[(Coordinate, Int, Int)] =
    combine(BoardMarksSelectorPos, BoardMarksSelectorSize, BoardMarksSelectorMargin)

  private val MissilesInicialPos: ReadableProperty[Coordinate] =
    combine(BoardSizeProperty, EnemyBoardPos, SquareSizeBig, SizeBig).transform {
      case (boardSize, enemyBoardPos, squareSizeBig, sizeBig) =>
        enemyBoardPos + Coordinate(squareSizeBig * boardSize + sizeBig / 2, 0)
    }
  private val MissilesSqSize: ReadableProperty[Int] =
    SizeBig.transform(sizeBig => sizeBig * 2)

  private val MissilesPosDiff: ReadableProperty[Coordinate] =
    MissilesSqSize.transform { missilesSqSize =>
      Coordinate(0, missilesSqSize)
    }

  private val PlaceShipsPos: ReadableProperty[Coordinate] =
    combine(BoardSizeProperty, MyBoardPreGamePos, MyBoardPreGameSqSize).transform {
      case (boardSize, myBoardPreGamePos, myBoardPreGameSqSize) =>
        myBoardPreGamePos +
          Coordinate(myBoardPreGameSqSize * boardSize + myBoardPreGameSqSize, 0)
    }

  private val SummaryShipsSqSize: ReadableProperty[Int] = SquareSizeMedium
  private val DestructionSummaryHitCountSize: ReadableProperty[Int] = SquareSizeSmall
  private val SummaryMaxY: ReadableProperty[Int] =
    combine(BoardSizeProperty, EnemyBoardSqSize, SummaryShipsSqSize).transform {
      case (boardSize, enemyBoardSqSize, summaryShipsSqSize) =>
        (boardSize * enemyBoardSqSize) / summaryShipsSqSize
    }

  private val DestructionSummaryPos: ReadableProperty[Coordinate] =
    combine(
      gamePresenter.modeTypeProperty,
      MissilesInicialPos,
      MissilesSqSize,
      DestructionSummaryHitCountSize
    )
      .transform {
        case (
              Some(modeType @ (PlayingModeType | GameOverModeType)),
              missilesPos,
              missilesSize,
              destructionSummaryHitCountSize
            ) =>
          missilesPos +
            Coordinate(
              if (modeType == PlayingModeType)
                missilesSize + destructionSummaryHitCountSize * 2
              else
                destructionSummaryHitCountSize * 2,
              0
            )
        case _ =>
          Coordinate.origin
      }

  private val DestructionSummaryCombined: ReadableProperty[(Coordinate, Int)] =
    combine(DestructionSummaryPos, SummaryShipsSqSize)

  private val PlaceShipBoardMargin = Coordinate.square(20)

  private val shipsSummaryRelCoordinates: ReadableProperty[List[(ShipId, List[ViewShip])]] =
    combine(gamePresenter.rulesProperty, SummaryMaxY).transform {
      case (Some(Rules(_, shipsInThisGame, _, _, _)), summaryMaxY) =>
        def getShipsToPlacePos(
            posX: Int,
            posY: Int,
            columnX: Int,
            maxX: Int,
            ships: List[List[Ship]],
            currentList: List[ViewShip],
            shipBefore: Option[Ship]
        ): List[List[ViewShip]] =
          ships match {
            case (ship :: _) :: _ if posY > 0 && posY + ship.pieces.maxBy(_.y).y >= summaryMaxY =>
              val newColumnX = maxX + 4
              getShipsToPlacePos(
                posX = newColumnX,
                posY = 0,
                columnX = newColumnX,
                maxX = newColumnX,
                ships,
                currentList,
                shipBefore
              )
            case (ship :: next) :: nextList =>
              getShipsToPlacePos(
                posX + ship.size.x + 1,
                posY,
                columnX,
                Math.max(maxX, ship.pieces.map(_ + Coordinate(posX, posY)).maxBy(_.x).x),
                next :: nextList,
                ViewShip(ship, ship.pieces.map(_ + Coordinate(posX, posY))) :: currentList,
                Some(ship)
              )
            case Nil :: nextList =>
              currentList.reverse ::
                getShipsToPlacePos(
                  columnX,
                  posY + shipBefore.map(_.size.y + 1).getOrElse(0),
                  columnX,
                  maxX,
                  nextList,
                  Nil,
                  None
                )
            case Nil if currentList.nonEmpty =>
              currentList.reverse :: Nil
            case Nil =>
              Nil
          }

        val shipsGrouped: List[(ShipId, List[Ship])] =
          shipsInThisGame.ships
            .groupBy(_.shipId)
            .toList

        val minX =
          shipsGrouped.map { case (_, ships) =>
            val ship = ships.head
            val size = ships.size
            ship.size.min * size + size - 1
          }.max

        val shipsListList: List[List[Ship]] =
          shipsGrouped
            .map { case (shipId, ships) =>
              val ship = ships.head
              val size = ships.size
              val fullSizeX = ship.size.x * size + size - 1
              val fullSizeY = ship.size.y * size + size - 1
              if (
                Math.max(minX, fullSizeX) * ship.size.y <= Math.max(minX, fullSizeY) * ship.size.x
              )
                (shipId, ships)
              else
                (shipId, ships.map(_.rotateBy(1)))
            }
            .sortBy { case (shipId, list) =>
              (-list.head.piecesSize, shipId.id)
            }
            .map(_._2)

        getShipsToPlacePos(
          posX = 0,
          posY = 0,
          columnX = 0,
          maxX = 0,
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
      gamePresenter.modeTypeProperty,
      PlaceShipsPos,
      SummaryShipsSqSize
    ).transform {
      case (
            shipsSummary,
            Some(shipsLeftToPlace),
            Some(PreGameModeType),
            placeShipsPos,
            placeShipsSqSize
          ) =>
        val shipsLeftToPlaceMap: Map[ShipId, Int] =
          shipsLeftToPlace.groupBy(_.shipId).map { case (shipId, list) => shipId -> list.size }

        val shipsPlaced: Map[ShipId, Int] =
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
    combine(gamePresenter.mousePositionProperty, allShipsToPlaceCoordinates, SummaryShipsSqSize)
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

  val allShipsSummaryCoordinates
      : ReadableProperty[List[(ShipId, Coordinate, Int, List[SummaryShip])]] =
    combine(
      shipsSummaryRelCoordinates,
      gamePresenter.meProperty.transform(_.map(_.turnPlayHistory)),
      gamePresenter.modeTypeProperty,
      DestructionSummaryCombined,
      DestructionSummaryHitCountSize
    ).transform {
      case (
            shipsSummary,
            Some(turnPlayHistory),
            Some(PlayingModeType | GameOverModeType),
            (destructionSummaryPos, destructionSummarySqSize),
            destructionSummaryHitCountSize
          ) =>
        val shipsDestroyed: Map[ShipId, Int] =
          turnPlayHistory
            .flatMap(_.hitHints.collect { case ShipHit(shipId, true) => shipId })
            .groupBy(identity)
            .map { case (shipId, list) => shipId -> list.size }

        shipsSummary.map { case (shipId, viewShipList) =>
          val summaryShips: List[SummaryShip] =
            viewShipList.zipWithIndex.map { case (viewShip, index) =>
              SummaryShip(
                viewShip.ship,
                viewShip.pieces.map(relPieceCoor =>
                  destructionSummaryPos + relPieceCoor * destructionSummarySqSize
                ),
                shipsDestroyed.getOrElse(shipId, 0) > index
              )
            }

          val headShip = summaryShips.head
          val summaryCenter: Coordinate = {
            val minX = headShip.pieces.minBy(_.x).x
            val min = headShip.pieces.minBy(_.y).y
            val max = headShip.pieces.maxBy(_.y).y
            val centerY =
              (max + min) / 2 + destructionSummarySqSize / 2 -
                destructionSummaryHitCountSize / 2 + 1
            Coordinate(
              minX,
              centerY
            )
          }

          val hitCount: Int =
            turnPlayHistory
              .map(_.hitHints.count {
                case ShipHit(shipHitId, _) if shipHitId == shipId => true
                case _                                            => false
              })
              .sum - summaryShips.count(_.destroyed) * headShip.pieces.size

          (shipId, summaryCenter, hitCount, summaryShips)
        }
      case _ =>
        Nil
    }

  val summaryShipHover: ReadableProperty[Option[Ship]] =
    combine(
      gamePresenter.mousePositionProperty,
      allShipsSummaryCoordinates,
      SummaryShipsSqSize
    ).transform {
      case (Some(mousePosition), shipsSummary, destructionSummarySqSize) =>
        val sizeCoor = Coordinate.square(destructionSummarySqSize)
        shipsSummary
          .find { case (_, _, _, summaryShips) =>
            summaryShips.exists(
              _.pieces.exists(sqCoor =>
                mousePosition >= sqCoor && mousePosition <= sqCoor + sizeCoor
              )
            )
          }
          .map(_._4.head.ship)
      case _ =>
        None
    }

  val myBoardMouseCoordinate: ReadableProperty[Option[Coordinate]] =
    combine(
      gamePresenter.mousePositionProperty,
      gamePresenter.meProperty.transform(_.map(_.myBoard.boardSize)),
      gamePresenter.modeTypeProperty,
      SquareSizeBig,
      MyBoardPreGamePos
    ).transform {
      case (
            Some(mousePosition),
            Some(boardSize),
            Some(PreGameModeType),
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
      gamePresenter.enemyProperty,
      gamePresenter.modeTypeProperty,
      EnemyBoardPos,
      EnemyBoardSqSize
    ).transform {
      case (
            Some(mousePosition),
            Some(enemy),
            Some(PlayingModeType | GameOverModeType),
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

  private val BoardMarksSelectorAllPositions
      : ReadableProperty[List[(InGameMarkSelector, Coordinate)]] =
    combine(
      gamePresenter.modeTypeProperty,
      BoardMarksSelectorCombined
    ).transform {
      case (
            Some(PlayingModeType | GameOverModeType),
            (boardMarksSelectorPos, boardMarksSelectorSize, boardMarksSelectorMargin)
          ) =>
        MarksSelectorOrder.zipWithIndex.map { case (boardMark, index) =>
          (
            boardMark,
            boardMarksSelectorPos +
              Coordinate(index * (boardMarksSelectorSize + boardMarksSelectorMargin), 0)
          )
        }
      case _ =>
        Nil
    }

  val boardMarkHover: ReadableProperty[Option[InGameMarkSelector]] =
    combine(
      gamePresenter.mousePositionProperty,
      gamePresenter.modeTypeProperty,
      BoardMarksSelectorAllPositions,
      BoardMarksSelectorSize
    ).transform {
      case (
            Some(mousePosition),
            Some(PlayingModeType | GameOverModeType),
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

  val myBoardWaterCoordinatesSeqProperty: ReadableProperty[List[Coordinate]] =
    gamePresenter.meProperty.transform(_.map(_.myBoard)).transform {
      case Some(myBoard) =>
        val boardSize: Coordinate =
          myBoard.boardSize
        val water: Array[Array[Boolean]] =
          Array.fill(boardSize.x, boardSize.y)(false)

        myBoard.ships.foreach { case ShipInBoard(ship, position) =>
          ship.pieces
            .map(_ + position)
            .foreach { case Coordinate(x, y) =>
              for (dx <- -1 to 1; dy <- -1 to 1)
                Some(Coordinate(x + dx, y + dy)).filter(_.isInsideBoard(boardSize)).foreach {
                  case Coordinate(cx, cy) =>
                    water(cx)(cy) = true
                }
            }
        }

        (for {
          x <- 0 until boardSize.x
          y <- 0 until boardSize.y
          if water(x)(y)
        } yield Coordinate(x, y)).toList
      case _ =>
        Nil
    }

  def paint(): Unit = {
    val GameModel(
      mousePositionOpt,
      _,
      selectedShipOpt,
      turnAttacks,
      turnAttacksQueuedStatus,
      selectedBoardMarkOpt,
      _,
      _
    ) = gameModel.get

    val screenModelData = screenModel.get

    val renderingCtx = myBoardCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    renderingCtx.clearRect(0, 0, myBoardCanvas.width, myBoardCanvas.height)

    gamePresenter.gameStateProperty.get match {
      case Some(GameState(_, _, me, enemy, _: PreGameMode)) =>
        drawMyBoard(
          renderingCtx,
          screenModelData.myBoardTitle.innerText,
          me,
          enemy,
          mousePositionOpt,
          selectedShipOpt,
          MyBoardPreGamePos.get,
          SquareSizeBig.get,
          fillEmptySquares = false,
          hideMyBoard = false,
          isMyTurn = false,
          tick = screenModelData.tick
        )
      case Some(GameState(_, _, me, enemy, PlayingMode(isMyTurn, _, _, _, _))) =>
        drawMissiles(renderingCtx, turnAttacks, screenModelData.missilesPopupMillisOpt)
        drawDestructionSummary(renderingCtx, selectedShipOpt)

        drawMyBoard(
          renderingCtx,
          screenModelData.myBoardTitle.innerText,
          me,
          enemy,
          None,
          None,
          MyBoardInGamePos.get,
          SquareSizeMedium.get,
          fillEmptySquares = true,
          hideMyBoard = screenModelData.hideMyBoard,
          isMyTurn = !isMyTurn,
          tick = screenModelData.tick
        )

        drawEnemyBoard(
          renderingCtx,
          screenModelData.enemyBoardTitle.innerText,
          me,
          enemy,
          turnAttacks,
          EnemyBoardPos.get,
          selectedBoardMarkOpt,
          selectedShipOpt,
          screenModelData.hoverMove,
          attacksQueuedStatus = turnAttacksQueuedStatus,
          isMyTurn = isMyTurn,
          tick = screenModelData.tick
        )

        drawBoardMarksSelector(renderingCtx, selectedBoardMarkOpt)
        drawExtraTurnPopup(
          renderingCtx,
          turnAttacks,
          screenModelData.extraTurnPopup,
          screenModelData.extraTurnText.innerText
        )
      case Some(GameState(_, _, me, enemy, GameOverMode(_, _, _, _, enemyRealBoard))) =>
        drawDestructionSummary(renderingCtx, selectedShipOpt)

        if (screenModelData.revealEnemyBoard)
          drawGameOverEnemyBoard(
            renderingCtx,
            screenModelData.realEnemyBoardTitle.innerText,
            me,
            enemyRealBoard,
            MyBoardGameOverPos.get,
            SquareSizeBig.get,
            selectedShipOpt,
            screenModelData.hoverMove
          )
        else
          drawMyBoard(
            renderingCtx,
            screenModelData.myBoardTitle.innerText,
            me,
            enemy,
            None,
            None,
            MyBoardGameOverPos.get,
            SquareSizeBig.get,
            fillEmptySquares = true,
            hideMyBoard = false,
            isMyTurn = false,
            tick = screenModelData.tick
          )

        drawEnemyBoard(
          renderingCtx,
          screenModelData.enemyBoardTitle.innerText,
          me,
          enemy,
          Nil,
          EnemyBoardPos.get,
          selectedBoardMarkOpt,
          selectedShipOpt,
          screenModelData.hoverMove,
          attacksQueuedStatus = AttacksQueuedStatus.NotSet,
          isMyTurn = false,
          tick = screenModelData.tick
        )

        drawBoardMarksSelector(renderingCtx, selectedBoardMarkOpt)
      case _ =>
    }
  }

  def drawMyBoard(
      renderingCtx: CanvasRenderingContext2D,
      boardTitle: String,
      me: Player,
      enemy: SimplePlayer,
      mousePositionOpt: Option[Coordinate],
      selectedShipOpt: Option[Ship],
      boardPosition: Coordinate,
      squareSize: Int,
      fillEmptySquares: Boolean,
      hideMyBoard: Boolean,
      isMyTurn: Boolean,
      tick: Int
  ): Unit = {
    val boardSize = me.myBoard.boardSize

    drawBoardLimits(
      renderingCtx,
      boardTitle,
      boardSize,
      boardPosition,
      squareSize,
      if (fillEmptySquares && !hideMyBoard) Some(CanvasColor.Water()) else None,
      Some(tick).filter(_ => isMyTurn)
    )

    val shipToPlaceHoverOpt: Option[ToPlaceShip] = shipToPlaceHover.get
    val placeShipsSqSize = SummaryShipsSqSize.get
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

    if (!hideMyBoard) {
      if (!fillEmptySquares)
        myBoardWaterCoordinatesSeqProperty.get.foreach { case Coordinate(x, y) =>
          drawBoardSquare(
            renderingCtx,
            boardPosition,
            Coordinate(x, y),
            squareSize,
            CanvasColor.Water()
          )
        }

      me.myBoard.ships.foreach { case ShipInBoard(ship, position) =>
        ship.pieces
          .map(_ + position)
          .foreach(drawBoardSquare(renderingCtx, boardPosition, _, squareSize, CanvasColor.Ship()))
      }
    }

    enemy.turnPlayHistory.zipWithIndex.foreach { case (TurnPlay(turn, turnAttacks, _), index) =>
      turnAttacks.flatMap(_.coordinateOpt).foreach { coor =>
        val textSize = Math.max((MinTextSize * 0.6).toInt, (squareSize * 0.6).toInt)
        drawTurnNumberCoor(
          renderingCtx = renderingCtx,
          boardPosition = boardPosition,
          coor = coor,
          size = squareSize,
          turn = turn,
          textSize = textSize
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

  def drawEnemyBoard(
      renderingCtx: CanvasRenderingContext2D,
      boardTitle: String,
      me: Player,
      enemy: SimplePlayer,
      turnAttacks: List[Attack],
      boardPosition: Coordinate,
      selectedBoardMarkOpt: Option[InGameMarkSelector],
      selectedShipOpt: Option[Ship],
      hoverMove: Option[Turn],
      attacksQueuedStatus: AttacksQueuedStatus,
      isMyTurn: Boolean,
      tick: Int
  ): Unit = {
    val squareSize: Int = EnemyBoardSqSize.get

    drawBoardLimits(
      renderingCtx,
      boardTitle,
      enemy.boardSize,
      boardPosition,
      squareSize,
      backgroundColor = None,
      Some(tick).filter(_ => isMyTurn)
    )

    val boardMarksWithCoor: Seq[(Coordinate, Option[Turn], BoardMark)] =
      me.enemyBoardMarksWithCoor

    boardMarksWithCoor.foreach { case (coor, turnNumberOpt, mark) =>
      val canvasColor: CanvasColor =
        mark match {
          case BoardMark.Empty       => CanvasColor.White()
          case BoardMark.Water       => CanvasColor.WaterDarker()
          case BoardMark.ShipHit     => CanvasColor.ShipDarker()
          case BoardMark.ManualWater => CanvasColor.Water()
          case BoardMark.ManualShip  => CanvasColor.Ship()
        }
      drawBoardSquare(renderingCtx, boardPosition, coor, squareSize, canvasColor)
      turnNumberOpt.foreach { turnNumber =>
        drawTurnNumberCoor(
          renderingCtx,
          boardPosition,
          coor,
          size = squareSize,
          turnNumber,
          // TODO to property (use same as drawGameOverEnemyBoard)
          textSize = (SquareSizeBig.get * 0.6).toInt
        )
      }
    }

    me.turnPlayHistory.zipWithIndex.foreach { case (turnPlay, index) =>
      def drawTurn(canvasColor: CanvasColor): Unit =
        turnPlay.turnAttacks.flatMap(_.coordinateOpt).foreach { coor =>
          drawBoardSquare(
            renderingCtx,
            boardPosition,
            coor,
            squareSize,
            canvasColor
          )
        }

      selectedShipOpt match {
        case _ if hoverMove.contains(turnPlay.turn) =>
          drawTurn(CanvasColor.White(CanvasBorder.DashBlue()))
        case Some(ship) if hoverMove.isEmpty && turnPlay.hitHints.exists {
              case ShipHit(shipId, _) if ship.shipId == shipId => true
              case _                                           => false
            } =>
          drawTurn(CanvasColor.White(CanvasBorder.DashRed()))
        case None =>
          if (index == 0)
            drawTurn(CanvasColor.White(CanvasBorder.RedBold()))
        case _ =>
      }
    }

    enemyBoardMouseCoordinate.get.foreach { enemyBoardCoor =>
      selectedBoardMarkOpt match {
        case Some(selectedBoardMark) =>
          val boardMark = me.enemyBoardMarks(enemyBoardCoor.x)(enemyBoardCoor.y)._2
          (boardMark, selectedBoardMark) match {
            case (
                  BoardMark.ShipHit | BoardMark.ManualShip,
                  InGameMarkSelector.FillWaterSelector
                ) =>
              drawImageAbs(
                renderingCtx,
                fillWaterImage.element,
                x = boardPosition.x + enemyBoardCoor.x * squareSize + 2,
                y = boardPosition.y + enemyBoardCoor.y * squareSize + 2,
                squareSize - 4,
                squareSize - 4,
                useAntiAliasing = true
              )
            case (boardMark, _) if !boardMark.isPermanent =>
              val canvasColor: CanvasColor =
                selectedBoardMark match {
                  case InGameMarkSelector.ManualWaterSelector => CanvasColor.Water(alpha = 0.5)
                  case InGameMarkSelector.ManualShipSelector  => CanvasColor.Ship(alpha = 0.5)
                  case _                                      => CanvasColor.White()
                }
              drawBoardSquare(renderingCtx, boardPosition, enemyBoardCoor, squareSize, canvasColor)
            case _ =>
          }
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
        drawCrosshairAbs(
          renderingCtx,
          boardPosition + enemyBoardCoor * squareSize + Coordinate.square(2),
          squareSize - 4,
          lineWidth = 2.0,
          alpha = 1.0,
          red = attacksQueuedStatus != AttacksQueuedStatus.Queued
        )
      case _ =>
    }
  }

  def drawGameOverEnemyBoard(
      renderingCtx: CanvasRenderingContext2D,
      boardTitle: String,
      me: Player,
      enemyShips: List[ShipInBoard],
      boardPosition: Coordinate,
      squareSize: Int,
      selectedShipOpt: Option[Ship],
      hoverMove: Option[Turn]
  ): Unit = {
    val boardSize = me.myBoard.boardSize

    drawBoardLimits(
      renderingCtx,
      boardTitle,
      boardSize,
      boardPosition,
      squareSize,
      backgroundColor = Some(CanvasColor.Water()),
      None
    )

    enemyShips.foreach { case ShipInBoard(ship, position) =>
      ship.pieces
        .map(_ + position)
        .foreach(drawBoardSquare(renderingCtx, boardPosition, _, squareSize, CanvasColor.Ship()))
    }

    me.turnPlayHistory.zipWithIndex.foreach { case (turnPlay, index) =>
      turnPlay.turnAttacks.flatMap(_.coordinateOpt).foreach { coor =>
        drawTurnNumberCoor(
          renderingCtx,
          boardPosition,
          coor,
          size = squareSize,
          turnPlay.turn,
          textSize = (SquareSizeBig.get * 0.6).toInt // TODO to property (use same as Enemy board)
        )
      }

      def drawTurn(canvasColor: CanvasColor): Unit =
        turnPlay.turnAttacks.flatMap(_.coordinateOpt).foreach { coor =>
          drawBoardSquare(
            renderingCtx,
            boardPosition,
            coor,
            squareSize,
            canvasColor
          )
        }

      selectedShipOpt match {
        case _ if hoverMove.contains(turnPlay.turn) =>
          drawTurn(CanvasColor.White(CanvasBorder.DashBlue()))
        case Some(ship) if hoverMove.isEmpty && turnPlay.hitHints.exists {
              case ShipHit(shipId, _) if ship.shipId == shipId => true
              case _                                           => false
            } =>
          drawTurn(CanvasColor.White(CanvasBorder.DashRed()))
        case None =>
          if (index == 0)
            drawTurn(CanvasColor.White(CanvasBorder.RedBold()))
        case _ =>
      }
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

        drawImageAbs(
          renderingCtx,
          attackSimpleImage.element,
          missilesPos.x + index * missilesPosDiff.x - currentMissilesDiff.x,
          missilesPos.y + index * missilesPosDiff.y - currentMissilesDiff.y,
          currentMissilesSize,
          currentMissilesSize,
          useAntiAliasing = true
        )

        renderingCtx.globalAlpha = 1.0
      case _ =>
    }
  }

  def drawExtraTurnPopup(
      renderingCtx: CanvasRenderingContext2D,
      turnAttacks: List[Attack],
      extraTurnPopupOpt: Option[Int],
      extraTurnPopupText: String
  ): Unit =
    extraTurnPopupOpt.foreach { timeRemaining =>
      val middleX = myBoardCanvas.width / 2
      val bottomY = myBoardCanvas.height - SizeMedium.get
      val textSize = (SizeBig.get * 1.6).toInt
      val fadeAlpha =
        if (timeRemaining > ExtraTurnPopupTimeFade)
          1.0
        else
          timeRemaining.toDouble / ExtraTurnPopupTimeFade
      val extraTurnPopupMissileSize = SizeBig.get
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
        renderingCtx.fillText(extraTurnPopupText, middleX, bottomY)

        renderingCtx.globalAlpha = fadeAlpha
        turnAttacks.zipWithIndex.foreach { case (Attack(_, _), index) =>
          drawImageAbs(
            renderingCtx,
            attackSimpleImage.element,
            middleX + missilesInitialPos.x + (missilesDiff * index).x,
            bottomY + missilesInitialPos.y + (missilesDiff * index).y,
            extraTurnPopupMissileSize,
            extraTurnPopupMissileSize,
            useAntiAliasing = false
          )
        }
        renderingCtx.globalAlpha = 1.0
      }
    }

  def drawDestructionSummary(
      renderingCtx: CanvasRenderingContext2D,
      selectedShipOpt: Option[Ship]
  ): Unit = {
    val sqSize = SummaryShipsSqSize.get
    val hitCountSize = DestructionSummaryHitCountSize.get
    allShipsSummaryCoordinates.get.foreach { case (_, summaryCenter, hitCount, summaryShipList) =>
      if (hitCount > 0) {
        val relCoor = summaryCenter + Coordinate(-6, hitCountSize / 2)
        renderingCtx.fillStyle = s"rgb(0, 0, 0)"
        renderingCtx.font = s"bold ${sqSize}px serif"
        renderingCtx.textBaseline = "middle"
        renderingCtx.textAlign = "right"
        renderingCtx.fillText(s"+$hitCount", relCoor.x, relCoor.y)
      }
      summaryShipList.foreach { case SummaryShip(summaryShip, pieces, destroyed) =>
        pieces.foreach { shipPiece =>
          drawSquareAbs(renderingCtx, shipPiece, sqSize, CanvasColor.Ship())

          selectedShipOpt.foreach {
            case selectedShip if selectedShip.shipId == summaryShip.shipId =>
              drawSquareAbs(
                renderingCtx,
                shipPiece,
                sqSize,
                CanvasColor.White(CanvasBorder.DashRed(lineWidth = 2.0))
              )
            case _ =>
          }
        }

        if (destroyed)
          pieces.foreach(pieceCoor =>
            drawCrosshairAbs(
              renderingCtx,
              pieceCoor + Coordinate.square(2),
              sqSize - 4,
              lineWidth = 2.0,
              alpha = 1.0
            )
          )
      }
    }
  }

  def drawBoardMarksSelector(
      renderingCtx: CanvasRenderingContext2D,
      selectedBoardMarkOpt: Option[InGameMarkSelector]
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
          case InGameMarkSelector.ManualWaterSelector =>
            CanvasColor.Water(canvasBorder)
          case InGameMarkSelector.ManualShipSelector | InGameMarkSelector.FillWaterSelector =>
            CanvasColor.Ship(canvasBorder)
          case _ =>
            CanvasColor.White(canvasBorder)
        }
      drawBoardSquare(renderingCtx, position, Coordinate.origin, boardMarksSize, canvasColor)

      if (boardMark == InGameMarkSelector.FillWaterSelector) {
        drawImageAbs(
          renderingCtx,
          fillWaterImage.element,
          x = position.x + 3,
          y = position.y + 3,
          boardMarksSize - 6,
          boardMarksSize - 6,
          useAntiAliasing = true
        )
      }
    }
  }

}

object BoardView {

  case class ViewShip(ship: Ship, pieces: List[Coordinate])

  case class ToPlaceShip(ship: Ship, pieces: List[Coordinate], alreadyPlaced: Boolean)

  case class SummaryShip(ship: Ship, pieces: List[Coordinate], destroyed: Boolean)

  val MinTextSize = 15
  val BoardMarkSize = 30
  val BoardMarkMargin = 20
  val CanvasSize: Coordinate =
    Coordinate(1000, 20 + 300 + BoardMarkMargin * 2 + BoardMarkSize)

  sealed trait InGameMarkSelector {
    def matches(currentBoardMark: BoardMark): Boolean
  }

  object InGameMarkSelector {

    case object ManualShipSelector extends InGameMarkSelector {
      override def matches(boardMark: BoardMark): Boolean = boardMark.isShip
    }

    case object ManualWaterSelector extends InGameMarkSelector {
      override def matches(boardMark: BoardMark): Boolean = boardMark.isWater
    }

    case object FillWaterSelector extends InGameMarkSelector {
      override def matches(boardMark: BoardMark): Boolean = false
    }

  }

  val MarksSelectorOrder: List[InGameMarkSelector] =
    List(
      InGameMarkSelector.ManualShipSelector,
      InGameMarkSelector.ManualWaterSelector,
      InGameMarkSelector.FillWaterSelector
    )

  val MissilesInitialPopupTime: Int = 2000
  val MissilesFastPopupTime: Int = 600
  val MissilesMaxOversize: Double = 0.5
  val MissilesFastPerc: Double = 0.9

  val ExtraTurnPopupTime: Int = 7600
  def ExtraTurnAppear(timeRemaining: Int): Boolean =
    timeRemaining < 4000 || ((timeRemaining / 400) % 2 == 0)
  val ExtraTurnPopupTimeFade: Int = 2000

}
