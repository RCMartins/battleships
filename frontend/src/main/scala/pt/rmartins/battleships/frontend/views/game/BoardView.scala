package pt.rmartins.battleships.frontend.views.game

import io.udash._
import io.udash.bindings.modifiers.Binding
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.css.CssView
import org.scalajs.dom._
import org.scalajs.dom.html.{Canvas, Div}
import pt.rmartins.battleships.frontend.views.game.BoardView._
import pt.rmartins.battleships.frontend.views.game.CanvasUtils._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.shared.css.GameStyles
import pt.rmartins.battleships.shared.model.game.GameMode._
import pt.rmartins.battleships.shared.model.game.HitHint.ShipHit
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils.canPlaceInBoard
import scalatags.JsDom
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class BoardView(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gamePresenter: GamePresenter,
    mousePresenter: MousePresenter,
    canvasUtils: CanvasUtils,
    viewUtils: ViewUtils,
) extends CssView {

  import canvasUtils._

  val mainBoardCanvas: Canvas =
    canvas(
      GameStyles.canvasWithoutBorder,
    ).render

  val smallBoardCanvas: Canvas =
    canvas(
      GameStyles.canvasWithoutBorder,
    ).render

  screenModel
    .subProp(_.mainBoardCanvasSize)
    .listen(
      { canvasSize =>
        mainBoardCanvas.setAttribute("width", canvasSize.x.toString)
        mainBoardCanvas.setAttribute("height", canvasSize.y.toString)
      },
      initUpdate = true
    )

  screenModel
    .subProp(_.smallBoardCanvasSize)
    .listen(
      { canvasSize =>
        smallBoardCanvas.setAttribute("width", canvasSize.x.toString)
        smallBoardCanvas.setAttribute("height", canvasSize.y.toString)
      },
      initUpdate = true
    )

  mainBoardCanvas.onkeypress = (event: KeyboardEvent) => {
    gamePresenter.keyDown(event.key, event.ctrlKey)
  }

  mainBoardCanvas.onmousemove = (mouseEvent: MouseEvent) => {
    val rect = mainBoardCanvas.getBoundingClientRect()
    mousePresenter.mouseMove(
      this,
      mouseEvent.clientX.toInt - rect.left.toInt,
      mouseEvent.clientY.toInt - rect.top.toInt
    )
  }

  mainBoardCanvas.onmouseleave = (_: MouseEvent) => {
    mousePresenter.mouseLeave()
  }

  mainBoardCanvas.onmousedown = (mouseEvent: MouseEvent) => {
    mousePresenter.mouseDown(this, mouseEvent.button)
    false // Prevent the mouse down from exiting the canvas
  }

  mainBoardCanvas.onmouseup = (_: MouseEvent) => {
    mousePresenter.mouseUp()
  }

  mainBoardCanvas.onmousewheel = (wheelEvent: WheelEvent) => {
    mousePresenter.mouseWheel(this, wheelEvent.deltaY.toInt / 100)
  }

  mainBoardCanvas.oncontextmenu = (event: MouseEvent) => {
    event.preventDefault()
  }

  private def createBoardSizes(
      boardSizeProperty: ReadableProperty[Option[Coordinate]],
      canvasSize: ReadableProperty[Coordinate],
      defaultMargin: Int
  ): ReadableProperty[(Coordinate, Int)] =
    combine(boardSizeProperty, canvasSize).transform {
      case (Some(boardSize), canvasSize) =>
        val boardSquareSize: Int =
          Math.min(
            (canvasSize.x - defaultMargin * 2) / boardSize.x,
            (canvasSize.y - defaultMargin * 2) / boardSize.y
          )
        val boardPos: Coordinate =
          Coordinate(
            (canvasSize.x - boardSquareSize * boardSize.x) / 2,
            (canvasSize.y - boardSquareSize * boardSize.y) / 2
          )
        (boardPos, boardSquareSize)
      case _ =>
        (Coordinate.square(defaultMargin), 1)
    }

  private val MainBoardSizes: ReadableProperty[(Coordinate, Int)] =
    createBoardSizes(
      gamePresenter.mainBoardSizeProperty,
      screenModel.subProp(_.mainBoardCanvasSize),
      MainBoardDefaultMargin
    )

  private val MainBoardPos: ReadableProperty[Coordinate] =
    MainBoardSizes.transform(_._1)

  private val MainBoardSquareSize: ReadableProperty[Int] =
    MainBoardSizes.transform(_._2)

  private val MainBoardTurnTextSize: ReadableProperty[Int] =
    MainBoardSquareSize.transform(sqSize => Math.max(MinTextSize, (sqSize * 0.6).toInt))

  private val SmallBoardSizes: ReadableProperty[(Coordinate, Int)] =
    createBoardSizes(
      gamePresenter.myBoardSizeProperty,
      screenModel.subProp(_.smallBoardCanvasSize),
      SmallBoardDefaultMargin
    )

  private val SmallBoardPos: ReadableProperty[Coordinate] =
    SmallBoardSizes.transform(_._1)

  private val SmallBoardSquareSize: ReadableProperty[Int] =
    SmallBoardSizes.transform(_._2)

  private val SmallBoardTurnTextSize: ReadableProperty[Int] =
    SmallBoardSquareSize.transform(sqSize => Math.max(MinTextSize, (sqSize * 0.6).toInt))

//  private val SummaryShipsSqSize: ReadableProperty[Int] = SquareSizeMedium
//  private val DestructionSummaryHitCountSize: ReadableProperty[Int] = SquareSizeSmall
//  private val SummaryMaxY: ReadableProperty[Int] =
//    combine(BoardSizeProperty, EnemyBoardSqSize, SummaryShipsSqSize).transform {
//      case (boardSize, enemyBoardSqSize, summaryShipsSqSize) =>
//        (boardSize * enemyBoardSqSize) / summaryShipsSqSize
//    }
//
//  private val shipsForSummaryProperty: ReadableProperty[Option[List[Ship]]] =
//    combine(
//      gamePresenter.rulesProperty.transform(_.map(_.gameFleet.shipsList)),
//      gamePresenter.gamePuzzleStateProperty.transform(
//        _.map(_.playerPuzzle.gameFleet.shipsList)
//      )
//    ).transform { case (shipsListOpt1, shipsListOpt2) =>
//      shipsListOpt1.orElse(shipsListOpt2)
//    }
//
//  private val DestructionSummaryPos: ReadableProperty[Coordinate] =
//    combine(
//      gamePresenter.modeTypeOrPuzzleProperty,
//      MissilesInicialPos,
//      MissilesSqSize,
//      DestructionSummaryHitCountSize
//    ).transform {
//      case (
//            modeType @ ((Some(PlayingModeType | GameOverModeType), _) | (_, true)),
//            missilesPos,
//            missilesSize,
//            destructionSummaryHitCountSize
//          ) =>
//        missilesPos +
//          Coordinate(
//            if (modeType._1.contains(PlayingModeType))
//              missilesSize + destructionSummaryHitCountSize * 2
//            else
//              destructionSummaryHitCountSize * 2,
//            0
//          )
//      case _ =>
//        Coordinate.origin
//    }

//  private val DestructionSummaryCombined: ReadableProperty[(Coordinate, Int)] =
//    combine(DestructionSummaryPos, SummaryShipsSqSize)

//  private val shipsSummaryRelCoordinates: ReadableProperty[List[(ShipId, List[ViewShip])]] =
//    combine(shipsForSummaryProperty, SummaryMaxY).transform {
//      case (Some(shipsInThisGame), summaryMaxY) =>
//        def getShipsToPlacePos(
//            posX: Int,
//            posY: Int,
//            columnX: Int,
//            maxX: Int,
//            ships: List[List[Ship]],
//            currentList: List[ViewShip],
//            shipBefore: Option[Ship]
//        ): List[List[ViewShip]] =
//          ships match {
//            case (ship :: _) :: _ if posY > 0 && posY + ship.pieces.maxBy(_.y).y >= summaryMaxY =>
//              val newColumnX = maxX + 4
//              getShipsToPlacePos(
//                posX = newColumnX,
//                posY = 0,
//                columnX = newColumnX,
//                maxX = newColumnX,
//                ships,
//                currentList,
//                shipBefore
//              )
//            case (ship :: next) :: nextList =>
//              getShipsToPlacePos(
//                posX + ship.size.x + 1,
//                posY,
//                columnX,
//                Math.max(maxX, ship.pieces.map(_ + Coordinate(posX, posY)).maxBy(_.x).x),
//                next :: nextList,
//                ViewShip(ship, ship.pieces.map(_ + Coordinate(posX, posY))) :: currentList,
//                Some(ship)
//              )
//            case Nil :: nextList =>
//              currentList.reverse ::
//                getShipsToPlacePos(
//                  columnX,
//                  posY + shipBefore.map(_.size.y + 1).getOrElse(0),
//                  columnX,
//                  maxX,
//                  nextList,
//                  Nil,
//                  None
//                )
//            case Nil if currentList.nonEmpty =>
//              currentList.reverse :: Nil
//            case Nil =>
//              Nil
//          }
//
//        val shipsGrouped: List[(ShipId, List[Ship])] =
//          shipsInThisGame.groupBy(_.shipId).toList
//
//        val minX =
//          shipsGrouped.map { case (_, ships) =>
//            val ship = ships.head
//            val size = ships.size
//            ship.size.min * size + size - 1
//          }.max
//
//        val shipsListList: List[List[Ship]] =
//          shipsGrouped
//            .map { case (shipId, ships) =>
//              val ship = ships.head
//              val size = ships.size
//              val fullSizeX = ship.size.x * size + size - 1
//              val fullSizeY = ship.size.y * size + size - 1
//              if (
//                Math.max(minX, fullSizeX) * ship.size.y <= Math.max(minX, fullSizeY) * ship.size.x
//              )
//                (shipId, ships)
//              else
//                (shipId, ships.map(_.rotateBy(1)))
//            }
//            .sortBy(_._2.head.shipBiggestToSmallestOrder)
//            .map(_._2)
//
//        getShipsToPlacePos(
//          posX = 0,
//          posY = 0,
//          columnX = 0,
//          maxX = 0,
//          shipsListList,
//          Nil,
//          None
//        ).map { viewShipList => viewShipList.head.ship.shipId -> viewShipList }
//      case _ =>
//        Nil
//    }

//  private val allShipsToPlaceCoordinates: ReadableProperty[List[ToPlaceShip]] =
//    combine(
//      shipsSummaryRelCoordinates,
//      gameModel.subProp(_.shipsLeftToPlace),
//      gamePresenter.modeTypeProperty,
//      PlaceShipsPos,
//      SummaryShipsSqSize
//    ).transform {
//      case (
//            shipsSummary,
//            shipsLeftToPlace,
//            Some(PlacingGameModeType),
//            placeShipsPos,
//            placeShipsSqSize
//          ) =>
//        val shipsLeftToPlaceMap: Map[ShipId, Int] =
//          shipsLeftToPlace.groupBy(_.shipId).map { case (shipId, list) => shipId -> list.size }
//
//        val shipsPlaced: Map[ShipId, Int] =
//          shipsLeftToPlaceMap.map { case (shipId, shipLeftToPlace) =>
//            shipId ->
//              shipsSummary
//                .find(_._1 == shipId)
//                .map(_._2.size - shipLeftToPlace)
//                .getOrElse(0)
//          }
//
//        shipsSummary.flatMap { case (shipId, viewShipList) =>
//          viewShipList.zipWithIndex.map { case (viewShip, index) =>
//            ToPlaceShip(
//              viewShip.ship,
//              viewShip.pieces.map(relPieceCoor => placeShipsPos + relPieceCoor * placeShipsSqSize),
//              shipsPlaced.getOrElse(shipId, Int.MaxValue) > index
//            )
//          }
//        }
//      case _ =>
//        Nil
//    }

//  val shipToPlaceHover: ReadableProperty[Option[ToPlaceShip]] =
//    combine(gamePresenter.mousePositionProperty, allShipsToPlaceCoordinates, SummaryShipsSqSize)
//      .transform {
//        case (Some(mousePosition), shipsSummary, placeShipsSqSize) =>
//          val sizeCoor = Coordinate.square(placeShipsSqSize)
//          shipsSummary.find { case ToPlaceShip(_, pieces, alreadyPlaced) =>
//            !alreadyPlaced && pieces.exists(sqCoor =>
//              mousePosition >= sqCoor && mousePosition <= sqCoor + sizeCoor
//            )
//          }
//        case _ =>
//          None
//      }

//  val allShipsSummaryCoordinates
//      : ReadableProperty[List[(ShipId, Coordinate, Int, List[SummaryShip])]] =
//    combine(
//      shipsSummaryRelCoordinates,
//      gamePresenter.turnPlayHistory,
//      gamePresenter.modeTypeOrPuzzleProperty,
//      DestructionSummaryCombined,
//      DestructionSummaryHitCountSize
//    ).transform {
//      case (
//            shipsSummary,
//            Some(turnPlayHistory),
//            (Some(PlayingModeType | GameOverModeType), _) | (_, true),
//            (destructionSummaryPos, destructionSummarySqSize),
//            destructionSummaryHitCountSize
//          ) =>
//        val shipsDestroyed: Map[ShipId, Int] =
//          turnPlayHistory
//            .flatMap(_.hitHints.collect { case ShipHit(shipId, true) => shipId })
//            .groupBy(identity)
//            .map { case (shipId, list) => shipId -> list.size }
//
//        shipsSummary.map { case (shipId, viewShipList) =>
//          val summaryShips: List[SummaryShip] =
//            viewShipList.zipWithIndex.map { case (viewShip, index) =>
//              SummaryShip(
//                viewShip.ship,
//                viewShip.pieces.map(relPieceCoor =>
//                  destructionSummaryPos + relPieceCoor * destructionSummarySqSize
//                ),
//                shipsDestroyed.getOrElse(shipId, 0) > index
//              )
//            }
//
//          val headShip = summaryShips.head
//          val summaryCenter: Coordinate = {
//            val minX = headShip.pieces.minBy(_.x).x
//            val min = headShip.pieces.minBy(_.y).y
//            val max = headShip.pieces.maxBy(_.y).y
//            val centerY =
//              (max + min) / 2 + destructionSummarySqSize / 2 -
//                destructionSummaryHitCountSize / 2 + 1
//            Coordinate(
//              minX,
//              centerY
//            )
//          }
//
//          val hitCount: Int =
//            turnPlayHistory
//              .map(_.hitHints.count {
//                case ShipHit(shipHitId, _) if shipHitId == shipId => true
//                case _                                            => false
//              })
//              .sum - summaryShips.count(_.destroyed) * headShip.pieces.size
//
//          (shipId, summaryCenter, hitCount, summaryShips)
//        }
//      case _ =>
//        Nil
//    }
//
//  val summaryShipHover: ReadableProperty[Option[Ship]] =
//    combine(
//      gamePresenter.mousePositionProperty,
//      allShipsSummaryCoordinates,
//      SummaryShipsSqSize
//    ).transform {
//      case (Some(mousePosition), shipsSummary, destructionSummarySqSize) =>
//        val sizeCoor = Coordinate.square(destructionSummarySqSize)
//        shipsSummary
//          .find { case (_, _, _, summaryShips) =>
//            summaryShips.exists(
//              _.pieces.exists(sqCoor =>
//                mousePosition >= sqCoor && mousePosition <= sqCoor + sizeCoor
//              )
//            )
//          }
//          .map(_._4.head.ship)
//      case _ =>
//        None
//    }
//

  val mainBoardMouseCoordinate: ReadableProperty[Option[Coordinate]] =
    combine(
      gamePresenter.mousePositionProperty,
      gamePresenter.meProperty.transform(_.map(_.myBoard.boardSize)),
      gamePresenter.modeTypeProperty,
      MainBoardSquareSize,
      MainBoardPos
    ).transform {
      case (
            Some(mousePosition),
            Some(boardSize),
            Some(_),
            preGameSquareSize,
            myBoardPosPreGame
          ) =>
        val relativeBoardCoor = mousePosition - myBoardPosPreGame
        Some(relativeBoardCoor)
          .map(_ / preGameSquareSize)
          .map(_.roundTo(boardSize))
      case _ =>
        None
    }

//  val enemyBoardMouseCoordinate: ReadableProperty[Option[Coordinate]] =
//    combine(
//      gamePresenter.mousePositionProperty,
//      gamePresenter.mainBoardSizeProperty,
//      gamePresenter.modeTypeOrPuzzleProperty,
//      MainBoardPos,
//      EnemyBoardSqSize
//    ).transform {
//      case (
//            Some(mousePosition),
//            Some(boardSize),
//            (Some(PlayingModeType | GameOverModeType), _) | (_, true),
//            enemyBoardPos,
//            defaultSquareSize
//          ) =>
//        val relativeBoardCoor = mousePosition - enemyBoardPos
//        Some(relativeBoardCoor)
//          .filter(coor =>
//            coor >= -PlaceShipBoardMargin &&
//              coor <= (boardSize * defaultSquareSize + PlaceShipBoardMargin)
//          )
//          .map(_ / defaultSquareSize)
//          .map(_.roundTo(boardSize))
//      case _ =>
//        None
//    }

//  private val BoardMarksSelectorAllPositions: ReadableProperty[List[(GameAction, Coordinate)]] =
//    combine(
//      gamePresenter.modeTypeOrPuzzleProperty,
//      BoardMarksSelectorCombined
//    ).transform {
//      case (
//            (Some(PlayingModeType | GameOverModeType), _) | (_, true),
//            (boardMarksSelectorPos, boardMarksSelectorSize, boardMarksSelectorMargin)
//          ) =>
//        MarksSelectorOrder.zipWithIndex.map { case (boardMark, index) =>
//          (
//            boardMark,
//            boardMarksSelectorPos +
//              Coordinate(index * (boardMarksSelectorSize + boardMarksSelectorMargin), 0)
//          )
//        }
//      case _ =>
//        Nil
//    }

//  val boardMarkHover: ReadableProperty[Option[GameAction]] =
//    combine(
//      gamePresenter.mousePositionProperty,
//      gamePresenter.modeTypeOrPuzzleProperty,
//      BoardMarksSelectorAllPositions,
//      BoardMarksSelectorSize
//    ).transform {
//      case (
//            Some(mousePosition),
//            (Some(PlayingModeType | GameOverModeType), _) | (_, true),
//            boardMarksSelectorAllPositions,
//            boardMarksSelectorSize
//          ) =>
//        boardMarksSelectorAllPositions
//          .find { case (_, position) =>
//            mousePosition >= position &&
//            mousePosition <= position + Coordinate.square(boardMarksSelectorSize)
//          }
//          .map(_._1)
//      case _ =>
//        None
//    }

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

//
//  def paint(): Unit = {
//    val GameModel(
//      _,
//      mousePositionOpt,
//      _,
//      selectedShipOpt,
//      turnAttacks,
//      _,
//      selectedAction,
//      _,
//      _,
//      _
//    ) = gameModel.get
//
//    val screenModelData = screenModel.get
//    val translationsData = translationsModel.get
//
//    val renderingCtx = myBoardCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
//
//    renderingCtx.clearRect(0, 0, myBoardCanvas.width, myBoardCanvas.height)
//
//    gamePresenter.gameStateProperty.get match {
//      case Some(GameState(_, rules, me, enemy, _: PlacingShipsMode)) =>
//        drawMyBoard(
//          renderingCtx,
//          translationsData.myBoardTitle.innerText,
//          me.myBoard,
//          enemy.turnPlayHistory,
//          mousePositionOpt,
//          selectedShipOpt,
//          MyBoardPreGamePos.get,
//          SquareSizeBig.get,
//          fillEmptySquares = false,
//          hideMyBoard = false,
//          isMyTurn = false,
//          tick = screenModelData.tick
//        )
//        drawRulesSummary(
//          renderingCtx,
//          rules,
//          translationsData
//        )
//      case Some(GameState(_, _, me, enemy, PlayingMode(isMyTurn, _, _, _, _))) =>
//        drawMissiles(renderingCtx, turnAttacks, screenModelData.missilesPopupMillisOpt)
//        drawDestructionSummary(renderingCtx, selectedShipOpt)
//
//        drawMyBoard(
//          renderingCtx,
//          translationsData.myBoardTitle.innerText,
//          me.myBoard,
//          enemy.turnPlayHistory,
//          None,
//          None,
//          MyBoardInGamePos.get,
//          SquareSizeMedium.get,
//          fillEmptySquares = true,
//          hideMyBoard = screenModelData.hideMyBoard,
//          isMyTurn = !isMyTurn,
//          tick = screenModelData.tick
//        )
//
//        drawEnemyBoard(
//          renderingCtx,
//          translationsData.enemyBoardTitle.innerText,
//          me,
//          turnAttacks,
//          EnemyBoardPos.get,
//          selectedAction,
//          selectedShipOpt,
//          screenModelData.hoverMove,
//          isMyTurn = isMyTurn,
//          tick = screenModelData.tick
//        )
//
//        drawBoardMarksSelector(renderingCtx, selectedAction, showShootSelector = true)
//        drawExtraTurnPopup(
//          renderingCtx,
//          turnAttacks,
//          screenModelData.extraTurnPopup,
//          translationsData.extraTurnText.innerText
//        )
//      case Some(GameState(_, _, me, enemy, GameOverMode(_, _, _, _, enemyRealBoard))) =>
//        drawDestructionSummary(renderingCtx, selectedShipOpt)
//
//        if (screenModelData.revealEnemyBoard)
//          drawGameOverEnemyBoard(
//            renderingCtx,
//            translationsData.realEnemyBoardTitle.innerText,
//            me,
//            enemyRealBoard,
//            MyBoardGameOverPos.get,
//            SquareSizeBig.get,
//            selectedShipOpt,
//            screenModelData.hoverMove
//          )
//        else
//          drawMyBoard(
//            renderingCtx,
//            translationsData.myBoardTitle.innerText,
//            me.myBoard,
//            enemy.turnPlayHistory,
//            None,
//            None,
//            MyBoardGameOverPos.get,
//            SquareSizeBig.get,
//            fillEmptySquares = true,
//            hideMyBoard = false,
//            isMyTurn = false,
//            tick = screenModelData.tick
//          )
//
//        drawEnemyBoard(
//          renderingCtx,
//          translationsData.enemyBoardTitle.innerText,
//          me,
//          Nil,
//          EnemyBoardPos.get,
//          selectedAction,
//          selectedShipOpt,
//          screenModelData.hoverMove,
//          isMyTurn = false,
//          tick = screenModelData.tick
//        )
//
//        drawBoardMarksSelector(renderingCtx, selectedAction, showShootSelector = false)
//      case None =>
//        gamePresenter.gamePuzzleStateProperty.get match {
//          case Some(
//                GamePuzzleState(
//                  puzzleSolvedCounter,
//                  _,
//                  enemyBoardMarks,
//                  playerPuzzle,
//                  puzzleSolutionOpt
//                )
//              ) =>
//            drawDestructionSummary(renderingCtx, selectedShipOpt)
//
//            drawEnemyBoard(
//              renderingCtx,
//              "",
//              Player(
//                Board(playerPuzzle.boardSize, Nil),
//                enemyBoardMarks,
//                playerPuzzle.turnPlayHistory
//              ),
//              Nil,
//              EnemyBoardPos.get,
//              selectedAction,
//              selectedShipOpt,
//              screenModelData.hoverMove,
//              isMyTurn = false,
//              tick = screenModelData.tick
//            )
//
//            drawBoardMarksSelector(renderingCtx, selectedAction, showShootSelector = false)
//
//            puzzleSolutionOpt match {
//              case None =>
//                drawPlayerPuzzleObjective(renderingCtx, playerPuzzle, translationsData)
//              case Some((PuzzleSolution.CorrectShipBoardMarksSolution(board), correctState)) =>
//                drawMyBoard(
//                  renderingCtx,
//                  "Solution",
//                  board,
//                  Nil,
//                  None,
//                  None,
//                  MyBoardGameOverPos.get,
//                  SquareSizeBig.get,
//                  fillEmptySquares = true,
//                  hideMyBoard = false,
//                  isMyTurn = false,
//                  tick = screenModelData.tick
//                )
//
//                drawPuzzleCorrectSolution(renderingCtx, correctState, translationsData)
//              case _ =>
//            }
//
//            drawPuzzleCounter(renderingCtx, puzzleSolvedCounter, translationsData)
//
//          case None =>
//        }
//    }
//  }

  def clearCanvas(canvas: Canvas): Unit = {
    val renderingCtx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    renderingCtx.clearRect(0, 0, canvas.width, canvas.height)
  }

  def drawPuzzleBoardDiv: Div = {
    drawMainBoardDiv
  }

  def drawMainBoardDiv: Div = {
    combine(
      gamePresenter.gameStateProperty,
      combine(
        screenModel.subProp(_.tick),
        screenModel.subProp(_.hoverMove),
        MainBoardPos,
        MainBoardSquareSize,
        MainBoardTurnTextSize
      ),
      combine(
        gameModel.subProp(_.mousePosition),
        gameModel.subProp(_.selectedShip),
        gameModel.subProp(_.selectedAction),
        gameModel.subProp(_.turnAttacks),
      ),
    ).listen {
      case (
            Some(GameState(_, _, me, _, _: PlacingShipsMode)),
            (_, _, boardPos, squareSize, _),
            (mousePositionOpt, selectedShipOpt, _, _)
          ) =>
        clearCanvas(mainBoardCanvas)
        drawPlaceShipsBoard(
          canvas = mainBoardCanvas,
          myBoard = me.myBoard,
          boardPosition = boardPos,
          squareSize = squareSize,
          mousePositionOpt = mousePositionOpt,
          selectedShipOpt = selectedShipOpt,
          hideMyBoard = false,
        )
      case (
            Some(GameState(_, _, me, _, PlayingMode(isMyTurn, _, _, _, _))),
            (screenModelDataTick, hoverMoveOpt, boardPos, squareSize, textSize),
            (_, selectedShipOpt, selectedAction, turnAttacks),
          ) =>
        clearCanvas(mainBoardCanvas)
        drawEnemyBoard(
          canvas = mainBoardCanvas,
          me = me,
          boardPosition = boardPos,
          squareSize = squareSize,
          textSize = textSize,
          turnAttacks = turnAttacks,
          selectedAction = selectedAction,
          selectedShipOpt = selectedShipOpt,
          hoverMoveOpt = hoverMoveOpt,
          isMyTurn = isMyTurn,
          tick = screenModelDataTick,
        )
      case (
            Some(GameState(_, _, me, _, GameOverMode(_, _, _, _, _))),
            (screenModelDataTick, hoverMoveOpt, boardPos, squareSize, textSize),
            (_, selectedShipOpt, selectedAction, _),
          ) =>
        clearCanvas(mainBoardCanvas)
        drawEnemyBoard(
          canvas = mainBoardCanvas,
          me = me,
          boardPosition = boardPos,
          squareSize = squareSize,
          textSize = textSize,
          turnAttacks = Nil,
          selectedAction = selectedAction,
          selectedShipOpt = selectedShipOpt,
          hoverMoveOpt = hoverMoveOpt,
          isMyTurn = false,
          tick = screenModelDataTick,
        )
      case _ =>
    }

    div(
      mainBoardCanvas
    ).render
  }

  def drawSmallBoardDiv: Div = {
    combine(
      gamePresenter.gameStateProperty,
      combine(
        screenModel.subProp(_.tick),
        screenModel.subProp(_.hideMyBoard),
        screenModel.subProp(_.revealEnemyBoard),
        screenModel.subProp(_.hoverMove)
      ),
      combine(
        SmallBoardPos,
        SmallBoardSquareSize,
        SmallBoardTurnTextSize,
      ),
      gameModel.subProp(_.selectedShip)
    ).listen {
      case (
            Some(GameState(_, _, me, enemy, PlayingMode(isMyTurn, _, _, _, _))),
            (screenModelDataTick, hideMyBoard, _, _),
            (boardPos, squareSize, textSize),
            _
          ) =>
        clearCanvas(smallBoardCanvas)
        drawMyBoard(
          canvas = smallBoardCanvas,
          myBoard = me.myBoard,
          boardPosition = boardPos,
          squareSize = squareSize,
          textSize = textSize,
          turnPlayHistory = enemy.turnPlayHistory,
          mousePositionOpt = None,
          fillEmptySquares = true,
          hideMyBoard = hideMyBoard,
          isMyTurn = !isMyTurn,
          tick = screenModelDataTick
        )
      case (
            Some(GameState(_, _, me, _, GameOverMode(_, _, _, _, enemyRealBoard))),
            (_, _, true, hoverMoveOpt),
            (boardPos, squareSize, textSize),
            selectedShipOpt
          ) =>
        clearCanvas(smallBoardCanvas)
        drawGameOverEnemyBoard(
          canvas = smallBoardCanvas,
          me = me,
          boardPosition = boardPos,
          squareSize = squareSize,
          textSize = textSize,
          enemyShips = enemyRealBoard,
          selectedShipOpt = selectedShipOpt,
          hoverMoveOpt = hoverMoveOpt,
        )
      case (
            Some(GameState(_, _, me, enemy, GameOverMode(_, _, _, _, _))),
            (screenModelDataTick, _, false, _),
            (boardPos, squareSize, textSize),
            _
          ) =>
        clearCanvas(smallBoardCanvas)
        drawMyBoard(
          canvas = smallBoardCanvas,
          myBoard = me.myBoard,
          boardPosition = boardPos,
          squareSize = squareSize,
          textSize = textSize,
          turnPlayHistory = enemy.turnPlayHistory,
          mousePositionOpt = None,
          fillEmptySquares = true,
          hideMyBoard = false,
          isMyTurn = false,
          tick = screenModelDataTick
        )
      case _ =>
    }

    div(
      `class` := "d-flex justify-content-center position-relative",
      smallBoardCanvas,
//      div(
//        `class` := "position-absolute",
//        style := "top: 50%; left: 50%",
//        h1("Hidden")
//      )
    ).render
  }

  def drawPlaceShipsBoard(
      canvas: Canvas,
      myBoard: Board,
      boardPosition: Coordinate,
      squareSize: Int,
      mousePositionOpt: Option[Coordinate],
      selectedShipOpt: Option[Ship],
      hideMyBoard: Boolean
  ): Unit = {
    val renderingCtx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val boardSize = myBoard.boardSize

    drawBoardLimits(
      renderingCtx = renderingCtx,
      boardSize = boardSize,
      boardPosition = boardPosition,
      squareSize = squareSize,
      backgroundColor = None,
      drawAsSelectedTick = None
    )

    if (!hideMyBoard) {
      myBoardWaterCoordinatesSeqProperty.get.foreach { case Coordinate(x, y) =>
        drawBoardSquare(
          renderingCtx,
          boardPosition,
          Coordinate(x, y),
          squareSize,
          CanvasColor.Water()
        )
      }

      myBoard.ships.foreach { case ShipInBoard(ship, position) =>
        ship.pieces
          .map(_ + position)
          .foreach(drawBoardSquare(renderingCtx, boardPosition, _, squareSize, CanvasColor.Ship()))
      }
    }

    (mousePositionOpt, selectedShipOpt) match {
      case (Some(mousePosition), Some(ship)) =>
        mainBoardMouseCoordinate.get match {
          case Some(boardCoor) =>
            val roundedBoardCoor =
              boardCoor.roundTo(boardSize - ship.size + Coordinate(1, 1))

            def drawCoordinate(coor: Coordinate): Unit = {
              val alpha = if (canPlaceInBoard(myBoard, ship, roundedBoardCoor)) 0.9 else 0.75
              drawBoardSquare(
                renderingCtx,
                boardPosition,
                coor,
                squareSize,
                CanvasColor.Ship(CanvasBorder.Standard(alpha = alpha), alpha = alpha)
              )
            }

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

  def drawMyBoard(
      canvas: Canvas,
      myBoard: Board,
      boardPosition: Coordinate,
      squareSize: Int,
      textSize: Int,
      turnPlayHistory: List[TurnPlay],
      mousePositionOpt: Option[Coordinate],
      fillEmptySquares: Boolean,
      hideMyBoard: Boolean,
      isMyTurn: Boolean,
      tick: Int
  ): Unit = {
    val renderingCtx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val boardSize = myBoard.boardSize

    drawBoardLimits(
      renderingCtx,
      boardSize,
      boardPosition,
      squareSize,
      if (fillEmptySquares && !hideMyBoard) Some(CanvasColor.Water()) else None,
      Some(tick).filter(_ => isMyTurn)
    )

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

      myBoard.ships.foreach { case ShipInBoard(ship, position) =>
        ship.pieces
          .map(_ + position)
          .foreach(drawBoardSquare(renderingCtx, boardPosition, _, squareSize, CanvasColor.Ship()))
      }
    }

    turnPlayHistory.zipWithIndex.foreach { case (TurnPlay(turn, turnAttacks, _), index) =>
      turnAttacks.foreach {
        case Attack(attackType, Some(coor)) =>
          if (attackType == AttackType.Simple)
            drawTurnNumberCoor(
              renderingCtx = renderingCtx,
              boardPosition = boardPosition,
              coor = coor,
              size = squareSize,
              turn = turn,
              textSize = textSize
            )
          if (index == 0) {
            if (attackType == AttackType.Radar)
              drawImageAbs(
                renderingCtx,
                radarImage.element,
                x = boardPosition.x + coor.x * squareSize + 2,
                y = boardPosition.y + coor.y * squareSize + 2,
                squareSize - 4,
                squareSize - 4,
                useAntiAliasing = true
              )

            drawBoardSquare(
              renderingCtx,
              boardPosition,
              coor,
              squareSize,
              CanvasColor.White(CanvasBorder.RedBold())
            )
          }
        case _ =>
      }
    }
  }

  def drawEnemyBoard(
      canvas: Canvas,
      me: Player,
      boardPosition: Coordinate,
      squareSize: Int,
      textSize: Int,
      turnAttacks: List[Attack],
      selectedAction: GameAction,
      selectedShipOpt: Option[Ship],
      hoverMoveOpt: Option[Turn],
      isMyTurn: Boolean,
      tick: Int
  ): Unit = {
    val renderingCtx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    drawBoardLimits(
      renderingCtx,
      me.myBoard.boardSize,
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
          textSize = textSize
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
        case _ if hoverMoveOpt.contains(turnPlay.turn) =>
          drawTurn(CanvasColor.White(CanvasBorder.DashBlue()))
        case Some(ship) if hoverMoveOpt.isEmpty && turnPlay.hitHints.exists {
              case ShipHit(shipId, _) if ship.shipId == shipId => true
              case _                                           => false
            } =>
          drawTurn(CanvasColor.White(CanvasBorder.DashRed()))
        case None if index == 0 =>
          drawTurn(CanvasColor.White(CanvasBorder.RedBold()))
        case _ =>
      }
    }

    mainBoardMouseCoordinate.get.foreach { enemyBoardCoor =>
      selectedAction match {
        case GameAction.ShotSelector
            if turnAttacks.exists(!_.isPlaced) &&
              gamePresenter.isValidCoordinateTarget(enemyBoardCoor) =>
          turnAttacks.find(_.coordinateOpt.isEmpty).foreach { case Attack(attackType, _) =>
            val image: CanvasImage =
              if (attackType == AttackType.Simple) attackSimpleImage else radarImage
            drawImageAbs(
              renderingCtx,
              image.element,
              x = boardPosition.x + enemyBoardCoor.x * squareSize + 2,
              y = boardPosition.y + enemyBoardCoor.y * squareSize + 2,
              squareSize - 4,
              squareSize - 4,
              useAntiAliasing = true,
              alpha = 0.7
            )
          }
        case _ =>
          val boardMark = me.enemyBoardMarks(enemyBoardCoor.x)(enemyBoardCoor.y)._2
          (boardMark, selectedAction) match {
            case (
                  BoardMark.ShipHit | BoardMark.ManualShip,
                  GameAction.FillWaterSelector
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
                selectedAction match {
                  case GameAction.ManualWaterSelector => CanvasColor.Water(alpha = 0.5)
                  case GameAction.ManualShipSelector  => CanvasColor.Ship(alpha = 0.5)
                  case _                              => CanvasColor.White()
                }
              drawBoardSquare(renderingCtx, boardPosition, enemyBoardCoor, squareSize, canvasColor)
            case _ =>
          }
      }
    }

    turnAttacks.foreach {
      case Attack(attackType, Some(enemyBoardCoor)) =>
        val image: CanvasImage =
          if (attackType == AttackType.Simple) attackSimpleImage else radarImage

        drawImageAbs(
          renderingCtx,
          image.element,
          x = boardPosition.x + enemyBoardCoor.x * squareSize + 2,
          y = boardPosition.y + enemyBoardCoor.y * squareSize + 2,
          squareSize - 4,
          squareSize - 4,
          useAntiAliasing = true
        )
        drawBoardSquare(
          renderingCtx,
          boardPosition,
          enemyBoardCoor,
          squareSize,
          CanvasColor.White(CanvasBorder.DashBlack())
        )
      case _ =>
    }
  }

  def drawGameOverEnemyBoard(
      canvas: Canvas,
      me: Player,
      boardPosition: Coordinate,
      squareSize: Int,
      textSize: Int,
      enemyShips: List[ShipInBoard],
      selectedShipOpt: Option[Ship],
      hoverMoveOpt: Option[Turn]
  ): Unit = {
    val renderingCtx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val boardSize = me.myBoard.boardSize

    drawBoardLimits(
      renderingCtx,
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
          textSize = textSize
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
        case _ if hoverMoveOpt.contains(turnPlay.turn) =>
          drawTurn(CanvasColor.White(CanvasBorder.DashBlue()))
        case Some(ship) if hoverMoveOpt.isEmpty && turnPlay.hitHints.exists {
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

//  def drawBoardMarksSelector(
//      renderingCtx: CanvasRenderingContext2D,
//      selectedAction: GameAction,
//      showShootSelector: Boolean
//  ): Unit = {
//    val boardMarksSize = BoardMarksSelectorSize.get
//
//    BoardMarksSelectorAllPositions.get.foreach { case (boardMark, position) =>
//      val canvasBorder: CanvasBorder =
//        if (selectedAction == boardMark)
//          CanvasBorder.RedBold()
//        else
//          CanvasBorder.Standard()
//
//      val canvasColorOpt =
//        boardMark match {
//          case GameAction.ManualWaterSelector =>
//            Some(CanvasColor.Water(canvasBorder))
//          case GameAction.ManualShipSelector | GameAction.FillWaterSelector =>
//            Some(CanvasColor.Ship(canvasBorder))
//          case GameAction.ShotSelector if showShootSelector =>
//            Some(CanvasColor.White(canvasBorder))
//          case _ =>
//            None
//        }
//
//      canvasColorOpt.foreach { canvasColor =>
//        drawBoardSquare(renderingCtx, position, Coordinate.origin, boardMarksSize, canvasColor)
//      }
//
//      boardMark match {
//        case GameAction.FillWaterSelector =>
//          drawImageAbs(
//            renderingCtx,
//            fillWaterImage.element,
//            x = position.x + 3,
//            y = position.y + 3,
//            boardMarksSize - 6,
//            boardMarksSize - 6,
//            useAntiAliasing = true
//          )
//        case GameAction.ShotSelector if showShootSelector =>
//          drawImageAbs(
//            renderingCtx,
//            attackSimpleImage.element,
//            x = position.x + 3,
//            y = position.y + 3,
//            boardMarksSize - 6,
//            boardMarksSize - 6,
//            useAntiAliasing = true
//          )
//        case _ =>
//      }
//    }
//  }

//  def drawRulesSummary(
//      renderingCtx: CanvasRenderingContext2D,
//      rules: Rules,
//      translationsData: TranslationsModel
//  ): Unit = {
//    val textSize = 23
//    val lineMargin = 7
//
//    renderingCtx.fillStyle = s"rgb(0, 0, 0)"
//    renderingCtx.font = s"${textSize}px serif"
//    renderingCtx.textBaseline = "middle"
//    renderingCtx.textAlign = "right"
//
//    val initialCoordinate: Coordinate =
//      screenModel.get.mainBoardCanvasSize.map { case Coordinate(x, _) =>
//        Coordinate(x - SquareSizeBig.get, SquareSizeBig.get)
//      }
//
//    val rulesDataList: List[String] =
//      List(
//        rules.timeLimit match {
//          case WithoutRuleTimeLimit =>
//            List(translationsData.withoutRuleTimeLimit.innerText)
//          case WithRuleTimeLimit(
//                initialTotalTimeSeconds,
//                additionalTurnTimeSecondsOpt
//              ) =>
//            List(
//              translationsData.withRuleTimeLimit.innerText,
//              List(
//                initialTotalTimeSeconds,
//                translationsData.seconds.innerText,
//                translationsData.totalTime.innerText
//              ).mkString(" ")
//            ) ++
//              additionalTurnTimeSecondsOpt.map { case (additionalTurnTimeSeconds, _) =>
//                List(
//                  s"+$additionalTurnTimeSeconds",
//                  translationsData.seconds.innerText,
//                  translationsData.eachTurn.innerText
//                ).mkString(" ")
//              }.toList
//        },
//        List(""),
//        List(s"${translationsData.amountOfShots.innerText}: ${rules.defaultTurnAttacks.size}"),
//        List(""),
//        Some(rules.gameBonuses.nonEmpty)
//          .map(_ => translationsData.turnBonuses.innerText + ":")
//          .toList, {
//          def rewardsToString(bonusRewardList: List[BonusReward]): String =
//            bonusRewardList
//              .map { case BonusReward.ExtraTurn(attackTypes) =>
//                s"${attackTypes.size} ${translationsData.shots.innerText}"
//              }
//              .mkString(", ")
//
//          rules.gameBonuses.map {
//            case GameBonus(BonusType.FirstBlood, bonusRewardList) =>
//              s"${translationsData.bonusFirstBlood.innerText}: ${rewardsToString(bonusRewardList)}"
//            case GameBonus(BonusType.DoubleKill, bonusRewardList) =>
//              s"${translationsData.bonusDoubleKill.innerText}: ${rewardsToString(bonusRewardList)}"
//            case GameBonus(BonusType.TripleKill, bonusRewardList) =>
//              s"${translationsData.bonusTripleKill.innerText}: ${rewardsToString(bonusRewardList)}"
//            case GameBonus(BonusType.UltraKill, bonusRewardList) =>
//              s"${translationsData.bonusUltraKill.innerText}: ${rewardsToString(bonusRewardList)}"
//          }
//        }
//      ).flatten
//
//    rulesDataList.foldLeft(initialCoordinate) {
//      case (Coordinate(lineLeftPosX, lineLeftPosY), lineStr) =>
//        renderingCtx.fillText(lineStr, lineLeftPosX, lineLeftPosY)
//        Coordinate(lineLeftPosX, lineLeftPosY + textSize + lineMargin)
//    }
//  }
//
//  def drawPlayerPuzzleObjective(
//      renderingCtx: CanvasRenderingContext2D,
//      playerPuzzle: PlayerPuzzle,
//      translationsData: TranslationsModel
//  ): Unit = {
//    val textSize = 23
//    val lineMargin = 15
//
//    val initialCoordinateText: Coordinate =
//      screenModel.get.mainBoardCanvasSize.map { case Coordinate(x, _) =>
//        Coordinate(x - SquareSizeSmall.get, SquareSizeBig.get + 100)
//      }
//
//    playerPuzzle.puzzleObjective match {
//      case PuzzleObjective.CorrectShipBoardMarks =>
//        renderingCtx.fillStyle = s"rgb(0, 0, 0)"
//        renderingCtx.font = s"bold ${textSize}px serif"
//        renderingCtx.textBaseline = "middle"
//        renderingCtx.textAlign = "right"
//        renderingCtx.fillText(
//          translationsData.placeMarksCorrectly1.innerText,
//          initialCoordinateText.x,
//          initialCoordinateText.y
//        )
//
//        renderingCtx.fillText(
//          s"${translationsData.placeMarksCorrectly2.innerText} '${translationsData.sendPuzzleAnswer.innerText}'",
//          initialCoordinateText.x,
//          initialCoordinateText.y + textSize + lineMargin
//        )
//      case PuzzleObjective.WinInXTurns(maximumTurns) =>
//        ???
//    }
//  }
//
//  def drawPuzzleCounter(
//      renderingCtx: CanvasRenderingContext2D,
//      puzzleSolvedCounter: Int,
//      translationsData: TranslationsModel
//  ): Unit = {
//    val textSize = 23
//
//    val counterPosition: Coordinate =
//      screenModel.get.mainBoardCanvasSize.map { case Coordinate(x, y) =>
//        Coordinate(x / 2, y - textSize)
//      }
//
//    renderingCtx.fillStyle = s"rgb(0, 0, 0)"
//    renderingCtx.font = s"bold ${textSize}px serif"
//    renderingCtx.textBaseline = "bottom"
//    renderingCtx.textAlign = "center"
//    renderingCtx.fillText(
//      s"${translationsData.solvedPuzzles.innerText}: $puzzleSolvedCounter",
//      counterPosition.x,
//      counterPosition.y
//    )
//  }

//  def drawPuzzleCorrectSolution(
//      renderingCtx: CanvasRenderingContext2D,
//      correctState: Boolean,
//      translationsData: TranslationsModel
//  ): Unit = {
//    val textSize = 23
//    renderingCtx.fillStyle =
//      if (correctState)
//        s"rgb(${CanvasColor.DarkGreen().fillColor})"
//      else
//        s"rgb(${CanvasColor.Red().fillColor})"
//    renderingCtx.font = s"bold ${textSize}px serif"
//    renderingCtx.textBaseline = "bottom"
//    renderingCtx.textAlign = "right"
//
//    val initialCoordinate: Coordinate =
//      screenModel.get.mainBoardCanvasSize.map { case Coordinate(x, y) =>
//        Coordinate(x - SquareSizeBig.get, y - textSize)
//      }
//
//    val text: String =
//      if (correctState)
//        translationsData.puzzleCorrect.innerText
//      else
//        translationsData.puzzleIncorrect.innerText
//
//    renderingCtx.fillText(text, initialCoordinate.x, initialCoordinate.y)
//  }

  def createFleetPlacePreview(nested: NestedInterceptor): Binding = {
    nested(
      produce(
        combine(
          preGameModel.subProp(_.rules).transform(_.gameFleet),
          screenModel.subProp(_.mainBoardCanvasSize),
          gameModel.subProp(_.shipsLeftToPlace),
        )
      ) { case (gameFleet, canvasSize, shipsLeftToPlace) =>
        val fleetSorted: List[(Ship, Int)] =
          gameFleet.shipCounterList
            .filter(_._2._1 > 0)
            .map { case (shipId, (amount, rotation)) => (Ship.getShip(shipId, rotation), amount) }
            .sortBy { case (ship, _) => (ship.piecesSize, ship.shipId.id) }

        val maxTotalHeight1column = 40
        val totalFleetYSize = Math.max(10, fleetSorted.map(_._1.size.y).sum + fleetSorted.size)
        val (previewSqSize, twoColumns) =
          if (totalFleetYSize <= maxTotalHeight1column)
            (Math.max(7, canvasSize.y / totalFleetYSize - 2), false)
          else
            (Math.max(7, canvasSize.y / (totalFleetYSize / 2) - 2), true)

        def createShipCanvas(ship: Ship, placed: Boolean): Canvas = {
          val canvasSize: Coordinate = ship.size * previewSqSize + Coordinate.square(4)
          val canvasColor: CanvasColor.Ship =
            if (placed)
              CanvasColor.Ship(CanvasBorder.Standard(alpha = 0.4), alpha = 0.4)
            else
              CanvasColor.Ship()

          viewUtils.createShipCanvas(
            canvasSize,
            previewSqSize,
            ship,
            destroyed = false,
            centerXCanvas = true,
            centerYCanvas = true,
            drawRadar = false,
            canvasColor = canvasColor
          )
        }

        val shipsLeftToPlaceMap: Map[ShipId, Int] =
          shipsLeftToPlace.groupBy(_.shipId).map { case (shipId, list) => shipId -> list.size }

        val shipsPlaced: Map[ShipId, Int] =
          shipsLeftToPlaceMap.map { case (shipId, shipLeftToPlace) =>
            shipId ->
              gameFleet.shipCounterMap
                .get(shipId)
                .map(_._1 - shipLeftToPlace)
                .getOrElse(0)
          }

        val fleetDivs: List[Div] =
          fleetSorted.map { case (ship, amount) =>
            val amountPlaced: Int = shipsPlaced.getOrElse(ship.shipId, amount)
            val amountLeftToPlace: Int = shipsLeftToPlaceMap.getOrElse(ship.shipId, 0)

            div(
              `class` := (if (twoColumns) "col-6" else "col-12"),
              div(
                (1 to amountPlaced).map(_ => createShipCanvas(ship, placed = true)),
                (1 to amountLeftToPlace).map(_ => createShipCanvas(ship, placed = false))
              )
            ).render.tap { shipDiv =>
              if (amountLeftToPlace > 0)
                shipDiv.onclick = _ => {
                  gameModel.subProp(_.selectedShip).set(Some(ship))
                }
            }
          }

        div(
          `class` := "d-flex align-items-start",
          div(
            `class` := "row mx-0 my-3",
            fleetDivs
          )
        ).render
      }
    )
  }

  def createFleetPreview(nested: NestedInterceptor): Binding = {
    nested(
      produce(
        combine(
          preGameModel.subProp(_.rules).transform(_.gameFleet),
          screenModel.subProp(_.mainBoardCanvasSize),
          gamePresenter.meProperty
        )
      ) {
        case (gameFleet, canvasSize, Some(Player(_, _, turnPlayHistory))) =>
          val shipsDestroyed: Map[ShipId, Int] =
            turnPlayHistory
              .flatMap(_.hitHints)
              .flatMap(_.shipIdDestroyedOpt)
              .groupBy(identity)
              .map { case (shipId, list) =>
                shipId -> list.size
              }

          val extraShipHits: Map[ShipId, Int] =
            turnPlayHistory
              .flatMap(_.hitHints)
              .flatMap(_.shipIdOpt)
              .groupBy(identity)
              .map { case (shipId, list) =>
                val piecesSize = Ship.getShip(shipId, Rotation.Rotation0).piecesSize
                shipId -> (list.size - shipsDestroyed.getOrElse(shipId, 0) * piecesSize)
              }

          val fleetSorted: List[(Ship, Int)] =
            gameFleet.shipCounterList
              .filter(_._2._1 > 0)
              .map { case (shipId, (amount, rotation)) => (Ship.getShip(shipId, rotation), amount) }
              .sortBy { case (ship, _) => (ship.piecesSize, ship.shipId.id) }

          val maxTotalHeight1column = 40
          val totalFleetYSize = Math.max(10, fleetSorted.map(_._1.size.y).sum + fleetSorted.size)
          val (previewSqSize, twoColumns) =
            if (totalFleetYSize <= maxTotalHeight1column)
              (Math.max(7, canvasSize.y / totalFleetYSize - 2), false)
            else
              (Math.max(7, canvasSize.y / (totalFleetYSize / 2) - 2), true)

          def createShipCanvas(ship: Ship, destroyed: Boolean): Canvas = {
            val canvasSize: Coordinate = ship.size * previewSqSize + Coordinate.square(4)
            viewUtils.createShipCanvas(
              canvasSize,
              previewSqSize,
              ship,
              destroyed = destroyed,
              centerXCanvas = true,
              centerYCanvas = true,
              drawRadar = false
            )
          }

          def createShipExtraHits(shipId: ShipId): JsDom.TypedTag[Div] =
            extraShipHits.get(shipId) match {
              case Some(shipHits) if shipHits > 0 =>
                div(
                  fontSize := "30px",
                  span("+", shipHits)
                )
              case _ =>
                div(
                  `class` := "invisible",
                  fontSize := "30px",
                  span("+0")
                )
            }

          val fleetDivs: List[Div] =
            fleetSorted.map { case (ship, amount) =>
              val amountDestroyed = shipsDestroyed.getOrElse(ship.shipId, 0)
              val amountAlive = amount - amountDestroyed

              div(
                `class` := (if (twoColumns) "col-6" else "col-12") + " p-0",
                div(
                  `class` := "d-flex align-items-center",
                  GameStyles.unselectableText,
                  createShipExtraHits(ship.shipId),
                  (1 to amountDestroyed).map(_ => createShipCanvas(ship, destroyed = true)) ++
                    (1 to amountAlive).map(_ => createShipCanvas(ship, destroyed = false))
                )
              ).render
            }

          div(
            `class` := "d-flex align-items-start",
            div(
              `class` := "row mx-0 my-3",
              fleetDivs
            )
          ).render
        case _ =>
          div.render
      }
    )
  }

}

object BoardView {

//  case class ViewShip(ship: Ship, pieces: List[Coordinate])

//  case class ToPlaceShip(ship: Ship, pieces: List[Coordinate], alreadyPlaced: Boolean)

//  case class SummaryShip(ship: Ship, pieces: List[Coordinate], destroyed: Boolean)

  val MinTextSize = 15

  sealed trait GameAction

  object GameAction {

    case object ShotSelector extends GameAction

    case object ManualShipSelector extends GameAction

    case object ManualWaterSelector extends GameAction

    case object FillWaterSelector extends GameAction

//    object BoardMarkSelector {
//      def unapply(gameAction: GameAction): Boolean = gameAction != ShotSelector
//    }

  }

  val MarksSelectorOrder: List[GameAction] =
    List(
      GameAction.ShotSelector,
      GameAction.ManualShipSelector,
      GameAction.ManualWaterSelector,
      GameAction.FillWaterSelector
    )

  val MainBoardDefaultMargin: Int = 10
  val SmallBoardDefaultMargin: Int = 10

}
