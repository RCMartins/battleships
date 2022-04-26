package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import pt.rmartins.battleships.frontend.views.game.BoardView.GameAction
import pt.rmartins.battleships.frontend.views.game.BoardView.GameAction._
import pt.rmartins.battleships.frontend.views.game.ModeType._
import pt.rmartins.battleships.shared.model.game.GameMode._
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils
import pt.rmartins.battleships.shared.model.utils.BoardUtils.BoardMarks
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC

import scala.annotation.tailrec

class MousePresenter(
    gameModel: ModelProperty[GameModel],
    gamePresenter: GamePresenter,
    gameRpc: GameRPC
) {

  private object PlayingOrGameOver {

    def unapply(gameMode: GameMode): Boolean =
      gameMode match {
        case PlayingMode(_, _, _, _, _)  => true
        case GameOverMode(_, _, _, _, _) => true
        case _                           => false
      }

  }

  def mouseMove(boardView: BoardView, mouseX: Int, mouseY: Int): Unit = {
    boardView.myBoardCanvas.setAttribute("tabindex", "0")
    boardView.myBoardCanvas.focus()

    gameModel.subProp(_.mousePosition).set(Some(Coordinate(mouseX, mouseY)))

    (
      gameModel.get,
      gamePresenter.gameStateProperty.get,
      gamePresenter.gamePuzzleStateProperty.get
    ) match {
      case (
            GameModel(_, Some(_), Some(button @ (0 | 2)), _, _, _, selectedAction, _, _, _),
            Some(gameState @ GameState(gameId, _, me, _, PlayingOrGameOver())),
            None
          ) if selectedAction != ShotSelector =>
        (boardView.enemyBoardMouseCoordinate.get, button) match {
          case (Some(enemyBoardCoor), 2) =>
            removeBoardMark(me.enemyBoardMarks, enemyBoardCoor).foreach {
              case (boardMarksUpdated, updatedBoardMarksList) =>
                gamePresenter.gameStateProperty.set(
                  Some(gameState.modify(_.me.enemyBoardMarks).setTo(boardMarksUpdated))
                )
                gameRpc.sendBoardMarks(gameId, updatedBoardMarksList)
            }
          case (Some(enemyBoardCoor), 0) =>
            setBoardMark(
              selectedAction,
              enemyBoardCoor,
              me.myBoard.boardSize,
              me.enemyBoardMarks,
              fillWaterSelectorActive = false
            ).foreach { case (boardMarksUpdated, updatedBoardMarksList) =>
              gamePresenter.gameStateProperty.set(
                Some(gameState.modify(_.me.enemyBoardMarks).setTo(boardMarksUpdated))
              )
              gameRpc.sendBoardMarks(gameId, updatedBoardMarksList)
            }
          case _ =>
        }
      case (
            GameModel(_, Some(_), Some(button @ (0 | 2)), _, _, _, selectedAction, _, _, _),
            None,
            Some(
              gamePuzzleState @ GamePuzzleState(
                _,
                _,
                boardMarks,
                PlayerPuzzle(boardSize, _, _, _, _),
                _
              )
            )
          ) if selectedAction != ShotSelector =>
        (boardView.enemyBoardMouseCoordinate.get, button) match {
          case (Some(enemyBoardCoor), 2) =>
            removeBoardMark(boardMarks, enemyBoardCoor).foreach { case (boardMarksUpdated, _) =>
              gamePresenter.gamePuzzleStateProperty.set(
                Some(gamePuzzleState.copy(boardMarks = boardMarksUpdated))
              )
            }
          case (Some(enemyBoardCoor), 0) =>
            setBoardMark(
              selectedAction,
              enemyBoardCoor,
              boardSize,
              boardMarks,
              fillWaterSelectorActive = false
            ).foreach { case (boardMarksUpdated, _) =>
              gamePresenter.gamePuzzleStateProperty.set(
                Some(gamePuzzleState.copy(boardMarks = boardMarksUpdated))
              )
            }
          case _ =>
        }
      case _ =>
    }
  }

  def mouseLeave(): Unit = {
    gameModel.subProp(_.mousePosition).set(None)
    mouseUp()
  }

  def mouseUp(): Unit =
    gameModel.subProp(_.mouseDown).set(None)

  def mouseDown(boardView: BoardView, button: Int): Unit = {
    boardView.myBoardCanvas.setAttribute("tabindex", "0")
    boardView.myBoardCanvas.focus()

    gameModel.subProp(_.mouseDown).set(Some(button))

    (
      gameModel.get,
      gamePresenter.gameStateProperty.get,
      gamePresenter.gamePuzzleStateProperty.get
    ) match {
      case (
            GameModel(_, Some(_), Some(0), selectedShipOpt, _, _, _, _, _, _),
            Some(gameState @ GameState(_, _, _, _, _: PlacingShipsMode)),
            _
          ) =>
        leftMouseDownPlacingShips(boardView, selectedShipOpt, gameState)
      case (
            GameModel(_, Some(_), Some(2), _, _, _, _, _, _, _),
            Some(gameState @ GameState(_, _, _, _, placingShipsMode: PlacingShipsMode)),
            _
          ) =>
        rightMouseDownPlacingShips(boardView, gameState, placingShipsMode)
      case (
            GameModel(_, Some(_), Some(2), selectedShipOpt, turnAttacks, _, _, _, _, _),
            Some(gameState @ GameState(_, _, _, _, PlayingOrGameOver())),
            _
          ) =>
        rightMouseDownInGame(boardView, selectedShipOpt, turnAttacks, gameState)
      case (
            gameModelValue @ GameModel(_, Some(_), Some(0), _, _, _, _, _, _, _),
            Some(gameState @ GameState(_, _, _, _, PlayingOrGameOver())),
            _
          ) =>
        leftMouseDownInGame(boardView, gameModelValue, gameState)
      case (
            GameModel(_, Some(_), Some(2), _, _, _, _, _, _, _),
            _,
            Some(gamePuzzleState)
          ) =>
        rightMouseDownPuzzleMode(boardView, gamePuzzleState)
      case (
            gameModelValue @ GameModel(_, Some(_), Some(0), _, _, _, _, _, _, _),
            _,
            Some(gamePuzzleState)
          ) =>
        leftMouseDownPuzzleMode(boardView, gameModelValue, gamePuzzleState)
      case _ =>
    }
  }

  private def leftMouseDownPlacingShips(
      boardView: BoardView,
      selectedShipOpt: Option[Ship],
      gameState: GameState
  ): Unit = {
    boardView.shipToPlaceHover.get.map(_.ship) match {
      case Some(ship) =>
        gameModel.subProp(_.selectedShip).set(Some(ship))
      case None =>
        (selectedShipOpt, boardView.myBoardMouseCoordinate.get) match {
          case (Some(ship), Some(boardCoor)) =>
            val roundedBoardCoor =
              boardCoor.roundTo(gameState.me.myBoard.boardSize - ship.size + Coordinate(1, 1))

            gamePresenter.tryToPlaceShipInBoard(gameState, gameState.me, ship, roundedBoardCoor)
          case (Some(_), None) =>
            gameModel.subProp(_.selectedShip).set(None)
          case _ =>
        }
    }
  }

  private def rightMouseDownPlacingShips(
      boardView: BoardView,
      gameState: GameState,
      placingShipsMode: PlacingShipsMode
  ): Unit =
    boardView.myBoardMouseCoordinate.get match {
      case Some(boardCoor) =>
        gameState.me.myBoard.removeShipAt(boardCoor) match {
          case (_, None) =>
          case (updatedBoard, Some(ShipInBoard(removedShip, _))) =>
            gamePresenter.gameStateProperty.set(
              Some(gameState.modify(_.me.myBoard).setTo(updatedBoard))
            )
            gameModel
              .subProp(_.shipsLeftToPlace)
              .set(
                removedShip :: gameModel.subProp(_.shipsLeftToPlace).get
              )
            gameModel.subProp(_.selectedShip).set(Some(removedShip))

            if (placingShipsMode.iPlacedShips)
              gamePresenter.cancelShipsPlacement()
        }
      case _ =>
    }

  private def rightMouseDownInGame(
      boardView: BoardView,
      selectedShipOpt: Option[Ship],
      turnAttacks: List[Attack],
      gameState: GameState
  ): Unit = {
    val GameState(gameId, _, me, _, mode) = gameState

    boardView.enemyBoardMouseCoordinate.get match {
      case Some(enemyBoardCoor) =>
        removeBoardMark(me.enemyBoardMarks, enemyBoardCoor).foreach {
          case (boardMarksUpdated, updatedBoardMarksList) =>
            gamePresenter.gameStateProperty.set(
              Some(gameState.modify(_.me.enemyBoardMarks).setTo(boardMarksUpdated))
            )
            gameRpc.sendBoardMarks(gameId, updatedBoardMarksList)
        }

        if (mode.isPlaying && turnAttacks.exists(_.coordinateOpt.contains(enemyBoardCoor))) {
          val turnAttackUpdated: List[Attack] =
            turnAttacks.map {
              case Attack(attackType, Some(coordinate)) if coordinate == enemyBoardCoor =>
                Attack(attackType, None)
              case other =>
                other
            }

          gameModel.subProp(_.turnAttacks).set(turnAttackUpdated)

          if (
            gameModel.get.turnAttacksQueuedStatus == AttacksQueuedStatus.Queued &&
            turnAttackUpdated.exists(!_.isPlaced)
          )
            gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.NotSet)
        }
      case None =>
        (selectedShipOpt, boardView.summaryShipHover.get) match {
          case (Some(currentSelectedShip), Some(summaryShipHover))
              if currentSelectedShip.shipId == summaryShipHover.shipId =>
            gameModel.subProp(_.selectedShip).set(None)
          case (Some(_), None) =>
            gameModel.subProp(_.selectedShip).set(None)
          case _ =>
        }
    }
  }

  private def leftMouseDownSelectedShip(
      boardView: BoardView,
      selectedShipOpt: Option[Ship]
  ): Unit = {
    (
      boardView.enemyBoardMouseCoordinate.get,
      boardView.boardMarkHover.get,
      selectedShipOpt,
      boardView.summaryShipHover.get
    ) match {
      case (_, _, Some(currentSelectedShip), Some(summaryShipHover))
          if currentSelectedShip.shipId == summaryShipHover.shipId =>
        gameModel.subProp(_.selectedShip).set(None)
      case (None, None, Some(_), None) =>
        gameModel.subProp(_.selectedShip).set(None)
      case (_, _, _, Some(summaryShipHover)) =>
        gameModel.subProp(_.selectedShip).set(Some(summaryShipHover))
      case _ =>
    }
  }

  private def leftMouseDownInGame(
      boardView: BoardView,
      gameModelValue: GameModel,
      gameState: GameState
  ): Unit = {
    val GameModel(
      _,
      _,
      _,
      selectedShipOpt,
      turnAttacks,
      turnAttacksSent,
      selectedAction,
      _,
      _,
      _
    ) = gameModelValue
    val GameState(gameId, Rules(boardSize, _, _, _, _), me, _, _) = gameState

    (boardView.enemyBoardMouseCoordinate.get, boardView.boardMarkHover.get) match {
      case (Some(enemyBoardCoor), None)
          if gameState.gameMode.isPlaying &&
            turnAttacksSent == AttacksQueuedStatus.NotSet &&
            gamePresenter.isValidCoordinateTarget(enemyBoardCoor) &&
            selectedAction == ShotSelector =>
        def setFirstMissile(turnAttacks: List[Attack]): List[Attack] =
          turnAttacks match {
            case Nil =>
              Nil
            case (headAttack @ Attack(_, None)) :: next =>
              headAttack.copy(coordinateOpt = Some(enemyBoardCoor)) :: next
            case headAttack :: next =>
              headAttack :: setFirstMissile(next)
          }

        val turnAttackUpdated: List[Attack] =
          if (turnAttacks.exists(_.coordinateOpt.contains(enemyBoardCoor)))
            turnAttacks.map {
              case Attack(attackType, Some(coordinate)) if coordinate == enemyBoardCoor =>
                Attack(attackType, None)
              case other =>
                other
            }
          else
            setFirstMissile(turnAttacks)

        gameModel.subProp(_.turnAttacks).set(turnAttackUpdated)
      case (Some(enemyBoardCoor), None) =>
        setBoardMark(
          selectedAction,
          enemyBoardCoor,
          boardSize,
          me.enemyBoardMarks,
          fillWaterSelectorActive = true
        ).foreach { case (updatedBoardMarks, updatedBoardMarksList) =>
          gamePresenter.gameStateProperty.set(
            Some(gameState.copy(me = me.copy(enemyBoardMarks = updatedBoardMarks)))
          )
          gameRpc.sendBoardMarks(gameId, updatedBoardMarksList)
        }
      case (None, Some(newSelectedAction)) if selectedAction != newSelectedAction =>
        gameModel.subProp(_.selectedAction).set(newSelectedAction)
      case (None, None) if selectedAction != ShotSelector =>
        gameModel.subProp(_.selectedAction).set(ShotSelector)
      case _ =>
    }

    leftMouseDownSelectedShip(boardView, selectedShipOpt)
  }

  private def rightMouseDownPuzzleMode(
      boardView: BoardView,
      gamePuzzleState: GamePuzzleState
  ): Unit =
    boardView.enemyBoardMouseCoordinate.get match {
      case Some(puzzleBoardCoor) =>
        removeBoardMark(gamePuzzleState.boardMarks, puzzleBoardCoor).foreach {
          case (boardMarksUpdated, _) =>
            gamePresenter.gamePuzzleStateProperty.set(
              Some(gamePuzzleState.copy(boardMarks = boardMarksUpdated))
            )
        }
      case _ =>
    }

  private def leftMouseDownPuzzleMode(
      boardView: BoardView,
      gameModelValue: GameModel,
      gamePuzzleState: GamePuzzleState
  ): Unit = {
    val GameModel(_, _, _, selectedShipOpt, _, _, selectedAction, _, _, _) = gameModelValue
    val GamePuzzleState(_, _, puzzleBoardMarks, PlayerPuzzle(boardSize, _, _, _, _), _) =
      gamePuzzleState

    (boardView.enemyBoardMouseCoordinate.get, boardView.boardMarkHover.get) match {
      case (Some(enemyBoardCoor), None) if selectedAction != ShotSelector =>
        setBoardMark(
          selectedAction,
          enemyBoardCoor,
          boardSize,
          puzzleBoardMarks,
          fillWaterSelectorActive = true
        ).foreach { case (updatedBoardMarks, _) =>
          gamePresenter.gamePuzzleStateProperty.set(
            Some(gamePuzzleState.modify(_.boardMarks).setTo(updatedBoardMarks))
          )
        }
      case (None, Some(newSelectedAction)) if selectedAction != newSelectedAction =>
        gameModel.subProp(_.selectedAction).set(newSelectedAction)
      case (None, None) if selectedAction != ShotSelector =>
        gameModel.subProp(_.selectedAction).set(ShotSelector)
      case _ =>
    }

    leftMouseDownSelectedShip(boardView, selectedShipOpt)
  }

  def mouseWheel(boardView: BoardView, wheelRotation: Int): Unit = {
    boardView.myBoardCanvas.setAttribute("tabindex", "0")
    boardView.myBoardCanvas.focus()

    gamePresenter.modeTypeOrPuzzleProperty.get match {
      case (Some(PlacingGameModeType), _) =>
        if (wheelRotation != 0)
          gamePresenter.rotateSelectedShip(wheelRotation)
      case modeTypePair @ ((Some(PlayingModeType | GameOverModeType), _) | (_, true)) =>
        val currentIndex: Int =
          BoardView.MarksSelectorOrder.indexOf(gameModel.get.selectedAction)
        val nextIndex: Int =
          if (modeTypePair._1.contains(PlayingModeType))
            (currentIndex + wheelRotation + BoardView.MarksSelectorOrder.size) %
              BoardView.MarksSelectorOrder.size
          else
            (((currentIndex - 1) + wheelRotation + BoardView.MarksSelectorOrder.size - 1) %
              (BoardView.MarksSelectorOrder.size - 1)) + 1
        gameModel
          .subProp(_.selectedAction)
          .set(BoardView.MarksSelectorOrder(nextIndex))
      case _ =>
    }
  }

  private def removeBoardMark(
      boardMarks: BoardMarks,
      boardCoordinate: Coordinate
  ): Option[(BoardMarks, List[(Coordinate, BoardMark)])] = {
    val (_, currentBoardMark) = boardMarks(boardCoordinate.x)(boardCoordinate.y)

    if (!currentBoardMark.isPermanent && currentBoardMark != BoardMark.Empty) {
      gameModel
        .subProp(_.marksPlacedHistory)
        .set(
          Set[(Coordinate, BoardMark, BoardMark)](
            (boardCoordinate, currentBoardMark, BoardMark.Empty)
          ) ::
            gameModel.subProp(_.marksPlacedHistory).get
        )

      Some(
        (
          BoardUtils.updateBoardMarksUsing(
            boardMarks,
            boardCoordinate,
            { case (turnOpt, _) => (turnOpt, BoardMark.Empty) }
          ),
          List((boardCoordinate, BoardMark.Empty))
        )
      )
    } else
      None
  }

  private def setBoardMark(
      selectedInGameMark: GameAction,
      boardCoordinate: Coordinate,
      boardSize: Coordinate,
      boardMarks: BoardMarks,
      fillWaterSelectorActive: Boolean
  ): Option[(BoardMarks, List[(Coordinate, BoardMark)])] = {
    val updatedBoardMarksList: List[(Coordinate, BoardMark, BoardMark)] =
      (selectedInGameMark match {
        case ManualShipSelector =>
          List((boardCoordinate, BoardMark.ManualShip))
        case ManualWaterSelector =>
          List((boardCoordinate, BoardMark.ManualWater))
        case FillWaterSelector if fillWaterSelectorActive =>
          def isShip(coordinate: Coordinate): Boolean =
            boardMarks(coordinate.x)(coordinate.y)._2.isShip

          @tailrec
          def getAllNearShipCoor(
              coorToCheck: List[Coordinate],
              shipCoors: Set[Coordinate],
              seen: Set[Coordinate]
          ): Set[Coordinate] =
            coorToCheck match {
              case Nil => shipCoors
              case center :: nextCoors =>
                val newCoordinates: List[Coordinate] =
                  center.get8CoorAround.filterNot(seen).filter(_.isInsideBoard(boardSize))
                val newShipCoordinates: List[Coordinate] =
                  newCoordinates.filter(isShip)
                getAllNearShipCoor(
                  newShipCoordinates ++ nextCoors,
                  shipCoors ++ Set(center).filter(isShip) ++ newShipCoordinates,
                  (seen + center) ++ newCoordinates
                )
            }

          val shipMarkPositions: Set[Coordinate] =
            getAllNearShipCoor(List(boardCoordinate).filter(isShip), Set.empty, Set.empty)

          if (shipMarkPositions.isEmpty)
            Nil
          else
            shipMarkPositions
              .flatMap(_.get8CoorAround)
              .filter(_.isInsideBoard(boardSize))
              .filterNot(shipMarkPositions)
              .map(_ -> BoardMark.ManualWater)
              .toList
        case _ =>
          Nil
      }).flatMap { case (coor, updateBoardMark) =>
        val currentBoardMark = boardMarks(coor.x)(coor.y)._2
        if (!currentBoardMark.isPermanent && currentBoardMark != updateBoardMark)
          Some((coor, currentBoardMark, updateBoardMark))
        else
          None
      }

    if (updatedBoardMarksList.nonEmpty) {
      val updatedBoardMarks: BoardMarks =
        updatedBoardMarksList.foldLeft(boardMarks) {
          case (enemyBoardMarks, (boardCoor, _, newBoardMark)) =>
            BoardUtils.updateBoardMarksUsing(
              enemyBoardMarks,
              boardCoor,
              { case (turnNumberOpt, _) => (turnNumberOpt, newBoardMark) }
            )
        }

      gameModel
        .subProp(_.marksPlacedHistory)
        .set(updatedBoardMarksList.toSet :: gameModel.subProp(_.marksPlacedHistory).get)

      Some(
        (
          updatedBoardMarks,
          updatedBoardMarksList.map { case (coor, _, boardMark) => (coor, boardMark) }
        )
      )
    } else
      None
  }

}
