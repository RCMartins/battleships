package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import io.udash.auth.AuthRequires
import io.udash.i18n.translatedDynamic
import org.scalajs.dom.html.Div
import org.scalajs.dom.window
import pt.rmartins.battleships.frontend.ApplicationContext.application
import pt.rmartins.battleships.frontend.routing.{RoutingInGameState, RoutingLoginPageState}
import pt.rmartins.battleships.frontend.services.rpc.NotificationsCenter
import pt.rmartins.battleships.frontend.services.{TranslationsService, UserContextService}
import pt.rmartins.battleships.frontend.views.game.BoardView.GameAction
import pt.rmartins.battleships.frontend.views.game.GamePresenter._
import pt.rmartins.battleships.frontend.views.game.Utils.combine
import pt.rmartins.battleships.frontend.views.model.JoinedPreGame.PlayingAgainstPlayer
import pt.rmartins.battleships.frontend.views.model.ModeType._
import pt.rmartins.battleships.frontend.views.model._
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.chat.ChatMessage
import pt.rmartins.battleships.shared.model.game.GameMode._
import pt.rmartins.battleships.shared.model.game._
import pt.rmartins.battleships.shared.model.utils.BoardUtils
import pt.rmartins.battleships.shared.model.utils.BoardUtils.{BoardMarks, canPlaceInBoard}
import pt.rmartins.battleships.shared.rpc.server.game.GameRPC
import scalatags.JsDom.all.span

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

class GamePresenter(
    preGameModel: ModelProperty[PreGameModel],
    gameModel: ModelProperty[GameModel],
    gameStateModel: ModelProperty[GameStateModel],
    chatModel: ModelProperty[ChatModel],
    screenModel: ModelProperty[ScreenModel],
    translationsModel: ModelProperty[TranslationsModel],
    gameRpc: GameRPC,
    userService: UserContextService,
    translationsService: TranslationsService,
    notificationsCenter: NotificationsCenter
)(implicit
    ec: ExecutionContext
) extends Presenter[RoutingInGameState.type]
    with AuthRequires {

  import translationsService._

  val previewBoardCache: Cache[String, Option[(Board, Int)]] =
    new Cache[String, Option[(Board, Int)]]

  val preGameRulesProperty: Property[Rules] =
    preGameModel.subProp(_.rules)

  val gameStateProperty: Property[Option[GameState]] =
    gameStateModel.bitransform(_.gameState)(gameStateOpt =>
      gameStateModel.get.copy(gameState = gameStateOpt)
    )

  val gamePuzzleStateProperty: Property[Option[GamePuzzleState]] =
    gameStateModel.bitransform(_.gamePuzzleState)(gamePuzzleStateOpt =>
      gameStateModel.get.copy(gamePuzzleState = gamePuzzleStateOpt)
    )

  val enemyProperty: ReadableProperty[Option[SimplePlayer]] =
    gameStateModel.transform(_.gameState.map(_.enemy))

  val enemyUsernameProperty: ReadableProperty[Option[Username]] =
    combine(
      preGameModel.subProp(_.inviter),
      preGameModel.subProp(_.inJoinedPreGame).transform(_.flatMap(_.enemyUsernameOpt)),
      gameStateProperty.transform(_.map(_.enemy.username))
    ).transform { case (inviter, preGameEnemy, inGameEnemy) =>
      inviter.map(_._1).orElse(preGameEnemy.orElse(inGameEnemy))
    }

  val gameModeProperty: Property[Option[GameMode]] =
    gameStateProperty.bitransform[Option[GameMode]](_.map(_.gameMode)) {
      case None =>
        gameStateProperty.get
      case Some(gameMode) =>
        gameStateProperty.get.map(_.copy(gameMode = gameMode))
    }

  val modeTypeProperty: ReadableProperty[Option[ModeType]] =
    gameModeProperty.transform(_.map {
      case _: PlacingShipsMode => PlacingGameModeType
      case _: PlayingMode      => PlayingModeType
      case _: GameOverMode     => GameOverModeType
    })

  val modeTypeOrPuzzleProperty: ReadableProperty[(Option[ModeType], Boolean)] =
    combine(
      modeTypeProperty,
      gamePuzzleStateProperty.transform(_.nonEmpty)
    )

  val myBoardSizeProperty: ReadableProperty[Option[Coordinate]] =
    gameStateProperty.transform(_.map(_.me.myBoard.boardSize))

  val mainBoardSizeProperty: ReadableProperty[Option[Coordinate]] =
    combine(
      gameStateProperty.transform(_.map(_.enemy.boardSize)),
      gamePuzzleStateProperty.transform(_.map(_.playerPuzzle.boardSize))
    ).transform { case (boardSizeOpt1, boardSizeOpt2) =>
      boardSizeOpt1.orElse(boardSizeOpt2)
    }

  val placingShipsModeProperty: Property[Option[PlacingShipsMode]] =
    gameModeProperty.bitransform[Option[PlacingShipsMode]] {
      case Some(placingShipsMode: PlacingShipsMode) => Some(placingShipsMode)
      case _                                        => None
    }(identity)

  val playingModeProperty: Property[Option[PlayingMode]] =
    gameModeProperty.bitransform[Option[PlayingMode]] {
      case Some(playingMode: PlayingMode) => Some(playingMode)
      case _                              => None
    }(identity)

  val rulesProperty: ReadableProperty[Option[Rules]] =
    gameStateProperty.transform(_.map(_.rules))

  val fleetProperty: ReadableProperty[Option[Fleet]] =
    rulesProperty.transform(_.map(_.gameFleet))

  val gameFleetMaxSize: ReadableProperty[Coordinate] =
    combine(
      fleetProperty,
      gamePuzzleStateProperty.transform(_.map(_.playerPuzzle.gameFleet.shipsList))
    ).transform { case (gameFleetOpt, shipsListOpt) =>
      gameFleetOpt
        .map(_.maxSize)
        .orElse(shipsListOpt.map(Fleet.fromShipsList(_).maxSize))
        .getOrElse(Coordinate.origin)
    }

//  val inPlacingShipsMode: ReadableProperty[Boolean] =
//    gameModeProperty.transform(_.exists(_.isPlacingShips))
//
//  val inPlayingMode: ReadableProperty[Boolean] =
//    gameModeProperty.transform(_.exists(_.isPlaying))
//
//  val inGameOverMode: ReadableProperty[Boolean] =
//    gameModeProperty.transform(_.exists(_.isEndGame))

  val isMyTurnProperty: ReadableProperty[Boolean] =
    playingModeProperty.transform(_.exists(_.isMyTurn))

  val meProperty: ReadableProperty[Option[Player]] =
    gameStateProperty.transform(_.map(_.me))

  val turnPlayHistory: ReadableProperty[Option[List[TurnPlay]]] =
    combine(
      meProperty.transform(_.map(_.turnPlayHistory)),
      gamePuzzleStateProperty.transform(_.map(_.playerPuzzle.turnPlayHistory))
    ).transform { case (turnPlayHistoryOpt1, turnPlayHistoryOpt2) =>
      turnPlayHistoryOpt1.orElse(turnPlayHistoryOpt2)
    }

  val selectedTabProperty: Property[String] =
    screenModel.subProp(_.selectedTab)

  val mousePositionProperty: Property[Option[Coordinate]] =
    gameModel.subProp(_.mousePosition)

  val lineDashOffset: Property[Int] =
    gameModel.subProp(_.lineDashOffset)

  private val msgCallback = notificationsCenter.onNewMsg { case msg =>
    chatModel.subSeq(_.msgs).append(msg)
  }

  private val onSendInviteRequestCallback = notificationsCenter.onSendInviteRequest {
    case (inviterUsername, playerInviteType) =>
      preGameModel
        .subProp(_.inviter)
        .set(Some((inviterUsername, playerInviteType)))
  }

  private val onSendInviteAnswerCallback = notificationsCenter.onSendInviteAnswer {
    case (invitedUsername, inviteAnswer) =>
      preGameModel.subProp(_.invitedUsername).get match {
        case Some(currentInvitesUsername) if currentInvitesUsername == invitedUsername =>
          preGameModel.subProp(_.invitedUsername).set(None)
          if (inviteAnswer)
            modeTypeOrPuzzleProperty.get match {
              case (None, false) | (Some(ModeType.GameOverModeType), _) =>
                gameRpc.startPreGameWithPlayer(invitedUsername, preGameRulesProperty.get)
              case _ =>
            }
        case _ =>
      }
  }

  private val onUpdatePreGameStateCallback = notificationsCenter.onUpdatePreGameState {
    case PreGameState(gameId, enemyUsername, confirmRules, enemyConfirmedRules, updateRules) =>
      preGameModel
        .subProp(_.inJoinedPreGame)
        .set(
          Some(
            PlayingAgainstPlayer(
              gameId,
              confirmed = confirmRules,
              enemyConfirmed = enemyConfirmedRules,
              enemyUsername
            )
          )
        )
      preGameModel.subProp(_.rules).set(updateRules)
      val beforeGameState: Option[GameState] = gameStateProperty.get
      gameStateProperty.set(None)
      updateGameState(beforeGameState.map(_.gameMode), None)
  }

  private val onPreGameRulesPatchCallback = notificationsCenter.onPreGameRulesPatch {
    case PreGameRulesPatch(Some(boardSizePatch), _, _, _, _) =>
      preGameRulesProperty.set(preGameRulesProperty.get.copy(boardSize = boardSizePatch))
    case PreGameRulesPatch(_, Some((shipId, counter)), _, _, _) =>
      preGameRulesProperty.set(
        preGameRulesProperty.get
          .modify(_.gameFleet)
          .using(fleet =>
            Fleet(fleet.shipCounterMap.updated(shipId, (counter, Rotation.Rotation0)).toList)
          )
      )
    case PreGameRulesPatch(_, _, Some(defaultTurnAttacksPatch), _, _) =>
      preGameRulesProperty.set(
        preGameRulesProperty.get.copy(defaultTurnAttacks = defaultTurnAttacksPatch)
      )
    case PreGameRulesPatch(_, _, _, Some(turnBonusesPatch), _) =>
      preGameRulesProperty.set(preGameRulesProperty.get.copy(gameBonuses = turnBonusesPatch))
    case PreGameRulesPatch(_, _, _, _, Some(timeLimitPatch)) =>
      preGameRulesProperty.set(preGameRulesProperty.get.copy(timeLimit = timeLimitPatch))
    case _ =>
  }

  private val onPreGameConfirmStateCallback =
    notificationsCenter.onPreGameConfirmState { case (confirmState, enemyConfirmState) =>
      preGameModel.subProp(_.inJoinedPreGame).get match {
        case Some(playingAgainstPlayer @ PlayingAgainstPlayer(_, _, _, _)) =>
          preGameModel
            .subProp(_.inJoinedPreGame)
            .set(
              Some(
                playingAgainstPlayer.copy(
                  confirmed = confirmState,
                  enemyConfirmed = enemyConfirmState
                )
              )
            )
        case _ =>
      }
    }

  private val onPlayerRequestCallback =
    notificationsCenter.onPlayerRequest { case playerRequestType: PlayerRequestType =>
      playerRequestType match {
        case PlayerRequestType.EditRules =>
          screenModel.subProp(_.receiveEditRequest).set(Some(()))
      }
    }

  private val onPlayerRequestAnswerCallback =
    notificationsCenter.onPlayerRequestAnswer { case (playerRequestType, _) =>
      playerRequestType match {
        case PlayerRequestType.EditRules =>
      }
    }

  private val onGameStateCallback = notificationsCenter.onGameState { case updatedGameState =>
    val currentGameState = gameStateProperty.get
    val mergedGameState: GameState = mergeGameState(currentGameState, updatedGameState)
    updateGameState(currentGameState.map(_.gameMode), Some(mergedGameState))
    gameStateProperty.set(Some(mergedGameState))
  }

  private val onGameModeCallback = notificationsCenter.onGameMode { case updatedGameMode =>
    val currentGameState = gameStateProperty.get
    val updatedGameState: Option[GameState] =
      currentGameState.map(_.copy(gameMode = updatedGameMode))
    val finalGameState: Option[GameState] =
      updatedGameState.map { updatedGameState =>
        mergeGameState(currentGameState, updatedGameState)
      }
    updateGameState(currentGameState.map(_.gameMode), finalGameState)
    gameStateProperty.set(finalGameState)
  }

  private val onQuitGameCallback = notificationsCenter.onQuitGame { case _ =>
    if (preGameModel.get.inJoinedPreGame.nonEmpty)
      clearPreGame()
    else {
      val updatedGameState = None
      updateGameState(gameStateProperty.get.map(_.gameMode), updatedGameState)
      gameModel.set(GameModel.Default)
      gameStateProperty.set(updatedGameState)
      screenModel.set(ScreenModel.resetScreenModel(screenModel.get))
    }
  }

  private val onUserErrorMessageCallback = notificationsCenter.onUserErrorMessage {
    case UserError.InviteItself =>
      screenModel.subProp(_.errorModalType).set(Some(ErrorModalType.InviteItselfError))
    case UserError.UsernameNotFound(username) =>
      screenModel.subProp(_.errorModalType).set(Some(ErrorModalType.UsernameNotFound(username)))
  }

  val chatMessagesProperty: ReadableSeqProperty[ChatMessage] =
    chatModel.subSeq(_.msgs)

  val chatMessagesSizeProperty: ReadableProperty[Int] =
    chatMessagesProperty.transform(_.size)

  val chatMessagesShowNotification: ReadableProperty[Option[Int]] =
    combine(
      chatMessagesSizeProperty,
      screenModel.subProp(_.lastSeenMessagesChat),
      selectedTabProperty
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.chatTab)
    }

  val myMovesHistoryProperty: ReadableSeqProperty[TurnPlay] =
    combine(
      gameStateProperty,
      gamePuzzleStateProperty,
      screenModel.subProp(_.showAllMoves),
      screenModel.subProp(_.showMissesMoves),
      screenModel.subProp(_.showDisabledMoves),
      screenModel.subProp(_.disabledMovesSet)
    ).transformToSeq {
      case (
            gameStateOpt,
            gamePuzzleState,
            showAllMoves,
            showMissesMoves,
            showDisabledMoves,
            disabledMovesSet
          ) =>
        gameStateOpt
          .map(_.me.turnPlayHistory)
          .orElse(gamePuzzleState.map(_.playerPuzzle.turnPlayHistory))
          .map(_.filter { turnPlay =>
            showAllMoves ||
            (showMissesMoves || turnPlay.hitHints.exists(_.isShip)) &&
            (showDisabledMoves || !disabledMovesSet(turnPlay.turn))
          })
          .getOrElse(Seq.empty)
    }

  val myMovesHistorySizeProperty: ReadableProperty[Int] =
    myMovesHistoryProperty.transform(_.size)

  val myMovesHistoryShowNotification: ReadableProperty[Option[Int]] =
    combine(
      myMovesHistorySizeProperty,
      screenModel.subProp(_.lastSeenMessagesMyMoves),
      selectedTabProperty
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.myMovesTab)
    }

  val enemyMovesHistoryProperty: ReadableSeqProperty[TurnPlay] =
    gameStateProperty.transformToSeq(_.map(_.enemy.turnPlayHistory).getOrElse(Seq.empty))

  val enemyMovesHistorySizeProperty: ReadableProperty[Int] =
    enemyMovesHistoryProperty.transform(_.size)

  val enemyMovesHistoryShowNotification: ReadableProperty[Option[Int]] =
    combine(
      enemyMovesHistorySizeProperty,
      screenModel.subProp(_.lastSeenMessagesEnemyMoves),
      selectedTabProperty
    ).transform { case (totalSize, lastSeenMessageCount, selectedTab) =>
      Some(totalSize - lastSeenMessageCount)
        .filter(_ > 0 && selectedTab != ScreenModel.enemyMovesTab)
    }

  def resetLastSeenMessages(): Unit =
    selectedTabProperty.get match {
      case ScreenModel.chatTab =>
        screenModel.subProp(_.lastSeenMessagesChat).set(chatMessagesSizeProperty.get)
      case ScreenModel.myMovesTab =>
        screenModel.subProp(_.lastSeenMessagesMyMoves).set(myMovesHistorySizeProperty.get)
      case ScreenModel.enemyMovesTab =>
        screenModel.subProp(_.lastSeenMessagesEnemyMoves).set(enemyMovesHistorySizeProperty.get)
    }

  val shipCounter: Map[ShipId, Property[Int]] =
    Ship.allShipsList.map(_.shipId -> Property(0)).toMap

  preGameModel
    .subProp(_.rules)
    .transform(_.gameFleet.shipCounterMap)
    .listen(
      { shipCounterMap =>
        Ship.allShipsList.foreach { ship =>
          val shipId = ship.shipId
          shipCounter(shipId).set(
            shipCounterMap.get(shipId).map(_._1).getOrElse(0)
          )
        }
      },
      initUpdate = true
    )

  private var lineDashOffsetIntervalHandle: Option[Int] = None
  private val lineDashOffsetIntervalMillis: Int = 200

  def setLineDashOffsetInterval(activate: Boolean): Unit = {
    lineDashOffsetIntervalHandle.foreach(window.clearTimeout)
    if (activate)
      lineDashOffsetIntervalHandle = Some(
        window.setInterval(
          () => {
            lineDashOffset.set(lineDashOffset.get + 1)
          },
          timeout = lineDashOffsetIntervalMillis
        )
      )
  }

  private var timeRemainingIntervalHandle: Option[Int] = None
  private val timeRemainingIntervalMillis: Int = 100

  private def clearPreGame(): Unit = {
    preGameModel.subProp(_.inJoinedPreGame).set(None)
  }

  private def mergeGameState(
      currentGameStateOpt: Option[GameState],
      updatedGameState: GameState
  ): GameState =
    (currentGameStateOpt, updatedGameState) match {
      case (
            Some(GameState(_, _, me, _, _: PlacingShipsMode)),
            GameState(_, _, _, _, _: PlacingShipsMode)
          ) =>
        updatedGameState.modify(_.me.myBoard).setTo(me.myBoard)
      case _ =>
        updatedGameState
    }

  private def updateGameState(
      fromGameState: Option[GameMode],
      toGameState: Option[GameState]
  ): Unit = {
    (fromGameState, toGameState) match {
      case (None, Some(GameState(_, rules, _, _, PlacingShipsMode(iPlacedShips, _)))) =>
        clearPreGame()
        val shipsToPlace: List[Ship] =
          if (iPlacedShips)
            Nil
          else
            rules.gameFleet.shipsList.sortBy(_.shipBiggestToSmallestOrder)
        gameModel
          .subProp(_.shipsLeftToPlace)
          .set(shipsToPlace)
        shipsToPlace.headOption.foreach { headShip =>
          gameModel.subProp(_.selectedShip).set(Some(headShip))
        }
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
        setLineDashOffsetInterval(activate = false)

        selectedTabProperty.set(ScreenModel.chatTab)
        screenModel.subProp(_.lastSeenMessagesChat).set(chatMessagesSizeProperty.get)
      case (
            None | Some(_: PlacingShipsMode),
            Some(GameState(_, _, me, enemy, PlayingMode(_, _, turnAttackTypes, _, _)))
          ) =>
        selectedTabProperty.set(ScreenModel.myMovesTab)
        screenModel.subProp(_.lastSeenMessagesMyMoves).set(me.turnPlayHistory.size)
        screenModel.subProp(_.lastSeenMessagesEnemyMoves).set(enemy.turnPlayHistory.size)
        gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))
        gameModel.subProp(_.selectedShip).set(None)

        setLineDashOffsetInterval(activate = true)
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = Some(
          window.setInterval(
            () => {
              def reduceTime(timeRemaining: TimeRemaining): TimeRemaining = {
                val timeInterval = timeRemainingIntervalMillis
                timeRemaining match {
                  case TimeRemaining(totalTimeRemainingMillis, None) =>
                    TimeRemaining(Math.max(0, totalTimeRemainingMillis - timeInterval), None)
                  case TimeRemaining(totalTimeRemainingMillis, Some(turnTimeRemainingMillis)) =>
                    if (turnTimeRemainingMillis >= timeInterval)
                      TimeRemaining(
                        totalTimeRemainingMillis,
                        Some(turnTimeRemainingMillis - timeInterval)
                      )
                    else
                      TimeRemaining(
                        Math.max(
                          0,
                          totalTimeRemainingMillis + turnTimeRemainingMillis - timeInterval
                        ),
                        Some(0)
                      )
                }
              }

              val timeRemainingPropertyOpt: Property[Option[(TimeRemaining, TimeRemaining)]] =
                gameModel.subProp(_.timeRemaining)

              (playingModeProperty.get.map(_.isMyTurn), timeRemainingPropertyOpt.get) match {
                case (Some(isMyTurn), Some((myTimeRemaining, enemyTimeRemaining))) =>
                  timeRemainingPropertyOpt.set(
                    Some(
                      if (isMyTurn)
                        (reduceTime(myTimeRemaining), enemyTimeRemaining)
                      else
                        (myTimeRemaining, reduceTime(enemyTimeRemaining))
                    )
                  )
                case _ =>
              }

              screenModel.subProp(_.tick).set(screenModel.get.tick + 1)
            },
            timeout = timeRemainingIntervalMillis
          )
        )
      case (_, Some(GameState(_, _, _, _, _: GameOverMode))) =>
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
        setLineDashOffsetInterval(activate = true)
      case (_, None) =>
        timeRemainingIntervalHandle.foreach(window.clearTimeout)
        timeRemainingIntervalHandle = None
        setLineDashOffsetInterval(activate = false)

        chatModel.subProp(_.msgs).set(Seq.empty)
        selectedTabProperty.set(ScreenModel.chatTab)
        screenModel.subProp(_.lastSeenMessagesChat).set(0)
      case _ =>
    }

    toGameState.map(_.gameMode) match {
      case Some(PlayingMode(_, _, _, Some(myTimeRemaining), Some(enemyTimeRemaining))) =>
        gameModel.subProp(_.timeRemaining).set(Some((myTimeRemaining, enemyTimeRemaining)))
      case Some(GameOverMode(_, _, Some(myTimeRemaining), Some(enemyTimeRemaining), _)) =>
        gameModel.subProp(_.timeRemaining).set(Some((myTimeRemaining, enemyTimeRemaining)))
      case _ =>
        gameModel.subProp(_.timeRemaining).set(None)
    }

    (fromGameState, toGameState) match {
      case (Some(_: PlayingMode), Some(GameState(_, _, _, _, _: PlayingMode))) =>
        screenModel.subProp(_.newTurn).set((), force = true)
      case _ =>
    }

    (fromGameState, toGameState) match {
      case (None, Some(GameState(_, rules, _, _, _))) =>
        preGameModel.subProp(_.rules).set(rules)
        val shipCounterMap: Map[ShipId, (Int, Rotation)] = rules.gameFleet.shipCounterMap
        Ship.allShipsList.foreach { ship =>
          val shipId = ship.shipId
          shipCounter(shipId).set(
            shipCounterMap.get(shipId).map(_._1).getOrElse(0)
          )
        }
      case _ =>
    }
  }

  playingModeProperty
    .transform(
      _.map(inGameMode => (inGameMode.isMyTurn, inGameMode.turn, inGameMode.turnAttackTypes))
    )
    .listen {
      case Some((isMyTurn, Turn(_, _), turnAttackTypes)) =>
        gameModel.get.turnAttacksQueuedStatus match {
          case AttacksQueuedStatus.NotSet =>
          case AttacksQueuedStatus.Queued if isMyTurn =>
            launchAttack()
          case AttacksQueuedStatus.Sent =>
            gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.NotSet)
            gameModel.subProp(_.turnAttacks).set(turnAttackTypes.map(Attack(_, None)))
        }
      case _ =>
    }

  override def handleState(state: RoutingInGameState.type): Unit = {
    chatModel.subProp(_.username).set(userService.getCurrentContext.username)

    gameRpc.getAllMessages.onComplete {
      case Failure(_) =>
      case Success(chatMessages) =>
        chatModel.subProp(_.msgs).set(chatMessages)
    }

    translationsModel
      .subProp(_.extraTurnText)
      .set(span(translatedDynamic(Translations.Game.extraTurn)(_.apply())).render)
    translationsModel
      .subProp(_.myBoardTitle)
      .set(span(translatedDynamic(Translations.Game.myBoardTitle)(_.apply())).render)
    translationsModel
      .subProp(_.enemyBoardTitle)
      .set(span(translatedDynamic(Translations.Game.enemyBoardTitle)(_.apply())).render)
    translationsModel
      .subProp(_.realEnemyBoardTitle)
      .set(span(translatedDynamic(Translations.Game.realEnemyBoardTitle)(_.apply())).render)
    translationsModel
      .subProp(_.previewBoardTitle)
      .set(span(translatedDynamic(Translations.PreGame.previewBoardTitle)(_.apply())).render)
    translationsModel
      .subProp(_.withoutRuleTimeLimit)
      .set(span(translatedDynamic(Translations.PreGame.withoutTimeLimit)(_.apply())).render)
    translationsModel
      .subProp(_.withRuleTimeLimit)
      .set(span(translatedDynamic(Translations.PreGame.withTimeLimit)(_.apply())).render)
    translationsModel
      .subProp(_.seconds)
      .set(span(translatedDynamic(Translations.PreGame.seconds)(_.apply())).render)
    translationsModel
      .subProp(_.totalTime)
      .set(span(translatedDynamic(Translations.PreGame.totalTime)(_.apply())).render)
    translationsModel
      .subProp(_.eachTurn)
      .set(span(translatedDynamic(Translations.PreGame.eachTurn)(_.apply())).render)
    translationsModel
      .subProp(_.amountOfShots)
      .set(span(translatedDynamic(Translations.PreGame.amountOfShots)(_.apply())).render)
    translationsModel
      .subProp(_.turnBonuses)
      .set(span(translatedDynamic(Translations.PreGame.turnBonuses)(_.apply())).render)
    translationsModel
      .subProp(_.bonusFirstBlood)
      .set(span(translatedDynamic(Translations.PreGame.bonusFirstBlood)(_.apply())).render)
    translationsModel
      .subProp(_.bonusDoubleKill)
      .set(span(translatedDynamic(Translations.PreGame.bonusDoubleKill)(_.apply())).render)
    translationsModel
      .subProp(_.bonusTripleKill)
      .set(span(translatedDynamic(Translations.PreGame.bonusTripleKill)(_.apply())).render)
    translationsModel
      .subProp(_.bonusUltraKill)
      .set(span(translatedDynamic(Translations.PreGame.bonusUltraKill)(_.apply())).render)
    translationsModel
      .subProp(_.shots)
      .set(span(translatedDynamic(Translations.PreGame.shots)(_.apply())).render)
    translationsModel
      .subProp(_.placeMarksCorrectly1)
      .set(span(translatedDynamic(Translations.Puzzles.placeMarksCorrectly1)(_.apply())).render)
    translationsModel
      .subProp(_.placeMarksCorrectly2)
      .set(span(translatedDynamic(Translations.Puzzles.placeMarksCorrectly2)(_.apply())).render)
    translationsModel
      .subProp(_.sendPuzzleAnswer)
      .set(span(translatedDynamic(Translations.Game.sendPuzzleAnswerButton)(_.apply())).render)
    translationsModel
      .subProp(_.solvedPuzzles)
      .set(span(translatedDynamic(Translations.Puzzles.solvedPuzzles)(_.apply())).render)
    translationsModel
      .subProp(_.puzzleCorrect)
      .set(span(translatedDynamic(Translations.Puzzles.puzzleCorrect)(_.apply())).render)
    translationsModel
      .subProp(_.puzzleIncorrect)
      .set(span(translatedDynamic(Translations.Puzzles.puzzleIncorrect)(_.apply())).render)

//     TODO force puzzle state:
//    startNewPuzzle()

    initializePreGameModel()
  }

  private def initializePreGameModel(): Unit = {
    val allShipCounters = shipCounter.toList
    val combinedShipCounters = allShipCounters.map(_._2).combineToSeqProperty

    val gameIdOpt: ReadableProperty[Option[GameId]] =
      preGameModel.subProp(_.inJoinedPreGame).transform {
        case Some(PlayingAgainstPlayer(gameId, _, _, _)) => Some(gameId)
        case _                                           => None
      }

    preGameModel
      .subProp(_.customNamedRulesMap)
      .set(
        SortedMap(
          Cookies.getAllNamedRulesCookieData().map(namedRules => (namedRules.name, namedRules)): _*
        )
      )

    combinedShipCounters.listen { _ =>
      val gameFleet: Fleet =
        Fleet.fromShipIds(
          allShipCounters.map { case (shipId, countProperty) =>
            shipId -> countProperty.get
          }
        )
      preGameRulesProperty.set(preGameRulesProperty.get.copy(gameFleet = gameFleet))
    }

    preGameRulesProperty.transform(_.boardSize).listen { updatedBoardSize =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(gameId, PreGameRulesPatch(boardSizePatch = Some(updatedBoardSize)))
      }
    }

    allShipCounters.foreach { case (shipId, counterProperty) =>
      counterProperty.listen { counter =>
        gameIdOpt.get.foreach { gameId =>
          gameRpc.sendRulesPatch(
            gameId,
            PreGameRulesPatch(gameFleetPatch = Some((shipId, counter)))
          )
        }
      }
    }

    preGameRulesProperty.transform(_.defaultTurnAttacks).listen { updatedDefaultTurnAttacks =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(
          gameId,
          PreGameRulesPatch(defaultTurnAttacksPatch = Some(updatedDefaultTurnAttacks))
        )
      }
    }

    preGameRulesProperty.transform(_.timeLimit).listen { updatedTimeLimit =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(
          gameId,
          PreGameRulesPatch(timeLimitPatch = Some(updatedTimeLimit))
        )
      }
    }

    preGameRulesProperty.transform(_.gameBonuses).listen { updatedTurnBonuses =>
      gameIdOpt.get.foreach { gameId =>
        gameRpc.sendRulesPatch(
          gameId,
          PreGameRulesPatch(gameBonusesPatch = Some(updatedTurnBonuses))
        )
      }
    }

    combine(
      combinedShipCounters,
      preGameRulesProperty.transform(_.boardSize),
      preGameModel.subProp(_.previewEnabled)
    ).listen(
      {
        case (_, boardSize, true) =>
          val sortedShipCounters: List[(ShipId, Property[Int])] =
            allShipCounters.sortBy { case (shipId, _) =>
              (-Ship.allShipsMap(shipId).piecesSize, -shipId.id)
            }

          val fleetKey: String =
            sortedShipCounters
              .map { case (shipId, countProperty) => s"$shipId-${countProperty.get}" }
              .mkString(boardSize.toString, ",", "")

          val fixedRandomGenerator = new Random(seed = fleetKey.hashCode)

          def tryPlacingAllPreviewShips(): Option[(Board, Int)] = {
            val possibleCoorLazyList: LazyList[(Coordinate, Rotation)] =
              LazyList.from(
                for {
                  x <- 0 until boardSize.x
                  y <- 0 until boardSize.y
                  rotation <- Rotation.all
                } yield (Coordinate(x, y), rotation)
              )

            val initialBoard = Board(boardSize, Nil)

            def placePreviewShip(board: Board, ship: Ship): Option[Board] = {
              val result: Option[(Coordinate, Ship)] =
                fixedRandomGenerator
                  .shuffle(possibleCoorLazyList)
                  .map { case (coordinate, rotation) => (coordinate, ship.rotateTo(rotation)) }
                  .find { case (coordinate, shipRotated) =>
                    canPlaceInBoard(board, shipRotated, coordinate)
                  }

              result match {
                case None =>
                  None
                case Some((coordinate, shipRotated)) =>
                  Some(board.addShip(ShipInBoard(shipRotated, coordinate)))
              }
            }

            @tailrec
            def placeAll(board: Board, ships: List[Ship]): Option[Board] = {
              ships match {
                case Nil =>
                  Some(board)
                case headShip :: next =>
                  placePreviewShip(board, headShip) match {
                    case None =>
                      None
                    case Some(updatedBoard) =>
                      placeAll(updatedBoard, next)
                  }
              }
            }

            @tailrec
            def placeAllLimited(
                initialBoard: Board,
                initialShips: List[Ship],
                tryCounter: Int,
                foundBoardOpt: Option[Board],
                successCounter: Int
            ): Option[(Board, Int)] =
              if (tryCounter < PreGameModel.MaxPreviewTries)
                placeAll(initialBoard, initialShips) match {
                  case None =>
                    placeAllLimited(
                      initialBoard,
                      initialShips,
                      tryCounter + 1,
                      foundBoardOpt,
                      successCounter
                    )
                  case Some(updatedBoard) =>
                    placeAllLimited(
                      initialBoard,
                      initialShips,
                      tryCounter + 1,
                      foundBoardOpt.orElse(Some(updatedBoard)),
                      successCounter + 1
                    )
                }
              else
                foundBoardOpt.map((_, successCounter))

            placeAllLimited(
              initialBoard,
              sortedShipCounters.flatMap { case (shipId, countProperty) =>
                List.fill(countProperty.get)(Ship.allShipsMap(shipId))
              },
              tryCounter = 0,
              foundBoardOpt = None,
              successCounter = 0
            )
          }

          val updatedPreviewBoardOpt: Option[(Board, Int)] =
            previewBoardCache.getOrUpdate(
              fleetKey,
              tryPlacingAllPreviewShips()
            )

          preGameModel.subProp(_.previewBoardOpt).set(updatedPreviewBoardOpt, force = true)
        case _ =>
      },
      initUpdate = true
    )
  }

  def onCanvasResize(div: Div): Unit = {
    val newSizeInt = div.clientHeight - MainBoardHeightMargin
    val newSize = Coordinate.square(newSizeInt)
    screenModel.subProp(_.mainBoardCanvasSize).set(newSize)

    val smallBoardNewSize: Coordinate =
      Coordinate.square((newSizeInt * SmallBoardSizeMultiplier).toInt)
    screenModel.subProp(_.smallBoardCanvasSize).set(smallBoardNewSize)
  }

  override def onClose(): Unit = {
    // remove callbacks from NotificationsCenter before exit
    msgCallback.cancel()

    onSendInviteRequestCallback.cancel()
    onSendInviteAnswerCallback.cancel()
    onUpdatePreGameStateCallback.cancel()
    onPreGameRulesPatchCallback.cancel()
    onPreGameConfirmStateCallback.cancel()
    onPlayerRequestCallback.cancel()
    onPlayerRequestAnswerCallback.cancel()
    onGameStateCallback.cancel()
    onGameModeCallback.cancel()

    onQuitGameCallback.cancel()
    onUserErrorMessageCallback.cancel()
  }

  def sendMsg(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, _)) =>
        val msgProperty = chatModel.subProp(_.msgInput)
        val msg = msgProperty.get.trim
        msgProperty.set("")
        if (msg.nonEmpty)
          gameRpc.sendMsg(gameId, msg)
      case _ =>
    }

  def showErrorModal(errorModalType: ErrorModalType): Unit =
    screenModel.subProp(_.errorModalType).set(Some(errorModalType), force = true)

  def getCurrentRulesValidated: Either[ErrorModalType, Rules] = {
    val preGame = preGameModel.get
    if (preGame.previewBoardOpt.forall(_._2 < PreGameModel.MinPreviewTries))
      Left(ErrorModalType.SmallBoardError)
    else if (preGame.rules.gameFleet.shipsAmount == 0)
      Left(ErrorModalType.EmptyFleetError)
    else {
      Right(preGame.rules)
    }
  }

  def startGameWithBots(): Unit =
    getCurrentRulesValidated match {
      case Left(errorModalType) => showErrorModal(errorModalType)
      case Right(rules)         => gameRpc.startGameWithBots(rules)
    }

  def invitePlayer(otherPlayerUsername: Username): Unit =
    if (otherPlayerUsername.username.nonEmpty) {
      preGameModel.subProp(_.invitedUsername).set(Some(otherPlayerUsername))
      gameRpc.invitePlayer(otherPlayerUsername, PlayerInviteType.Play)
    }

  def confirmRules(): Unit =
    preGameModel.subProp(_.inJoinedPreGame).get match {
      case Some(playingAgainstPlayer @ PlayingAgainstPlayer(gameId, false, _, _)) =>
        getCurrentRulesValidated match {
          case Left(errorModalType) =>
            showErrorModal(errorModalType)
          case Right(_) =>
            preGameModel
              .subProp(_.inJoinedPreGame)
              .set(Some(playingAgainstPlayer.copy(confirmed = true)))
            gameRpc.confirmRules(gameId)
        }
      case _ =>
    }

  def cancelRules(): Unit =
    preGameModel.subProp(_.inJoinedPreGame).get match {
      case Some(playingAgainstPlayer @ PlayingAgainstPlayer(gameId, true, _, _)) =>
        preGameModel
          .subProp(_.inJoinedPreGame)
          .set(Some(playingAgainstPlayer.copy(confirmed = false)))
        gameRpc.cancelRules(gameId)
      case _ =>
    }

  def rematchGame(): Unit =
    gameStateProperty.get match {
      case Some(GameState(_, _, _, enemy, _)) if enemy.isHuman =>
        preGameModel.subProp(_.invitedUsername).set(Some(enemy.username))
        gameRpc.invitePlayer(enemy.username, PlayerInviteType.Rematch)
      case Some(_) =>
        startGameWithBots()
      case _ =>
    }

  def logout(): Unit =
    gameStateProperty.get match {
      case None =>
        Cookies.clearLoginCookieData()
        for {
          _ <- gameRpc.logout()
          _ <- userService.logout()
        } yield application.goTo(RoutingLoginPageState)
      case _ =>
    }

  def quitCurrentGame(): Unit =
    preGameModel.get.inJoinedPreGame
      .flatMap(_.gameIdOpt)
      .orElse(gameStateProperty.get.map(_.gameId)) match {
      case Some(gameId) =>
        gameRpc.quitCurrentGame(gameId)
      case None =>
        if (gamePuzzleStateProperty.get.nonEmpty)
          gamePuzzleStateProperty.set(None)
    }

  def tryToPlaceShipInBoard(
      gameState: GameState,
      me: Player,
      ship: Ship,
      coordinate: Coordinate
  ): Boolean =
    if (
      gameModel.subProp(_.shipsLeftToPlace).get.exists(_.shipId == ship.shipId) &&
      canPlaceInBoard(me.myBoard, ship, coordinate)
    ) {
      val playerUpdated: Player =
        me
          .modify(_.myBoard)
          .using(_.addShip(ShipInBoard(ship, coordinate)))
      gameStateProperty.set(Some(gameState.copy(me = playerUpdated)))

      val updatedShipsLeftToPlace: List[Ship] =
        removeOneShip(ship.shipId, gameModel.subProp(_.shipsLeftToPlace).get)
      gameModel.subProp(_.shipsLeftToPlace).set(updatedShipsLeftToPlace)

      val nextShipOpt: Option[Ship] =
        if (updatedShipsLeftToPlace.exists(_.shipId == ship.shipId))
          Some(ship)
        else
          updatedShipsLeftToPlace.headOption
      gameModel.subProp(_.selectedShip).set(nextShipOpt)

      true
    } else
      false

  def keyDown(key: String, ctrlDown: Boolean): Unit = {
    gameStateProperty.get match {
      case Some(GameState(_, _, _, _, _: PlacingShipsMode)) =>
        if (key.equalsIgnoreCase("R"))
          rotateSelectedShip(1)
        else if (key.equalsIgnoreCase("\u001A") && ctrlDown)
          undoLastPlacedShip()
      case Some(gameState @ GameState(_, _, _, _, _: PlayingMode)) =>
        if (key.equalsIgnoreCase(" "))
          launchAttack()
        else if (key.equalsIgnoreCase("\u001A") && ctrlDown)
          undoLastPlacedMark(gameState)
      case _ =>
    }
  }

  def rotateSelectedShip(directionDelta: Int): Unit =
    (gameModel.get.selectedShip, modeTypeProperty.get) match {
      case (Some(ship), Some(PlacingGameModeType)) =>
        val updatedShip =
          if (directionDelta.abs != 1)
            ship.rotateBy(directionDelta)
          else {
            val possibleUpdatedShip = ship.rotateBy(directionDelta)
            if (
              possibleUpdatedShip.pieces.sortWith(Coordinate.defaultCompare(_, _) < 0) ==
                ship.pieces.sortWith(Coordinate.defaultCompare(_, _) < 0)
            )
              possibleUpdatedShip.rotateBy(directionDelta)
            else
              possibleUpdatedShip
          }
        gameModel.subProp(_.selectedShip).set(Some(updatedShip))
      case _ =>
    }

  private def removeOneShip(shipId: ShipId, list: List[Ship]): List[Ship] =
    list match {
      case Nil =>
        Nil
      case headShip :: next if headShip.shipId == shipId =>
        next
      case headShip :: next =>
        headShip :: removeOneShip(shipId, next)
    }

  def cancelShipsPlacement(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, PlacingShipsMode(true, _))) =>
        gameRpc.cancelShipsPlacement(gameId)
      case _ =>
    }

  def confirmShipPlacement(): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, me, _, _)) =>
        gameRpc.confirmShips(gameId, me.myBoard.ships)
      case _ =>
    }

  def undoLastPlacedShip(): Unit =
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, _, me, _, placingShipsMode: PlacingShipsMode)) =>
        val undoShip: Option[Ship] =
          me.myBoard.ships match {
            case Nil =>
              None
            case ShipInBoard(headShip, _) :: _ =>
              Some(headShip)
          }

        undoShip.foreach { headShip =>
          val shipsLeftToPlaceProperty = gameModel.subProp(_.shipsLeftToPlace)
          shipsLeftToPlaceProperty.set(headShip :: shipsLeftToPlaceProperty.get)
          gameStateProperty.set(
            Some(gameState.copy(me = me.modify(_.myBoard).using(_.removeLastShip)))
          )
        }

        gameModel.subProp(_.selectedShip).set(undoShip)

        if (placingShipsMode.iPlacedShips)
          cancelShipsPlacement()
      case _ =>
    }

  def resetPlacedShips(): Unit =
    gameStateProperty.get match {
      case Some(
            gameState @ GameState(
              _,
              Rules(_, fleet: Fleet, _, _, _),
              me,
              _,
              _: PlacingShipsMode
            )
          ) if me.myBoard.ships.nonEmpty =>
        val initialShips: List[Ship] =
          fleet.shipsList.sortBy(_.shipBiggestToSmallestOrder)
        gameModel.subProp(_.shipsLeftToPlace).set(initialShips)
        gameStateProperty.set(Some(gameState.copy(me = me.modify(_.myBoard).using(_.resetBoard))))
        gameModel.get.selectedShip match {
          case None =>
            gameModel.subProp(_.selectedShip).set(initialShips.headOption)
          case Some(_) =>
        }

        cancelShipsPlacement()
      case _ =>
    }

  def randomPlacement(): Unit =
    gameStateProperty.get match {
      case Some(gameState @ GameState(_, _, me, _, _: PlacingShipsMode)) =>
        gameModel.subProp(_.shipsLeftToPlace).get match {
          case headShip :: _ =>
            val possibleCoorList =
              for {
                x <- 0 until me.myBoard.boardSize.x
                y <- 0 until me.myBoard.boardSize.y
                rotation <- Rotation.all
              } yield (Coordinate(x, y), rotation)
            val result: Option[(Coordinate, Rotation)] =
              Random
                .shuffle(possibleCoorList)
                .find { case (coor, rotation) =>
                  tryToPlaceShipInBoard(gameState, me, headShip.rotateTo(rotation), coor)
                }

            if (result.nonEmpty)
              randomPlacement()
          case Nil =>
        }
      case _ =>
    }

  def cancelQueuedAttacks(): Unit =
    (
      gameModel.get.turnAttacksQueuedStatus,
      gameStateProperty.get
    ) match {
      case (AttacksQueuedStatus.Queued, Some(GameState(_, _, _, _, _: PlayingMode))) =>
        gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.NotSet)
      case _ =>
    }

  def launchAttack(): Unit =
    (
      gameModel.get.turnAttacksQueuedStatus,
      gameModel.get.turnAttacks,
      gameStateProperty.get
    ) match {
      case (
            AttacksQueuedStatus.NotSet | AttacksQueuedStatus.Queued,
            turnAttacks,
            Some(GameState(gameId, _, _, _, PlayingMode(isMyTurn, turn, _, _, _)))
          ) if turnAttacks.forall(_.isPlaced) =>
        if (isMyTurn) {
          gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.Sent)
          gameRpc.sendTurnAttacks(gameId, turn, turnAttacks)
        } else {
          gameModel.subProp(_.turnAttacksQueuedStatus).set(AttacksQueuedStatus.Queued)
        }
      case _ =>
    }

  def isValidCoordinateTarget(enemyBoardCoor: Coordinate): Boolean =
    gameStateProperty.get match {
      case Some(GameState(_, _, me, _, _: PlayingMode)) =>
        val Coordinate(x, y) = enemyBoardCoor
        val (turnNumberOpt, boardMark) = me.enemyBoardMarks(x)(y)
        turnNumberOpt.isEmpty && boardMark != BoardMark.Water
      case _ =>
        false
    }

  private def updateLastSeen(selectedTab: String): Unit =
    if (selectedTab == ScreenModel.chatTab)
      screenModel.subProp(_.lastSeenMessagesChat).set(chatMessagesSizeProperty.get)
    else if (selectedTab == ScreenModel.myMovesTab)
      screenModel.subProp(_.lastSeenMessagesMyMoves).set(myMovesHistorySizeProperty.get)
    else if (selectedTab == ScreenModel.enemyMovesTab)
      screenModel.subProp(_.lastSeenMessagesEnemyMoves).set(enemyMovesHistorySizeProperty.get)

  def setSelectedTab(selectedTab: String): Unit = {
    updateLastSeen(selectedTabProperty.get)
    selectedTabProperty.set(selectedTab)
    updateLastSeen(selectedTabProperty.get)
  }

  def getPreGameShipProperty(shipId: ShipId): Property[Int] =
    shipCounter(shipId)

  def addToEnemyTimeSeconds(secondsToAdd: Int): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, PlayingMode(_, _, _, _, Some(TimeRemaining(_, _))))) =>
        gameRpc.addToEnemyTimeSeconds(gameId, secondsToAdd)
      case _ =>
    }

  def answerInvitePlayerRequest(answer: Boolean): Unit = {
    preGameModel.subProp(_.inviter).get.foreach { case (inviterUsername, playerInviteType) =>
      preGameModel.subProp(_.inviter).set(None)
      gameRpc.sendPlayerInviteAnswer(inviterUsername, answer, playerInviteType)
    }
  }

  def requestEditRules(): Unit =
    gameStateProperty.get match {
      case Some(GameState(_, _, _, SimplePlayer(_, false, _, _), _: PlacingShipsMode)) =>
        quitCurrentGame()
      case Some(GameState(gameId, _, _, SimplePlayer(_, true, _, _), _: PlacingShipsMode)) =>
        gameRpc.sendPlayerRequest(gameId, PlayerRequestType.EditRules)
      case _ =>
    }

  def answerEditRulesRequest(answer: Boolean): Unit =
    gameStateProperty.get match {
      case Some(GameState(gameId, _, _, _, _: PlacingShipsMode)) =>
        gameRpc.sendPlayerRequestAnswer(gameId, PlayerRequestType.EditRules, answer)
      case _ =>
    }

  def startNewPuzzle(): Unit =
    gameRpc.getRandomPuzzle(chatModel.get.username).foreach {
      case None =>
      case Some((puzzleId, playerPuzzle)) =>
        gamePuzzleStateProperty.get
          .map(_.puzzleSolvedCounter)
          .map(Future.successful)
          .getOrElse(gameRpc.getPuzzlesSolvedCount(chatModel.get.username))
          .foreach { currentPuzzlesSolvedCounter =>
            gamePuzzleStateProperty.set(
              Some(
                GamePuzzleState(
                  currentPuzzlesSolvedCounter,
                  puzzleId,
                  playerPuzzle.initialBoardMarks,
                  playerPuzzle,
                  None
                )
              )
            )
            setSelectedTab(ScreenModel.myMovesTab)
            gameModel.set(
              gameModel.get.copy(
                selectedAction = GameAction.ManualShipSelector
              )
            )
          }
    }

  def getPuzzleSolution(): Unit =
    gamePuzzleStateProperty.get match {
      case Some(
            gamePuzzleState @ GamePuzzleState(
              puzzleSolvedCounter,
              puzzleId,
              boardMarks,
              PlayerPuzzle(boardSize, _, _, _, _),
              None
            )
          ) =>
        gameRpc.getPuzzleSolution(puzzleId).foreach {
          case None =>
          case Some(puzzleSolution) =>
            val correctState: Boolean =
              puzzleSolution match {
                case PuzzleSolution.CorrectShipBoardMarksSolution(solutionBoard) =>
                  solutionBoard.ships.flatMap(_.shipActualPieces).toSet ==
                    BoardUtils
                      .toList(boardSize, boardMarks)
                      .filter {
                        case (_, boardMark) if boardMark.isShip => true
                        case _                                  => false
                      }
                      .map(_._1)
                      .toSet
                case PuzzleSolution.CorrectAttacksSolution(solution) => ???
              }

            val updatedPuzzleSolvedCounter: Int =
              (if (correctState) 1 else 0) + puzzleSolvedCounter

            if (correctState)
              gameRpc.setPuzzleSolved(chatModel.get.username, puzzleId)

            gamePuzzleStateProperty.set(
              Some(
                gamePuzzleState.copy(
                  puzzleSolvedCounter = updatedPuzzleSolvedCounter,
                  puzzleSolutionOpt = Some((puzzleSolution, correctState))
                )
              )
            )
        }
      case _ =>
    }

  def undoLastPlacedMark(gameState: GameState): Unit =
    gameModel.subProp(_.marksPlacedHistory).get match {
      case Nil =>
      case lastMarksPlaced :: otherHistory =>
        val updatedEnemyBoardMarks: BoardMarks =
          lastMarksPlaced.foldLeft(gameState.me.enemyBoardMarks) {
            case (updatedBoard, (coor, before, _)) =>
              BoardUtils.updateBoardMarksUsing(
                updatedBoard,
                coor,
                {
                  case (maybeTurn, mark) if mark.isPermanent =>
                    (maybeTurn, mark)
                  case (maybeTurn, _) =>
                    (maybeTurn, before)
                }
              )
          }

        gameStateProperty.set(
          Some(gameState.modify(_.me.enemyBoardMarks).setTo(updatedEnemyBoardMarks))
        )

        gameModel.subProp(_.marksPlacedHistory).set(otherHistory)
        gameRpc.sendBoardMarks(
          gameState.gameId,
          lastMarksPlaced.map { case (coor, before, _) => (coor, before) }.toList
        )
    }

  def saveNewNamedRules(): Unit = {
    val name: String =
      screenModel.subProp(_.namedRuleName).get.trim.replaceAll("[\\s\\xA0]+", " ")
    if (name.nonEmpty) {
      val preGame = preGameModel.get

      val updatedNamedRules: NamedRules = NamedRules(name, preGame.rules)
      val updatedCustomNamedRules: SortedMap[String, NamedRules] =
        screenModel.subProp(_.namedRuleNameBefore).get match {
          case Some(namedRuleNameBefore) =>
            preGame.customNamedRulesMap
              .removed(namedRuleNameBefore)
              .updated(name, updatedNamedRules)
          case None =>
            preGame.customNamedRulesMap
              .updated(name, updatedNamedRules)
        }

      preGameModel.subProp(_.customNamedRulesMap).set(updatedCustomNamedRules)
      Cookies.saveAllNamedRulesCookieData(updatedCustomNamedRules.toList.map(_._2))
      preGameModel.subProp(_.selectedNamedRule).set(Some(updatedNamedRules))
    }
  }

}

object GamePresenter {

  val MainBoardHeightMargin: Int = 32
  val SmallBoardSizeMultiplier: Double = 0.35

}
