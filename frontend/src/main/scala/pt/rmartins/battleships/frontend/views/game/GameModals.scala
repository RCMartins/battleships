package pt.rmartins.battleships.frontend.views.game

import com.softwaremill.quicklens.ModifyPimp
import io.udash._
import io.udash.bindings.modifiers.Binding.NestedInterceptor
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.utils.BootstrapStyles.Color
import io.udash.bootstrap.utils.UdashIcons.FontAwesome
import io.udash.component.ComponentId
import io.udash.css.CssView
import io.udash.i18n._
import org.scalajs.dom.html.{Div, Input}
import pt.rmartins.battleships.frontend.services.TranslationsService
import pt.rmartins.battleships.frontend.views.game.Utils._
import pt.rmartins.battleships.frontend.views.model.ErrorModalType._
import pt.rmartins.battleships.frontend.views.model.NamedRules
import pt.rmartins.battleships.shared.i18n.Translations
import pt.rmartins.battleships.shared.model.game._
import scalatags.JsDom.all._

import scala.util.chaining.scalaUtilChainingOps

class GameModals(
    preGameModel: ModelProperty[PreGameModel],
    screenModel: ModelProperty[ScreenModel],
    presenter: GamePresenter,
    translationsService: TranslationsService
) extends CssView {

  import translationsService._

  val errorModalId: String = "error-modal"
  def generalGameErrorModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := errorModalId,
      div(
        `class` := "modal-dialog",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            nested(produceWithNested(screenModel.subProp(_.errorModalType)) {
              case (Some(SmallBoardError | EmptyFleetError), nested) =>
                h5(
                  nested(translatedDynamic(Translations.Game.cannotStartGameTitle)(_.apply()))
                ).render
              case _ =>
                h5(
                  nested(translatedDynamic(Translations.Game.generalErrorTitle)(_.apply()))
                ).render
            }),
            span(
              style := "cursor: pointer",
              FontAwesome.Solid.times,
              FontAwesome.Modifiers.Sizing.x2,
              attr("data-bs-dismiss") := "modal"
            )
          ),
          div(
            `class` := "modal-body",
            nested(produceWithNested(screenModel.subProp(_.errorModalType)) {
              case (Some(SmallBoardError), nested) =>
                span(
                  nested(translatedDynamic(Translations.Game.startGameBoardSizeError)(_.apply()))
                ).render
              case (Some(EmptyFleetError), nested) =>
                span(
                  nested(translatedDynamic(Translations.Game.startGameEmptyFleetError)(_.apply()))
                ).render
              case (Some(InviteItselfError), nested) =>
                span(
                  nested(translatedDynamic(Translations.Game.inviteItselfError)(_.apply()))
                ).render
              case (Some(UsernameNotFound(Username(username))), nested) =>
                span(
                  nested(translatedDynamic(Translations.Game.usernameNotFoundError)(_.apply())),
                  span(" ", b(username))
                ).render
              case (None, _) =>
                span.render
            })
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-primary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.closeButton)(_.apply()))
            )
          )
        )
      )
    ).render

  val acceptPlayerInviteModalId: String = "accept-player-invite-modal"
  def acceptPlayerInviteModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := acceptPlayerInviteModalId,
      attr("data-bs-backdrop") := "static",
      div(
        `class` := "modal-dialog",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            nested(produceWithNested(preGameModel.subProp(_.inviter)) {
              case (Some((_, PlayerInviteType.Play)), nested) =>
                h5(
                  nested(
                    translatedDynamic(Translations.Game.invitePlayerModalTitle)(_.apply())
                  )
                ).render
              case (Some((_, PlayerInviteType.Rematch)), nested) =>
                h5(
                  nested(
                    translatedDynamic(Translations.Game.invitePlayerRematchModalTitle)(_.apply())
                  )
                ).render
              case _ =>
                h5.render
            }),
            span(
              style := "cursor: pointer",
              FontAwesome.Solid.times,
              FontAwesome.Modifiers.Sizing.x2,
              attr("data-bs-dismiss") := "modal"
            )
          ),
          div(
            `class` := "modal-body",
            nested(produceWithNested(presenter.enemyUsernameProperty) {
              case (Some(enemyUsername), nested) =>
                span(
                  nested(
                    translatedDynamic(Translations.Game.invitePlayerModalBodyStart)(_.apply())
                  ),
                  " ",
                  b(enemyUsername.username),
                  " ",
                  nested(produceWithNested(preGameModel.subProp(_.inviter)) {
                    case (Some((_, PlayerInviteType.Play)), nested) =>
                      span(
                        nested(
                          translatedDynamic(Translations.Game.invitePlayerModalBody)(_.apply())
                        )
                      ).render
                    case (Some((_, PlayerInviteType.Rematch)), nested) =>
                      span(
                        nested(
                          translatedDynamic(Translations.Game.invitePlayerRematchModalBody)(
                            _.apply()
                          )
                        )
                      ).render
                    case _ =>
                      span.render
                  })
                ).render
              case _ =>
                span.render
            })
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-danger",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.modalDecline)(_.apply()))
            ).render,
            button(
              `class` := "btn btn-success",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.modalAccept)(_.apply()))
            ).render.tap {
              _.onclick = _ => {
                presenter.answerInvitePlayerRequest(true)
              }
            }
          )
        )
      )
    ).render

  val editRulesModalId: String = "edit-rules-modal"
  def acceptEditRulesModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := editRulesModalId,
      div(
        `class` := "modal-dialog",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            h5(nested(translatedDynamic(Translations.Game.editRulesModalTitle)(_.apply())))
          ),
          div(
            `class` := "modal-body",
            nested(produceWithNested(presenter.enemyUsernameProperty) {
              case (Some(enemyUsername), nested) =>
                span(
                  nested(translatedDynamic(Translations.Game.editRulesModalBodyStart)(_.apply())),
                  " ",
                  b(enemyUsername.username),
                  " ",
                  nested(translatedDynamic(Translations.Game.editRulesModalBody)(_.apply()))
                ).render
              case _ =>
                span.render
            })
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-danger",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.modalDecline)(_.apply()))
            ).render,
            button(
              `class` := "btn btn-success",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.modalAccept)(_.apply()))
            ).render.tap {
              _.onclick = _ => {
                presenter.answerEditRulesRequest(true)
              }
            }
          )
        )
      )
    ).render

  val fleetNameInput: Input =
    input(
      id := "fleet-name-modal-input",
      `class` := "form-control",
      `type` := "text",
      autofocus
    ).render

  fleetNameInput.onkeypress = event => {
    if (event.key == "Enter" || event.keyCode == 13)
      Globals.modalHide(fleetNameModalId)
    else if (
      fleetNameInput.value.length >= NamedRules.MaxNamedRulesLength &&
      fleetNameInput.selectionStart == fleetNameInput.selectionEnd
    )
      event.preventDefault()
  }

  fleetNameInput.onchange = _ => {
    fleetNameInput.value = fleetNameInput.value.take(NamedRules.MaxNamedRulesLength).trim
    screenModel.subProp(_.namedRuleName).set(fleetNameInput.value)
  }

  screenModel.subProp(_.namedRuleName).listen { newFleetName =>
    fleetNameInput.value = newFleetName
  }

  val fleetNameModalId: String = "fleet-name-modal"
  def fleetNameModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := fleetNameModalId,
      div(
        `class` := "modal-dialog modal-dialog-centered",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            h5(nested(translatedDynamic(Translations.PreGame.chooseFleetNameTitle)(_.apply()))),
            span(
              style := "cursor: pointer",
              FontAwesome.Solid.times,
              FontAwesome.Modifiers.Sizing.x2,
              attr("data-bs-dismiss") := "modal"
            ).render.tap {
              _.onclick = _ => {
                screenModel.subProp(_.namedRuleName).set("")
              }
            }
          ),
          div(
            `class` := "modal-body",
            div(
              `class` := "form-group",
              fleetNameInput
            )
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-secondary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.closeButton)(_.apply()))
            ).render.tap {
              _.onclick = _ => {
                screenModel.subProp(_.namedRuleName).set("")
              }
            },
            button(
              `class` := "btn btn-primary",
              `type` := "button",
              nested(translatedDynamic(Translations.PreGame.saveFleet)(_.apply()))
            ).render.tap {
              _.onclick = _ => {
                if (
                  fleetNameInput.value.nonEmpty &&
                  fleetNameInput.value.sizeIs <= NamedRules.MaxNamedRulesLength
                )
                  Globals.modalHide(fleetNameModalId)
              }
            }
          )
        )
      )
    ).render

  private val acceptGameBonusChangesButton = UdashButton(
    buttonStyle = Color.Success.toProperty,
    componentId = ComponentId("accept-game-bonus-changes-button"),
    disabled = combine(
      preGameModel.subProp(_.rules),
      preGameModel.subProp(_.editGameBonusType),
      preGameModel.subProp(_.editGameBonusRewards)
    ).transform { case (rules, editGameBonusType, editGameBonusRewards) =>
      rules.turnBonuses
        .find(_.bonusType == editGameBonusType)
        .map(_.bonusRewardList)
        .getOrElse(Nil) == editGameBonusRewards
    }
  )(nested =>
    Seq[Modifier](
      attr("data-bs-dismiss") := "modal",
      span(nested(translatedDynamic(Translations.PreGame.saveChangesButton)(_.apply())))
    )
  )

  acceptGameBonusChangesButton.listen { _ =>
    val preGame = preGameModel.get
    preGameModel
      .subProp(_.rules)
      .set(
        preGame.rules
          .modify(_.turnBonuses)
          .using { currentTurnBonuses =>
            (preGame.editGameBonusRewards match {
              case Nil  => Nil
              case list => List(TurnBonus(preGame.editGameBonusType, list))
            }) ++
              currentTurnBonuses.filterNot(_.bonusType == preGame.editGameBonusType)
          }
      )
  }

  val editGameBonusModalId: String = "game-bonus-modal"
  def editGameBonusModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := editGameBonusModalId,
      div(
        `class` := "modal-dialog modal-dialog-centered",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            h5(
              nested(translatedDynamic(Translations.PreGame.editGameBonusTitle)(_.apply())),
              nested(
                produceWithNested(preGameModel.subProp(_.editGameBonusType)) {
                  case (bonusType, nested) =>
                    span(" ", b(nested(TranslationUtils.bonusTypeToText(bonusType)))).render
                }
              )
            ),
            span(
              style := "cursor: pointer",
              FontAwesome.Solid.times,
              FontAwesome.Modifiers.Sizing.x2,
              attr("data-bs-dismiss") := "modal"
            )
          ),
          div(
            `class` := "modal-body",
            div(
              button(
                `class` := "btn btn-primary",
                `type` := "button",
                nested(translatedDynamic(Translations.PreGame.newBonusButton)(_.apply()))
              ).render.tap { button =>
                preGameModel.subProp(_.editGameBonusRewards).listen {
                  case Nil => button.disabled = false
                  case _   => button.disabled = true
                }

                button.onclick = _ => {
                  preGameModel
                    .subProp(_.editGameBonusRewards)
                    .set(List(BonusReward.ExtraTurn(List(AttackType.Simple))))
                }
              }
            ),
            div(
              nested(
                produce(preGameModel.subProp(_.editGameBonusDiv)) { div => div }
              )
            )
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-secondary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.closeButton)(_.apply()))
            ),
            acceptGameBonusChangesButton
          )
        )
      )
    ).render

  val quitGameModalId: String = "quit-game-modal"
  def quitGameModal(nested: NestedInterceptor): Div =
    div(
      `class` := "modal fade",
      id := quitGameModalId,
      div(
        `class` := "modal-dialog",
        div(
          `class` := "modal-content",
          div(
            `class` := "modal-header",
            h5(nested(translatedDynamic(Translations.Game.confirmQuitGameTitle)(_.apply()))),
            span(
              style := "cursor: pointer",
              FontAwesome.Solid.times,
              FontAwesome.Modifiers.Sizing.x2,
              attr("data-bs-dismiss") := "modal"
            )
          ),
          div(
            `class` := "modal-footer",
            button(
              `class` := "btn btn-secondary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.closeButton)(_.apply()))
            ),
            button(
              `class` := "btn btn-primary",
              `type` := "button",
              attr("data-bs-dismiss") := "modal",
              nested(translatedDynamic(Translations.Game.confirmButton)(_.apply()))
            ).render.tap {
              _.onclick = _ => {
                presenter.quitCurrentGame()
              }
            }
          )
        )
      )
    ).render

}
