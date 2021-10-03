package pt.rmartins.battleships.frontend.views.game

import io.udash.HasModelPropertyCreator
import pt.rmartins.battleships.shared.model.game.Coordinate

case class ScreenModel(
    canvasSize: Coordinate,
    selectedTab: String
)

object ScreenModel extends HasModelPropertyCreator[ScreenModel]
