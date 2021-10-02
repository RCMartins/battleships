package pt.rmartins.battleships.frontend.routing

import io.udash._

sealed abstract class RoutingState(val parentState: Option[ContainerRoutingState]) extends State {
  override type HierarchyRoot = RoutingState
}

sealed abstract class ContainerRoutingState(parentState: Option[ContainerRoutingState])
    extends RoutingState(parentState)

sealed abstract class FinalRoutingState(parentState: Option[ContainerRoutingState])
    extends RoutingState(parentState)

case object RoutingRootState extends ContainerRoutingState(None)
case object RoutingLoginPageState extends FinalRoutingState(Some(RoutingRootState))
case object RoutingInGameState extends ContainerRoutingState(Some(RoutingRootState))
