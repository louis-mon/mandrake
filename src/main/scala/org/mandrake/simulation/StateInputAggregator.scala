package org.mandrake.simulation

trait StateInputAggregator {
  def aggregate(events: Vector[Event]): Either[StateInputAggregator, StateInput]
}

object StateInputAggregator {
  type Result = Either[StateInputAggregator, StateInput]
}
