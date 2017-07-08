package org.mandrake.simulation

import scala.annotation.tailrec

final case class Simulation(states: Vector[State]) {
  def apply(events: Vector[Event]): (Simulation, Vector[Event]) = {
    type PendingStates = Vector[(State, StateInputAggregator)]

    @tailrec
    def step(pendingEvents: PendingStates, acc: StateOutput): StateOutput = pendingEvents match {
      case Vector() => acc
      case _ =>
        val (ready, notReady) = pendingEvents.map { case (state, agg) => state -> agg.aggregate(acc.events) }.partition(_._2.isRight)
        step(notReady.map { case (state, agg) => state -> (agg match {
          case Left(l) => l
        })
        },
          ready
            .map { case (state, agg) => state(agg match { case Right(r) => r }) }
            .fold(StateOutput(acc.states, Vector()))((o1, o2) => StateOutput(o1.states ++ o2.states, o1.events ++ o2.events)))
    }

    val lastOutput = step(states.map(state => state -> state.aggregator), StateOutput(Vector(), events))
    Simulation(lastOutput.states) -> lastOutput.events
  }
}
