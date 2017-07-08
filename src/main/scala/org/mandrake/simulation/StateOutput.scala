package org.mandrake.simulation

final case class StateOutput(states: Vector[State], events: Vector[Event])

object StateOutput {
  def apply(state: State): StateOutput = StateOutput(Vector(state), Vector())

  def apply(state: State, event: Event): StateOutput = new StateOutput(Vector(state), Vector(event))
}