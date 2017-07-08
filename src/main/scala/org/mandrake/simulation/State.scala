package org.mandrake.simulation

trait State {
  def aggregator: StateInputAggregator

  def apply(event: StateInput): StateOutput
}