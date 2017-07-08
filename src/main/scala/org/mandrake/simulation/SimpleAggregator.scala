package org.mandrake.simulation

import scala.reflect.ClassTag

case class SimpleAggregator[T <: Event : ClassTag]() extends StateInputAggregator {
  override def aggregate(events: Vector[Event]): StateInputAggregator.Result =
    events.iterator.collect {
      case event: T => event
    }.find(_ => true).fold[StateInputAggregator.Result](Left(this))(Right(_))
}

abstract class SimpleInputState[T <: Event : ClassTag] extends State {
  final override def aggregator: StateInputAggregator = SimpleAggregator[T]()

  final override def apply(event: StateInput): StateOutput = apply(event.asInstanceOf[T])

  def apply(event: T): StateOutput
}
