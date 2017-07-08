package org.mandrake.runners.slick2d

import org.mandrake.simulation.{Event, Simulation}
import org.newdawn.slick.{AppGameContainer, BasicGame, GameContainer, Graphics}

object Slick2DRunner {
  def run(rootSimulation: Simulation): Unit = {
    val app = new AppGameContainer(new BasicGame("") {
      private var currentSimulation = rootSimulation
      private var outEvents = Vector[Event]()

      override def init(container: GameContainer): Unit = {}

      override def update(container: GameContainer, delta: Int): Unit =
        currentSimulation(Vector(SlickInput(container, delta))) match {
          case (simulation, events) =>
            currentSimulation = simulation
            outEvents = events
        }

      override def render(container: GameContainer, g: Graphics): Unit = {
        g.setAntiAlias(true)
        outEvents.foreach(renderer(container, g))
      }
    })
    app.setDisplayMode(app.getScreenWidth, app.getScreenHeight, false)
    app.start()
  }

  def renderer(container: GameContainer, g: Graphics)(event: Event): Unit = event match {
    case event: RenderEvent => event.render(g)
    case _ =>
  }

  trait RenderEvent extends Event {
    def render(g: Graphics): Unit
  }

  case class SlickInput(container: GameContainer, delta: Int) extends Event
}
