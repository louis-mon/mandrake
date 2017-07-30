package org.mandrake.sphere

import org.mandrake.runners.slick2d.Slick2DRunner
import org.mandrake.runners.slick2d.Slick2DRunner.{RenderEvent, SlickInput}
import org.mandrake.simulation.{SimpleInputState, Simulation, StateOutput}
import org.newdawn.slick.Graphics
import org.newdawn.slick.geom.Circle

class SphereHunt extends SimpleInputState[SlickInput] {
  override def apply(event: SlickInput): StateOutput = {
    val sphereRadius = Math.min(event.container.getHeight, event.container.getWidth) / 3
    StateOutput.apply(this, new RenderEvent {
      override def render(g: Graphics): Unit = {
        g.draw(new Circle(event.container.getWidth / 2, event.container.getHeight / 2, sphereRadius))
      }
    })
  }
}

object SphereHunt extends App {
  Slick2DRunner.run(Simulation(Vector(new SphereHunt())))
}
