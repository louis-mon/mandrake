package org.mandrake.sphere

import org.mandrake.runners.slick2d.Slick2DRunner
import org.mandrake.runners.slick2d.Slick2DRunner.{RenderEvent, SlickInput}
import org.mandrake.simulation.{SimpleInputState, Simulation, StateOutput}
import org.newdawn.slick.{Color, Graphics}
import org.newdawn.slick.geom.Circle

case class Player(angle: Float, color: Color)

class SphereHuntInit() extends SimpleInputState[SlickInput] {
  override def apply(event: SlickInput): StateOutput = {
    val input = event.container.getInput
    val nbPlayers = Math.min(input.getControllerCount, 4)
    val colors = Vector(Color.blue, Color.red, Color.green, Color.yellow, Color.cyan, Color.magenta)
    StateOutput(SphereHunt(0.until(nbPlayers).map(i => Player(i*2 * Math.PI.toFloat / nbPlayers, colors(i))).toVector))
  }
}

case class SphereHunt(players: Vector[Player]) extends SimpleInputState[SlickInput] {
  override def apply(event: SlickInput): StateOutput = {
    val container = event.container
    val sphereRadius = Math.min(container.getHeight, container.getWidth) / 3
    val playerSize = 20
    StateOutput(this, new RenderEvent {
      override def render(g: Graphics): Unit = {
        g.pushTransform()
        g.translate(container.getWidth / 2, container.getHeight / 2)
        g.setColor(Color.white)
        g.draw(new Circle(0, 0, sphereRadius))
        players.foreach{player =>
          g.setColor(player.color)
          g.fill(new Circle((sphereRadius + playerSize) * Math.cos(player.angle).toFloat,
            (sphereRadius + playerSize) * Math.sin(player.angle).toFloat, playerSize))
        }
        g.popTransform()
      }
    })
  }
}

object SphereHunt extends App {
  Slick2DRunner.run(Simulation(Vector(new SphereHuntInit())))
}
