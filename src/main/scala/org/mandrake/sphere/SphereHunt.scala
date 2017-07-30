package org.mandrake.sphere

import org.mandrake.runners.slick2d.Slick2DRunner
import org.mandrake.runners.slick2d.Slick2DRunner.{RenderEvent, SlickInput}
import org.mandrake.simulation.{SimpleInputState, Simulation, StateOutput}
import org.newdawn.slick.geom.{Circle, Vector2f}
import org.newdawn.slick.{Color, Graphics, Input}

object Angle {
  val PI2: Double = Math.PI * 2

  def quadrant(angle: Double): Double = angle - ((angle / PI2).floor * PI2).toFloat
}

case class Player(index: Int, angle: Float, color: Color) {
  def move(event: SlickInput): Player = {
    val input = event.container.getInput
    val tangent = new Vector2f(-Math.sin(angle).toFloat, Math.cos(angle).toFloat)
    val axisDir = new Vector2f(input.getAxisValue(index, 1), input.getAxisValue(index, 0))
    val direction = if (axisDir.lengthSquared() > 0.25) if (tangent.dot(axisDir) > 0) 1 else -1 else 0
    copy(angle = Angle.quadrant(angle + direction * event.delta * Math.PI.toFloat / 1000).toFloat)
  }
}

class SphereHuntInit() extends SimpleInputState[SlickInput] {
  override def apply(event: SlickInput): StateOutput = {
    val input = event.container.getInput
    input.initControllers()
    val colors = Vector(Color.blue, Color.red, Color.green, Color.yellow)
    val nbPlayers = Math.min(input.getControllerCount, colors.size)
    StateOutput(SphereHunt(0.until(nbPlayers).map(i => Player(i, i * Angle.PI2.toFloat / nbPlayers, colors(i))).toVector))
  }
}

case class SphereHunt(players: Vector[Player]) extends SimpleInputState[SlickInput] {
  override def apply(event: SlickInput): StateOutput = {
    val container = event.container
    val sphereRadius = Math.min(container.getHeight, container.getWidth) / 3
    val playerSize = 20
    val newState = copy(
      players = players.map(_.move(event))
    )
    StateOutput(newState, new RenderEvent {
      override def render(g: Graphics): Unit = {
        g.pushTransform()
        g.translate(container.getWidth / 2, container.getHeight / 2)
        g.setColor(Color.white)
        g.draw(new Circle(0, 0, sphereRadius))
        newState.players.foreach { player =>
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
