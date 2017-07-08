package org.mandrake.exemple

import org.mandrake.runners.slick2d.Slick2DRunner
import org.mandrake.runners.slick2d.Slick2DRunner.{RenderEvent, SlickInput}
import org.mandrake.simulation._
import org.newdawn.slick._
import org.newdawn.slick.geom._

import scala.util.Random

object Stars extends App {

  implicit class VectorExtra(p: Vector2f) {
    def points = Array(p.x, p.y)
  }

  implicit class RandomExtras(random: Random) {
    def nextInt(from: Int, until: Int): Int = random.nextInt(until - from) + from

    def nextFloat(from: Float, until: Float): Float = from + random.nextFloat() * (until - from)
  }

  case class Delta(delta: Float) extends Event

  case class Star(pos: Vector2f,
                  angle: Float,
                  vAngle: Float,
                  points: Vector[(Float, Float)]) extends SimpleInputState[Delta] {
    def apply(delta: Delta): StateOutput = {
      val newState = copy(
        angle = angle + vAngle * delta.delta,
        points = points.map(Star.movePoint(delta.delta)))
      StateOutput(newState, new RenderEvent {
        override def render(g: Graphics): Unit = g.draw(newState.shape)
      })
    }

    def shape: Shape = {
      def rotate(i: Int) = Transform.createRotateTransform((Math.PI * i / points.size).toFloat)

      new Polygon(points.flatMap(p => Vector(p._1, Star.pointHMin)).zipWithIndex.flatMap { case (height, i) =>
        rotate(i).transform(new Vector2f(height, 0)).points
      }.toArray)
        .transform(baseTr)
    }

    private def baseTr = Transform.createTranslateTransform(pos.x, pos.y)
      .concatenate(Transform.createRotateTransform(angle))
  }

  case class AddStar() extends SimpleInputState[SlickInput] {
    override def apply(event: SlickInput): StateOutput = {
      val input = event.container.getInput
      val newPos = new Vector2f(input.getMouseX, input.getMouseY)
      val newStar =
        if (input.isMousePressed(0)) Vector(Star.create(newPos))
        else Vector()
      StateOutput(newStar :+ this, Vector(Delta(event.delta / 1000f)))
    }
  }

  object Star {
    val pointHMin = 5f
    val pointHMax = 40f

    def movePoint(vFactor: Float)(p: (Float, Float)): (Float, Float) = {
      val newPos = p._1 + p._2 * vFactor
      if (newPos > pointHMax) (pointHMax, -p._2)
      else if (newPos < pointHMin) (pointHMin, -p._2)
      else (newPos, p._2)
    }

    def create(pos: Vector2f) = Star(
      angle = Random.nextFloat() * Math.PI.toFloat * 2,
      vAngle = Random.nextFloat(-Math.PI.toFloat, Math.PI.toFloat),
      pos = pos,
      points = 1.to(Random.nextInt(5) + 5)
        .map(_ => (Random.nextFloat(pointHMin, pointHMax + 1), Random.nextFloat(-20, 20))).toVector)
  }

  Slick2DRunner.run(Simulation(Vector(AddStar())))
}
