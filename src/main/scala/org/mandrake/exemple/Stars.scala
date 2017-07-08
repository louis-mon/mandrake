package org.mandrake.exemple

import org.mandrake.runners.slick2d.Slick2DRunner
import org.mandrake.runners.slick2d.Slick2DRunner.{RenderEvent, SlickInput}
import org.mandrake.simulation._
import org.newdawn.slick._
import org.newdawn.slick.geom._

import scala.util.Random

object Stars extends App {

  object Angle {
    val PI2: Double = Math.PI * 2
    def quadrant(angle: Double): Double = angle - (angle / PI2).floor * PI2
  }
  implicit class VectorExtra(p: Vector2f) {
    def points = Array(p.x, p.y)
    def angle: Double = Math.atan2(p.y, p.x)
    def angleWith(other: Vector2f): Double = Angle.quadrant(angle - other.angle)
    def -(other: Vector2f): Vector2f = p.copy().sub(other)
  }

  implicit class ColorExtras(c: Color) {
    def +(other: Color): Color = c.addToCopy(other)
    def -(other: Color): Color = c.addToCopy(other * -1)
    def *(value: Float): Color = c.scaleCopy(value)
    def interpolate(other: Color, alpha: Float): Color = c + (other - c) * alpha
  }

  object Vector2fUtils {
    def from(array: Array[Float]) = new Vector2f(array(0), array(1))
  }

  implicit class RandomExtras(random: Random) {
    def nextInt(from: Int, until: Int): Int = random.nextInt(until - from) + from

    def nextFloat(from: Float, until: Float): Float = from + random.nextFloat() * (until - from)

    def choose[T](values: Seq[T]): T = values(random.nextInt(values.size))
  }

  implicit class ShapeExtras(shape: Shape) {
    def vectors: Vector[Vector2f] = shape.getPoints.grouped(2).map(Vector2fUtils.from).toVector
  }

  case class Delta(delta: Float) extends Event

  case class GradientShape() extends ShapeFill {
    override def colorAt(shape: Shape, x: Float, y: Float): Color = {
      Vector(Color.blue, Color.transparent)(shape.indexOf(x, y) % 2)
    }

    override def getOffsetAt(shape: Shape, x: Float, y: Float): Vector2f = new Vector2f()
  }

  case class Star(pos: Vector2f,
                  angle: Float,
                  vAngle: Float,
                  color: Color,
                  points: Vector[(Float, Float)]) extends SimpleInputState[Delta] {
    def apply(delta: Delta): StateOutput = {
      val newState = copy(
        angle = angle + vAngle * delta.delta,
        points = points.map(Star.movePoint(delta.delta)))
      StateOutput(newState, new RenderEvent {
        override def render(g: Graphics): Unit = g.fill(newState.shape, new ShapeFill {
          override def colorAt(shape: Shape, x: Float, y: Float): Color = color

          override def getOffsetAt(shape: Shape, x: Float, y: Float) = new Vector2f()
        })
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
    val pointHMin = 10f
    val pointHMax = 60f

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
      points = 1.to(Random.nextInt(3) + 3)
        .map(_ => (Random.nextFloat(pointHMin, pointHMax + 1), Random.nextFloat(-20, 20))).toVector,
      color = new Color(Random.nextFloat(0f, 1f), Random.nextFloat(0f, 1f),Random.nextFloat(0f, 1f))
    )
  }

  Slick2DRunner.run(Simulation(Vector(AddStar())))
}
