package org.mandrake.exemple

import org.mandrake.runners.slick2d.Slick2DRunner
import org.mandrake.runners.slick2d.Slick2DRunner.{RenderEvent, SlickInput}
import org.mandrake.simulation._
import org.newdawn.slick._
import org.newdawn.slick.geom._

import scala.util.Random

object Stars extends App {

  case class Delta(seconds: Float) extends Event

  implicit class VectorExtra(p: Vector2f) {
    def points = Array(p.x, p.y)

    def angleWith(other: Vector2f): Double = Angle.quadrant(angle - other.angle)

    def angle: Double = Math.atan2(p.y, p.x)

    def -(other: Vector2f): Vector2f = p.copy().sub(other)
  }

  implicit class ColorExtras(c: Color) {
    def interpolate(other: Color, alpha: Float): Color = c + (other - c) * alpha

    def +(other: Color): Color = c.addToCopy(other)

    def -(other: Color): Color = c.addToCopy(other * -1)

    def *(value: Float): Color = c.scaleCopy(value)

    def withAlpha(alpha: Float): Color = new Color(c.r, c.g, c.b, alpha)
  }

  case class Star(pos: Vector2f,
                  angle: Float,
                  color: Color,
                  points: Vector[Float]) {
    def render = new RenderEvent {
      override def render(g: Graphics): Unit = g.fill(shape, new ShapeFill {
        override def colorAt(shape: Shape, x: Float, y: Float): Color =
          Vector(color.withAlpha(0.5f), color)(shape.indexOf(x, y) % 2)

        override def getOffsetAt(shape: Shape, x: Float, y: Float) = new Vector2f()
      })
    }

    def shape: Shape = {
      def rotate(i: Int) = Transform.createRotateTransform((Math.PI * i / points.size).toFloat)

      new Polygon(points.flatMap(p => Vector(p, StarState.pointHMin)).zipWithIndex.flatMap { case (height, i) =>
        rotate(i).transform(new Vector2f(height, 0)).points
      }.toArray)
        .transform(baseTr)
    }

    private def baseTr = Transform.createTranslateTransform(pos.x, pos.y)
      .concatenate(Transform.createRotateTransform(angle))
  }

  implicit class RandomExtras(random: Random) {
    def nextInt(from: Int, until: Int): Int = from + random.nextInt(until - from)

    def nextFloat(from: Float, until: Float): Float = from + random.nextFloat() * (until - from)

    def choose[T](values: Seq[T]): T = values(random.nextInt(values.size))
  }

  implicit class ShapeExtras(shape: Shape) {
    def vectors: Vector[Vector2f] = shape.getPoints.grouped(2).map(Vector2fUtils.from).toVector
  }

  case class StarState(star: Star, angleSpeed: Float) extends SimpleInputState[Delta] {
    def apply(delta: Delta): StateOutput = {
      val newState = copy(star.copy(angle = star.angle + angleSpeed * delta.seconds))
      StateOutput(newState, newState.star.render)
    }
  }

  case class GrowingStar(star: Star, size: Float) extends SimpleInputState[Delta] {
    override def apply(event: Delta): StateOutput = {
      val newSize = size + event.seconds
      val newState =
        if (newSize >= 1) StarState(star, Random.nextFloat(-Math.PI.toFloat, Math.PI.toFloat))
        else copy(size = newSize)
      StateOutput(newState, new RenderEvent {
        override def render(g: Graphics): Unit = {
          g.pushTransform()
          g.scale(size, size)
          star.render.render(g)
          g.popTransform()
        }
      })
    }
  }

  case class AddStar() extends SimpleInputState[SlickInput] {
    override def apply(event: SlickInput): StateOutput = {
      val input = event.container.getInput
      val newPos = new Vector2f(input.getMouseX, input.getMouseY)
      val newStar =
        if (input.isMousePressed(0)) Vector(StarState.create(newPos))
        else Vector()
      StateOutput(newStar :+ this, Vector(Delta(event.delta / 1000f)))
    }
  }

  object Angle {
    val PI2: Double = Math.PI * 2

    def quadrant(angle: Double): Double = angle - (angle / PI2).floor * PI2
  }

  object Vector2fUtils {
    def from(array: Array[Float]) = new Vector2f(array(0), array(1))
  }

  object StarState {
    val pointHMin = 10f
    val pointHMax = 60f

    def create(pos: Vector2f) = GrowingStar(
      star = Star(
        angle = Random.nextFloat() * Math.PI.toFloat * 2,
        pos = pos,
        points = 1.to(Random.nextInt(3) + 3)
          .map(_ => Random.nextFloat(pointHMin, pointHMax + 1)).toVector,
        color = new Color(Random.nextFloat(0f, 1f), Random.nextFloat(0f, 1f), Random.nextFloat(0f, 1f))),
      size = 0.01f
    )
  }

  Slick2DRunner.run(Simulation(Vector(AddStar())))
}
