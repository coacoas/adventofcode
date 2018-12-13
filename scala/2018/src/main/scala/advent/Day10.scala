package advent

import cats.effect.IO

object Day10 extends AdventOfCode("day10.txt") {
  case class Velocity(x: Int, y: Int)
  case class Point(x: Int, y: Int, v: Velocity) {
    def step: Point = this.copy(x = x + v.x, y = y + v.y)
  }
  case class Bounds(minX: Int, minY: Int, maxX: Int, maxY: Int)
  object Bounds {
    def apply(points: List[Point]): Bounds = {
      val xs = points.map(_.x)
      val ys = points.map(_.y)

      Bounds(xs.min, ys.min, xs.max, ys.max)
    }
  }

  val inputRE = """position=\<\s*([+-]?\d+),\s*([+-]?\d+)> velocity=\<\s*([+-]?\d+),\s*([+-]?\d+)>""".r
  def parse(s: String): Point = s match {
    case inputRE(x, y, vx, vy) => Point(x.toInt, y.toInt, Velocity(vx.toInt, vy.toInt))
  }

  def show(points: List[Point]): String = {
    val Bounds(minX, minY, maxX, maxY) = Bounds(points)

    (minY to maxY).map { y =>
      (minX to maxX).map { x =>
        if (points.exists(p => p.x == x && p.y == y)) '#' else '.'
      }.mkString
    }.mkString("\n")
  }

  def mainIO(data: List[String]): IO[Unit] = IO {
    def move(
      points: List[Point],
      last: List[Point],
      lastYDiff: Int,
      iteration: Int
    ): List[Point] = Bounds(points) match {
      case Bounds(_, minY, _, maxY) if (maxY - minY) > lastYDiff =>
        println(s"Took ${iteration - 1} seconds")
        last
      case Bounds(_, minY, _, maxY) =>
        if (iteration % 1000 == 0) println(s"Iteration $iteration")
        move(points.map(_.step), points, maxY - minY, iteration + 1)
    }

    println(show(move(data.map(parse), List.empty, Int.MaxValue, 0)))
  }
}
