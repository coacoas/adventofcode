package advent

object Day3 {
  case class Point(x: Int, y: Int)

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int) {
    def points: List[Point] = for {
      x <- (left until left + width).toList
      y <- (top until top + height).toList
    } yield Point(x,y)
  }

  object Claim {
    val regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
    def apply(input: String): Claim = input match {
      case regex(id, left, top, width, height) =>
        Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
    }
  }

  def plot(claims: List[Claim]): Map[Point, List[Int]] =
    claims.flatMap { claim =>
      claim.points.map { (_ -> claim.id) }.toMap
    }.groupBy(_._1).map { case (point, data) => (point, data.map(_._2)) }

  def conflictingArea(plot: Map[Point, List[Int]]): Int =
    plot.filter { case (_, ids) => ids.size > 1  }
      .size

  def nonOverlapping(claims: List[Claim], plot: Map[Point, List[Int]]): Int = {
    val allIds = claims.map(_.id).toSet

    val overlaps = plot
      .collect { case (point, ids) if ids.size > 1 => ids }
      .flatten
      .toSet

    (allIds -- overlaps).head
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromURL(this.getClass.getClassLoader.getResource("day3.txt"))
    val input = source
      .getLines
      .toList
    source.close

    val claims = input.map(Claim.apply)
    val claimPlot = plot(claims)

    println(s"Overlapped space: ${conflictingArea(claimPlot)}")
    println(s"Non-overlapping: ${nonOverlapping(claims, claimPlot)}")
  }

}
