package advent

import org.scalatest.{ FlatSpec, Matchers }


class Day10Test extends FlatSpec with Matchers {
  import Day10.{Point, Velocity}

  val data = """|position=< 9,  1> velocity=< 0,  2>
                |position=< 7,  0> velocity=<-1,  0>
                |position=< 3, -2> velocity=<-1,  1>
                |position=< 6, 10> velocity=<-2, -1>
                |position=< 2, -4> velocity=< 2,  2>
                |position=<-6, 10> velocity=< 2, -2>
                |position=< 1,  8> velocity=< 1, -1>
                |position=< 1,  7> velocity=< 1,  0>
                |position=<-3, 11> velocity=< 1, -2>
                |position=< 7,  6> velocity=<-1, -1>
                |position=<-2,  3> velocity=< 1,  0>
                |position=<-4,  3> velocity=< 2,  0>
                |position=<10, -3> velocity=<-1,  1>
                |position=< 5, 11> velocity=< 1, -2>
                |position=< 4,  7> velocity=< 0, -1>
                |position=< 8, -2> velocity=< 0,  1>
                |position=<15,  0> velocity=<-2,  0>
                |position=< 1,  6> velocity=< 1,  0>
                |position=< 8,  9> velocity=< 0, -1>
                |position=< 3,  3> velocity=<-1,  1>
                |position=< 0,  5> velocity=< 0, -1>
                |position=<-2,  2> velocity=< 2,  0>
                |position=< 5, -2> velocity=< 1,  2>
                |position=< 1,  4> velocity=< 2,  1>
                |position=<-2,  7> velocity=< 2, -2>
                |position=< 3,  6> velocity=<-1, -1>
                |position=< 5,  0> velocity=< 1,  0>
                |position=<-6,  0> velocity=< 2,  0>
                |position=< 5,  9> velocity=< 1, -2>
                |position=<14,  7> velocity=<-2,  0>
                |position=<-3,  6> velocity=< 2, -1>
                |""".stripMargin


  "data parse" should "provide a list of lights" in {
    val expected = List(
      Point( 9,  1, Velocity( 0,  2)),
      Point( 7,  0, Velocity(-1,  0)),
      Point( 3, -2, Velocity(-1,  1)),
      Point( 6, 10, Velocity(-2, -1)),
      Point( 2, -4, Velocity( 2,  2)),
      Point(-6, 10, Velocity( 2, -2)),
      Point( 1,  8, Velocity( 1, -1)),
      Point( 1,  7, Velocity( 1,  0)),
      Point(-3, 11, Velocity( 1, -2)),
      Point( 7,  6, Velocity(-1, -1)),
      Point(-2,  3, Velocity( 1,  0)),
      Point(-4,  3, Velocity( 2,  0)),
      Point(10, -3, Velocity(-1,  1)),
      Point( 5, 11, Velocity( 1, -2)),
      Point( 4,  7, Velocity( 0, -1)),
      Point( 8, -2, Velocity( 0,  1)),
      Point(15,  0, Velocity(-2,  0)),
      Point( 1,  6, Velocity( 1,  0)),
      Point( 8,  9, Velocity( 0, -1)),
      Point( 3,  3, Velocity(-1,  1)),
      Point( 0,  5, Velocity( 0, -1)),
      Point(-2,  2, Velocity( 2,  0)),
      Point( 5, -2, Velocity( 1,  2)),
      Point( 1,  4, Velocity( 2,  1)),
      Point(-2,  7, Velocity( 2, -2)),
      Point( 3,  6, Velocity(-1, -1)),
      Point( 5,  0, Velocity( 1,  0)),
      Point(-6,  0, Velocity( 2,  0)),
      Point( 5,  9, Velocity( 1, -2)),
      Point(14,  7, Velocity(-2,  0)),
      Point(-3,  6, Velocity( 2, -1)))

    data.split("\n").map(Day10.parse) should equal(expected)
  }

  "show" should "display the dots" in {
    val small = List(
      Point(11, 11, Velocity(0, 0)),
      Point(12, 11, Velocity(0, 0)),
      Point(12, 13, Velocity(0, 0)),
      Point(15, 11, Velocity(0, 0)))

    Day10.show(small) should equal(
      """|##..#
         |.....
         |.#...""".stripMargin)
  }
}
