package advent

import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import fastparse._

class Day16Test extends FlatSpec with Matchers with PropertyChecks {
  import Day16._

  val testInput = """|Before: [3, 2, 1, 1]
                     |9 2 1 2
                     |After:  [3, 2, 2, 1]
                     |""".stripMargin.lines.toList


  "num" should "parse a number and return it" in {
    fastparse.parse("14", num(_)) should equal(
      Parsed.Success(14, 2)
    )
  }

  "vector" should "parse a vector of four numbers and return it" in {
    fastparse.parse("[1, 20, 3, 4]", vector(_)) should equal(
      Parsed.Success(Vector(1, 20, 3, 4), 13)
    )
  }

  "instruction" should "parse four integers into an instruction type" in {
    fastparse.parse("10 11 12 13", instruction(_)) should equal(
      Parsed.Success(Instruction(10, 11, 12, 13), 11)
    )
  }

  "state" should "parse Before lines" in {
    fastparse.parse("Before: [10, 11, 12, 13]", state("Before")(_)) should equal(
      Parsed.Success(Vector(10, 11, 12, 13), 24)
    )
  }

  "test" should "parse a single test" in {
    fastparse.parse(testInput.mkString("\n") + "\n", test(_)) should equal(
      Parsed.Success(Test(
        Vector(3, 2, 1, 1),
        Day16.Instruction(9, 2, 1, 2),
        Vector(3, 2, 2, 1)
      ), 50)
    )
  }

  "parseTests" should "parse input data into tests" in {

    val result = Day16.parseTests(testInput)

    val expected = List(Day16.Test(
      Vector(3, 2, 1, 1),
      Day16.Instruction(9, 2, 1, 2),
      Vector(3, 2, 2, 1)
    ))

    result should equal(expected)
  }


  it should "parse multiple tests" in {

    val result = Day16.parseTests(testInput ++ testInput)

    val expected = List(Day16.Test(
      Vector(3, 2, 1, 1),
      Day16.Instruction(9, 2, 1, 2),
      Vector(3, 2, 2, 1)
    ), Day16.Test(
      Vector(3, 2, 1, 1),
      Day16.Instruction(9, 2, 1, 2),
      Vector(3, 2, 2, 1)
    ))

    result should equal(expected)
  }

}
