package advent

import advent.Day3.Claim
import eu.timepit.refined.types.numeric.{ NonNegInt, PosInt }
import eu.timepit.refined.scalacheck.numeric._
import org.scalatest.EitherValues
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }


class Day3Test extends FlatSpec with Matchers with EitherValues with PropertyChecks {

  "Input" should "be able to be parsed into claims" in {
    val data = Table(("Input", "Claim"),
      ("#1 @ 1,3: 4x4", Claim(1, 1, 3, 4, 4)),
      ("#2 @ 3,1: 4x4", Claim(2, 3, 1, 4, 4)),
      ("#3 @ 5,5: 2x2", Claim(3, 5, 5, 2, 2)))

    forAll(data) { (input, expected) =>
      Claim(input) should equal(expected)
    }
  }

  val input = List(
    Claim(1, 1, 3, 4, 4),
    Claim(2, 3, 1, 4, 4),
    Claim(3, 5, 5, 2, 2))

  "Test data" should "produce 4" in {
    Day3.conflictingArea(Day3.plot(input)) should equal(4)
  }

  it should "show that rectangle 3 is the only one not overlapping" in {
    Day3.nonOverlapping(input, Day3.plot(input)) should equal(3)
  }
}
