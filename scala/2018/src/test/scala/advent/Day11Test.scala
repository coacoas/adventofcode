package advent

import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }


class Day11Test extends FlatSpec with Matchers with PropertyChecks {

// Fuel cell at  122,79, grid serial number 57: power level -5.
// Fuel cell at 217,196, grid serial number 39: power level  0.
// Fuel cell at 101,153, grid serial number 71: power level  4.

  val table = Table(
    ("x", "y", "serial", "power"),
    (3, 5, 8, 4),
    (122, 79, 57, -5),
    (217, 196, 39, 0),
    (101, 153, 71, 4))


  "power level" should "be computed" in forAll(table) {
    (x, y, serial, power) =>

    Day11.power(x, y, serial) should equal(power)
  }

  "sums" should "fill a table with partial sums" in {
    val data = Vector(
      Vector(1, 1, 1, 1, 1),
      Vector(1, 1, 1, 1, 1),
      Vector(1, 1, 1, 1, 1),
      Vector(1, 1, 1, 1, 1),
      Vector(1, 1, 1, 1, 1))

    val expected = Vector(
      Vector(1, 2, 3, 4, 5),
      Vector(2, 4, 6, 8, 10),
      Vector(3, 6, 9, 12, 15),
      Vector(4, 8, 12, 16, 20),
      Vector(5, 10, 15, 20, 25))

    Day11.sums(data) should equal(expected)
  }
}
