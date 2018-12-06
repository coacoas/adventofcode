package advent

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }


class Day5Test extends FlatSpec with Matchers with PropertyChecks {
  "characters of different polarities" should "be eliminated" in forAll(
    Gen.alphaLowerChar,
    Gen.alphaUpperChar
  ) {
    (c1, c2) =>
    Day5.isDifferentPolarity(c1, c1.toUpper) should be(true)
    Day5.isDifferentPolarity(c2, c2.toLower) should be(true)
    Day5.isDifferentPolarity(c1, c1) should be(false)
    Day5.isDifferentPolarity(c2, c2) should be(false)
  }

  "test data" should "properly eliminate" in {
    val data = "dabAcCaCBAcCcaDA"
    val expected = "dabCBAcaDA"

    Day5.eliminate(data.toVector) should contain theSameElementsInOrderAs(expected)
  }

  it should "do it again" in {
    val data = "abcdefgGFEDCBA"
    val expected = ""

    Day5.eliminate(data.toVector) should contain theSameElementsInOrderAs(expected)
  }

  it should "do it one more time" in {
    val data = "bcdefgGFEDCBA"
    val expected = "A"

    Day5.eliminate(data.toVector) should contain theSameElementsInOrderAs(expected)
  }
}
