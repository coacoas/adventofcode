package advent

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.Inside
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import fastparse._

class Day8Test extends FlatSpec with Matchers with PropertyChecks with Inside {
  import Day8.Tree

  "num" should "parse a number" in {
    (i: Int, j: Int) => 
    val result = parse(s"$i $j", Day8.num(_))

    val expected = Parsed.Success(i, s"$i".length)

    result should equal(expected)
  }

  "test data" should "produce a tree" in {
    val testdata = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

    val expected = Tree(
      List(
        Tree(List(), List(10, 11, 12)),
        Tree(List(
          Tree(List(), List(99))
        ), List(2))
      ), List(1, 1, 2))

    Day8.parse(testdata).unsafeRunSync should equal(expected)
  }

  it should "have a root value of 66" in {
    val testdata = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

    val test = for {
      result <- Day8.parse(testdata)
    } yield {
      result.value should equal(66)
    }

    test.unsafeRunSync
  }

}
