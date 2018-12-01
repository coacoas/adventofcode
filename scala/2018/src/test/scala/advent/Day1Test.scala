package advent

import org.scalatest.{ FlatSpec, Matchers }

class Day1Test extends FlatSpec
    with Matchers {

  "+1, +1, +1" should "result in  3" in {
    Day1.shift(List("+1", "+1", "+1")) should equal(3)
  }

  "+1, +1, -2" should "result in  0" in {
    Day1.shift(List("+1", "+1", "-2")) should equal(0)
  }

  "-1, -2, -3" should "result in -6" in {
    Day1.shift(List("-1", "-2", "-3")) should equal(-6)
  }

  "+1 -1" should "loop until 0" in {
    Day1.loop(List("+1", "-1")) should equal(0)
  }

  "+3, +3, +4, -2, -4" should "first reach 10 twice" in {
    Day1.loop(List("+3", "+3", "+4", "-2", "-4")) should equal(10)
  }

  "-6, +3, +8, +5, -6" should "first reach 5 twice" in {
    Day1.loop(List("-6", "+3", "+8", "+5", "-6")) should equal(5)
  }

  "+7, +7, -2, -7, -4" should "first reach 14 twice" in {
    Day1.loop(List("+7", "+7", "-2", "-7", "-4")) should equal(14)
  }

}
