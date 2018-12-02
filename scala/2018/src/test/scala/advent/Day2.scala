package advent

import org.scalatest.{ FlatSpec, Matchers }

class Day2Test extends FlatSpec with Matchers {

  "sample data" should "generate 12" in {
    val data = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab")

    Day2.checksum(data)
  }


  "commonletters" should "return the common letters" in {
    Day2.commonLetters("abcde", "abdce") should equal ("abe")
  }

  "diff" should "return the common letters of the two strings that differ above by one letter in the same position" in {
    val data = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz")

    Day2.diff(data) should equal("fgij")
  }
}
