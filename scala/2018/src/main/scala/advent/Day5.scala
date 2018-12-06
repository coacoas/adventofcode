package advent

import cats.effect.IO

object Day5 extends AdventOfCode("day5.txt") {
  type Polymer = Vector[Char]

  def isDifferentPolarity(c1: Char, c2: Char): Boolean =
    c1.toLower == c2.toLower && c1 != c2

  def eliminate(polymer: Polymer): Polymer = {
    val length = polymer.length
    def newI1(i1: Int, eliminated: Set[Int]): Int =
      if (i1 >= 0 && eliminated.contains(i1)) newI1(i1 - 1, eliminated) else i1

    def loop(i1: Int, i2: Int, eliminated: Set[Int]): Polymer =
      if (i2 == length) polymer.zipWithIndex.collect {
        case (value, index) if !(eliminated.contains(index)) => value
      } else {
        if (i1 >= 0 && isDifferentPolarity(polymer(i1), polymer(i2))) {
          // Add i1 and i2 to eliminated, increment i2, and find the correct i1
          loop(newI1(i1 - 1, eliminated), i2 + 1, eliminated + i2 + i1)
        } else {
          loop(i2, i2 + 1, eliminated)
        }
      }

    loop(0, 1, Set.empty)
  }

  def removed(polymer: Polymer): Polymer = {
    polymer.map(_.toLower)
      .distinct
      .map { id => id -> eliminate(polymer.filter(_.toLower != id)) }
      .sortBy(_._2.size)
      .head
      ._2
  }

  override def mainIO(input: List[String]): IO[Unit] = {
    IO {
      val polymer = input.head.toVector
      println(s"Original length: ${polymer.size}")
      println(s"Polymer length: ${eliminate(polymer).size}")
      println(s"Shortest elimination: ${removed(polymer).size}")
    }
  }

}
