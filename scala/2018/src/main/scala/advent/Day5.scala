package advent

import cats.effect.IO

object Day5 extends AdventOfCode("day5.txt") {
  type Polymer = Vector[Char]

  def isDifferentPolarity(c1: Char, c2: Char): Boolean =
    c1.toLower == c2.toLower && c1 != c2

  def eliminate(polymer: Polymer): Polymer = {
    val length = polymer.length

    def newPreviousIndex(previous: Int, eliminated: Set[Int]): Int =
      if (previous >= 0 && eliminated.contains(previous)) newPreviousIndex(previous - 1, eliminated) else previous

    /** This is an attempt to rationally process the polymer in one pass.
      * The notion is that the current index will march forward. If a
      * pair has to be eliminated, the current will move forward one
      * index, but the previous index has to go back to the most
      * recently uneliminated index. The set of eliminated indices
      * gets stored in the eliminated parameter.
      *
      * When the current index has extended past the end of the
      * polymer, simply filter out the filtered indices.
      */
    def loop(previous: Int, current: Int, eliminated: Set[Int]): Polymer = {
      if (current == length) polymer.zipWithIndex.collect {
        case (value, index) if !(eliminated.contains(index)) => value
      } else {
        if (previous >= 0 && isDifferentPolarity(polymer(previous), polymer(current))) {
          // Add previous and current to eliminated, increment current, and find the correct previous
          loop(newPreviousIndex(previous - 1, eliminated), current + 1, eliminated + current + previous)
        } else {
          loop(current, current + 1, eliminated)
        }
      }
    }

    loop(0, 1, Set.empty)
  }

  def removed(polymer: Polymer): Polymer = {
    polymer.map(_.toLower)
      .distinct
      .map { id => eliminate(polymer.filter(_.toLower != id)) }
      .sortBy(_.size)
      .head
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
