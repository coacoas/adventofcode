package advent

import cats.effect.IO
import advent.Zipper._

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

  def eliminateZipper(polymer: Polymer): Polymer = {
    def loop(current: Zipper[Char]): Polymer = {
      if (current.tail.isEmpty) current.toVector
      else {
        if (isDifferentPolarity(current.head, current.tail.head)) {
          val removed = current.remove.remove
          if (removed.before.isEmpty) loop(removed) else loop(removed.prev)
        } else {
          loop(current.next)
        }
      }
    }

    loop(polymer.toList.toZipper)
  }

  def removedZipper(polymer: Polymer): Polymer =
    polymer.map(_.toLower)
      .distinct
      .map { id => eliminateZipper(polymer.filter(_.toLower != id)) }
      .sortBy(_.size)
      .head

  def time[A](operation: String)(f: => A): A = {
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    println(s"$operation took ${end - start} milliseconds")
    result
  }

  override def mainIO(input: List[String]): IO[Unit] = {
    IO {
      val polymer = input.head.toVector
      time("Original length") {
        println(s"Original length: ${polymer.size}")
      }
      time("Polymer length") {
        println(s"Polymer length (using Set): ${eliminate(polymer).size}")
      }
      time("Shortest elimination") {
        println(s"Shortest elimination (using Set): ${removed(polymer).size}")
      }
      time("Polymer length (zipper)") {
        println(s"Polymer length (using Zipper): ${eliminateZipper(polymer).size}")
      }
      time("Shortest elimination (zipper)") {
        println(s"Shortest elimination (using Zipper): ${removedZipper(polymer).size}")
      }
    }
  }
}
