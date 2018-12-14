package advent

import cats.effect.IO
import cats.implicits._
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

  def eliminateZipper(polymer: Polymer): Polymer = {
    /** This is my second implementation, using a Zipper data type to
      * simulate mutation (although this is still entirey pure). It
      * seems to me that this is clearer, as well as much more performant.
      */
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

  def removed(polymer: Polymer, elimination: Polymer => Polymer): Polymer = {
    polymer.map(_.toLower)
      .distinct
      .map { id => elimination(polymer.filter(_.toLower != id)) }
      .sortBy(_.size)
      .head
  }

  override def mainIO(input: List[String]): IO[Unit] = for {
    polymer <- IO.pure(input.head.toVector)
    _ <- time("Original length")(polymer.size)
    _ <- time("Polymer length")(eliminate(polymer).size)
    _ <- time("Shortest elimination")(removed(polymer, eliminate(_)).size)
    _ <- time("Polymer length (zipper)")(eliminateZipper(polymer).size)
    _ <- time("Shortest elimination (zipper)")(removed(polymer, eliminateZipper(_)).size)
  } yield ()

  def time[A](operation: String)(f: => A): IO[A] = for {
    start <- IO(System.currentTimeMillis())
    result = f
    end <- IO(System.currentTimeMillis())
    _ <- IO(println(s"$operation took ${end - start} milliseconds"))
    _ <- IO(println(s"$operation: $result"))
  } yield result

}
