package advent

import cats.effect.IO
import fastparse._,NoWhitespace._
import java.text.ParseException

object Day8 extends AdventOfCode("day8.txt") {
  type Metadata = Int
  case class Tree(children: List[Tree], metadata: List[Metadata]) {
    def sum: Metadata = metadata.sum + children.foldLeft(0)(_ + _.sum)

    def value: Int =
      if (children.isEmpty) metadata.sum
      else {
        val values: Vector[Int] = children.map(_.value).toVector
        metadata.map { i =>
          if (i > values.length) 0 else values(i - 1)
        }.sum
      }
  }

  def num[_ : P]: P[Int] = P(CharsWhileIn("0-9").!.map(_.toInt) ~ " ".rep)
  def whitespace[_ : P]: P[Unit] = P(CharPred(_.isWhitespace).rep)
  def node[_ : P]: P[Tree] = P(for {
    childCount <- num
    metadataCount <- num
    children <- node.rep(exactly = childCount)
    metadata <- num.rep(exactly = metadataCount)
  } yield Tree(children.toList, metadata.toList) )

  def parse(data: String): IO[Tree] = fastparse.parse(data, node(_)) match {
    case Parsed.Success(t, _) => IO.pure(t)
    case Parsed.Failure(_, i, reason) =>
      IO.raiseError(new ParseException(s"Parsing failed after $i characters: $reason", i))
  }

  def mainIO(input: List[String]): IO[Unit] = for {
    tree <- parse(input.head)
  } yield {
    println(s"Tree: ${tree.sum}")
    println(s"Root: ${tree.value}")
  }
}
