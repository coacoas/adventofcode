package advent

import cats.effect.IO
import fastparse._,NoWhitespace._
import java.text.ParseException

object Day8 extends AdventOfCode("day8.txt") {
  type Metadata = Int
  case class Tree(children: List[Tree], metadata: List[Metadata]) {
    def sum: Metadata = metadata.sum + children.foldLeft(0)(_ + _.sum)

    def value: Int =
      if (children.isEmpty) {
        val sum = metadata.sum
        println(s"Leaf node value is $sum")
        sum
      } else {
        val values: Vector[Int] = children.map(_.value).toVector
        val valueSize = values.length
        val sum = metadata.map { i =>
          if (i > valueSize) 0
          else values(i - 1)
        }.sum

        println(s"Tree node value is $sum from $values and $metadata")

        sum
      }
  }

  def num[_ : P]: P[Int] = P(CharsWhileIn("0-9").!.map(_.toInt) ~ " ".rep)
  def whitespace[_ : P]: P[Unit] = P(CharPred(_.isWhitespace).rep)
  def node[_ : P]: P[Tree] = P(for {
    childCount <- num
//    _ = println(s"Got child count $childCount")
    metadataCount <- num
//    _ = println(s"Got metadata count $metadataCount")
    children <- node.rep(exactly = childCount)
    metadata <- num.rep(exactly = metadataCount)
  } yield Tree(children.toList, metadata.toList) )


  def parse(data: String): Tree = fastparse.parse(data, node(_)) match {
    case Parsed.Success(t, _) => t
    case f@Parsed.Failure(label, i, _) =>
      println(s"Failure! $f")
      throw new ParseException(s"Parsing failed after $i characters: $label", i)
  }


  def mainIO(input: List[String]): IO[Unit] = {
    val tree = parse(input.head)

    IO(println(s"Tree: ${tree.sum}"))
    IO(println(s"Root: ${tree.value}"))
  }
}
