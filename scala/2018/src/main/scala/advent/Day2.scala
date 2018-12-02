package advent

object Day2 {
 
  def repeated(ids: String, count: Int): Int = {
    ids.groupBy(identity).collect {
      case (k, v) if v.size == count =>
        k
    }.size
  }

  def addTuples(t1: (Int, Int), t2: (Int, Int)): (Int, Int) =
    (t1._1 + t2._1, t1._2 + t2._2)

  def checksum(boxIds: List[String]): Int = {
    val (twos, threes) = boxIds
      .map { ids => (repeated(ids, 2), repeated(ids, 3)) }
      .map { case (two, three) =>
        (if (two > 0) 1 else 0, if (three > 0) 1 else 0)
      }
      .foldLeft((0, 0))(addTuples)

    twos * threes
  }

  def commonLetters(s1: String, s2: String): String =
    s1.zip(s2).flatMap { case (c1, c2) =>
      if (c1 == c2) s"$c1" else s""
    }.mkString

  def diff(boxIds: List[String]): String = {
    boxIds.flatMap { s1 =>
      boxIds.flatMap { s2 =>
        val common = commonLetters(s1, s2)
        if (common.size == s1.size - 1) List(common)
        else List.empty
      }
    }.head
  }

  def main(args: Array[String]): Unit = {
    val input = io.Source.fromURL(this.getClass.getClassLoader.getResource("day2.txt"))
      .getLines
      .toList

    println(s"Checksum: ${checksum(input)}")
    println(s"Diff: ${diff(input)}")
  }
}
