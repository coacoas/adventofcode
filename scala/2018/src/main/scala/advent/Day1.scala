package advent

object Day1 {
  def shift(shifts: List[String]): Int = {
    shifts.map(_.toInt).sum
  }

  def loop(shifts: List[String]): Int = {
    Stream.continually(shifts).flatten
      .map(_.toInt)
      .scanLeft(0 -> Set.empty[Int]) { (acc, current) =>
        val (freq, past) = acc
        (freq + current, past + freq)
      }
      .dropWhile { case (freq, set) => !set.contains(freq) }
      .head
      ._1
  }

  def main(args: Array[String]): Unit = {
    val input = io.Source.fromURL(this.getClass.getClassLoader.getResource("day1.txt"))
      .getLines
      .toList

    println(s"Frequency shift: ${shift(input)}")
    println(s"First duplicate: ${loop(input)}")
  }
}
