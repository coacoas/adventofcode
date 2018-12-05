package advent

object Day4 {
  type GuardId = Int

  val GuardEntry = """\[.*\] Guard \#(\d+) begins shift$""".r
  val Sleep = """\[.*(\d\d)\] falls asleep$""".r
  val Wake = """\[.*(\d\d)\] wakes up""".r

  GuardEntry.unapplySeq("[1518-04-01 00:00] Guard #3167 begins shift")

  def sleepLog(log: List[String]): Map[GuardId, Vector[Int]] = {
    def go(
      remaining: List[String],
      sleep: Int,
      events: Map[GuardId, Vector[Int]],
      id: GuardId
    ): Map[GuardId, Vector[Int]] =
      if (remaining.isEmpty) events
      else {
        val entry = remaining.head
        entry match {
          case GuardEntry(id) => go(remaining.tail, -1, events, id.toInt)
          case Sleep(minute) => go(remaining.tail, minute.toInt, events, id)
          case Wake(minute) =>
            if (sleep == -1) throw new RuntimeException("Something bad")
            val minutes = (sleep until (minute.toInt)).toVector
            val previous = events.get(id).getOrElse(Vector.empty)
            go(remaining.tail, -1, events + (id -> (previous ++ minutes)), id)
        }
      }

    go(log, -1, Map.empty, 0)
  }

  def sleepingGuard(log: Map[GuardId, Vector[Int]]): Int = 
    log.mostCommon
  def sleepingMinute(log: Vector[Int]): Int =
    log.groupBy(identity).mostCommon

  def findGuardAndMinute(log: Map[GuardId, Vector[Int]]): Int = {
    val (guard, minute) = log.toList.flatMap { case (id, minutes) =>
      minutes.map(id -> _)
    }.groupBy(identity).mostCommon

    guard * minute
  }

  def main(args: Array[String]): Unit = {
    val input = io.Source.fromURL(this.getClass.getClassLoader.getResource("day4.txt"))
    val data = input.getLines.toList.sorted
    input.close()

    val minutes = sleepLog(data)
    val guard = sleepingGuard(minutes)
    val minute = sleepingMinute(minutes(guard))
    val guardMinute = findGuardAndMinute(minutes)

    println(s"Result 1: ${guard * minute}")
    println(s"Result 2: $guardMinute")
  }

  implicit class RichMap[K, V](val m: Map[K, Seq[V]]) {
    def mostCommon: K =
      m.mapValues(_.length).toList.sortBy(_._2).reverse.head._1
  }

}
