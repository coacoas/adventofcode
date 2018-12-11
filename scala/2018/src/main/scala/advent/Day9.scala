package advent

import cats.effect.IO

object Day9 {

  case class CircularZipper(
    p: List[Long],
    current: Long,
    n: List[Long]
  ) {
    def remove: (Long, CircularZipper) = (current, CircularZipper(p, n.head, n.tail))

    def insert(value: Long) = CircularZipper(current :: p, value, n)

    def modify(f: Long => Long) = CircularZipper(p, f(current), n)

    def next =
      if (n.isEmpty) {
        if (p.isEmpty) this else {
          val reset = p.reverse
          CircularZipper(List(current), reset.head, reset.tail)
        }
      } else {
        CircularZipper(current :: p, n.head, n.tail)
      }

    def prev: CircularZipper = if (p.isEmpty) {
      if (n.isEmpty) this else {
        val reset = n.reverse
        CircularZipper(reset.tail, reset.head, List(current))
      }
    } else {
      CircularZipper(p.tail, p.head, current :: n)
    }

    def toList =
      p.reverse ++ List(current) ++ n
  }

  case class GameState(
    circle: CircularZipper,
    scores: CircularZipper,
    marble: Long)

  def move(state: GameState): GameState = {
    def twentyThreeMove(state: GameState): GameState = {
      val (removeMarble, newCircle) = state.circle.prev.prev.prev.prev.prev.prev.prev.remove

      val points = state.marble + removeMarble

      val result = GameState(
        newCircle,
        state.scores.modify(_ + points).next,
        state.marble + 1)
      result
    }

    def normalMove(state: GameState): GameState = {
      val newCircle = state.circle.next.insert(state.marble)

      val result = GameState(
        newCircle,
        state.scores.next,
        state.marble + 1)
      result
    }

    // println(s"Current marble: ${state.circle(state.position)}")
    if (state.marble % 23 == 0) twentyThreeMove(state) else normalMove(state)
  }

  def game(players: Int, lastMarble: Long): Vector[Long] = {
    val initialState = GameState(
      CircularZipper(List.empty, 0, List.empty),
      CircularZipper(List.empty, 0, List.fill(players - 1)(0)),
      1)

    def gameStream: fs2.Stream[IO, GameState] =
      fs2.Stream
        .iterate(initialState)(move)
        .covary[IO]

    val endgame: GameState = gameStream
      .drop(lastMarble.toLong)
      .take(1)
      .compile
      .toList
      .unsafeRunSync
      .head

    val winningScore = endgame.scores.toList.max

    // println(s"Game end. Winner with score ${winningScore}. Final state $endgame")

    endgame.scores.toList.toVector
  }

  def mainIO: IO[Unit] = IO {
    val players = 446
    val lastMarbleScore = 71522

    val scores = game(players, lastMarbleScore)
    println(s"Winning score: ${scores.max}")

    val part2 = game(players, lastMarbleScore * 100)
    println(s"Winning score: ${part2.max}")
  }

  def main(args: Array[String]): Unit = mainIO.unsafeRunSync
}
