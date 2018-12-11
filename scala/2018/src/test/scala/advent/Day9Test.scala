package advent

import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers, Inside }


class Day9Test extends FlatSpec with Matchers with PropertyChecks with Inside {
  import Day9._

  "A circular zipper" should "return the same thing for next or prev in a single element list" in {
    val z = CircularZipper(List.empty, 3, List.empty)

    z.next should equal(z)
    z.prev should equal(z)
  }

  it should "circle around for previous" in {
    val z = CircularZipper(List(1), 2, List(3))

    z.prev should equal(CircularZipper(List.empty, 1, List(2, 3)))
    z.prev.prev should equal(CircularZipper(List(2), 3, List(1)))
  }

  val combos = Table(
    ("players", "lastScore", "winningScore"),
    (10, 1618L, 8317),
    (13, 7999L, 146373),
    (17, 1104L, 2764),
    (21, 6111L, 54718),
    (30, 5807L, 37305))

  "Normal move" should "do what it's supposed to do" in {
    val initial = GameState(
      CircularZipper(List.empty, 0, List.empty),
      CircularZipper(List.empty, 0, List.fill(8)(0)),
      1
    )

    Day9.move(initial) should equal(GameState(
      CircularZipper(List(0), 1, List.empty),
      CircularZipper(List(0), 0, List.fill(7)(0)),
      2
    ))
  }

  it should "do the next step" in {
    val initial = GameState(
      CircularZipper(List(0), 1, List.empty),
      CircularZipper(List(0), 0, List.fill(7)(0)),
      2
    )

    Day9.move(initial) should equal(GameState(
      CircularZipper(List(0, 1), 2, List.empty),
      CircularZipper(List(0, 0), 0, List.fill(6)(0)),
      3))
  }

  it should "do the third step" in {
    val initial = GameState(
      CircularZipper(List(0), 2, List(1)),
      CircularZipper(List(0, 0), 0, List.fill(6)(0)),
      3)

    Day9.move(initial) should equal(GameState(
      CircularZipper(List(1, 2, 0), 3, List.empty),
      CircularZipper(List(0, 0, 0), 0, List.fill(5)(0)),
      4))
  }

  it should "do the 23rd step" in {
    val initial = GameState(
      CircularZipper(List[Long](0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5).reverse, 22, List(11, 1, 12, 6, 13, 3, 14, 7, 15)),
      CircularZipper(List.fill(4)(0), 0, List.fill(4)(0)),
      23)

    Day9.move(initial) should equal(GameState(
      CircularZipper(List(18, 4, 17, 8, 16, 0), 19, List(2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15)),
      CircularZipper(32L :: List.fill[Long](4)(0), 0, List.fill(3)(0)),
      24))
  }

  it should "do the 24th step" in {
    val initial = GameState(
      CircularZipper(List(18, 4, 17, 8, 16, 0), 19, List(2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15)),
      CircularZipper(List(32, 0, 0, 0, 0), 0, List(0, 0, 0)),
      24)

    Day9.move(initial) should equal(GameState(
      CircularZipper(List(2, 19, 18, 4, 17, 8, 16, 0), 24, List(20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15)),
      CircularZipper(List(0, 32, 0, 0, 0, 0), 0, List(0, 0)),
      25))
  }

  "Sample data" should "provide the correct answer" in forAll(
    combos
  ) {(players, lastScore, winningScore) =>
    println(s"Running game for $players players until $lastScore")
    val game = Day9.game(players, lastScore)

    println(s"Completed game")

    game.max should equal(winningScore)
  }

}
