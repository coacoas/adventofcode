package advent

import cats.effect.IO

object Day14 extends AdventOfCode("day14.txt") {
  case class Recipes(recipes: Vector[Int], elf1: Int, elf2: Int) {
    val zero = '0'.toInt
    def tick: Recipes = {
      val sum = recipes(elf1) + recipes(elf2)
      val digits = sum.toString.map(_.toInt - zero).toVector

      val elf1Diff = 1 + recipes(elf1)
      val elf2Diff = 1 + recipes(elf2)
      val newRecipes = recipes ++ digits
      def shift(i: Int, diff: Int) = (i + diff) % newRecipes.length

      val updated = Recipes(newRecipes, shift(elf1, elf1Diff), shift(elf2, elf2Diff))
//      println(s"Moved: Updated = $updated")
      updated
    }
  }

  def move(state: Recipes)(stop: Vector[Int] => Boolean): Recipes = {
    if (stop(state.recipes)) state
    else move(state.tick)(stop)
  }

  override def mainIO(input: List[String]): IO[Unit] = for {
    recipeNumber <- IO.pure(input.head.toInt)
    init = Recipes(Vector(3, 7), 0, 1)
    _ <- time("Ten past input") {
      move(init)(_.size >= (recipeNumber + 10)).recipes.drop(recipeNumber).take(10).mkString
    }
    _ <- time("Find structure") {
      val target = recipeNumber.toString
      move(init) { v =>
        v.takeRight(target.length + 1).mkString.contains(target)
      }.recipes.mkString.indexOf(target)
    }
  } yield ()

}
