package advent

import cats.effect.IO
import cats.implicits._

object Day11 {

  def power(x: Int, y: Int, serial: Int) = {
    // Find the fuel cell's rack ID, which is its X coordinate plus 10.
    val rack = x + 10
    // Begin with a power level of the rack ID times the Y coordinate.
    val powerInit = (((rack * y)
      + serial)
      * rack)

    val powerLevel = (powerInit / 100) % 10

    powerLevel - 5
  }

  def grid(serial: Int): Vector[Vector[Int]] =
    (1 to 300).map { x =>
      (1 to 300).map { y =>
        power(x, y, serial)
      }.toVector
    }.toVector

  def sums(rawGrid: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    val lineSums = (0 until rawGrid.length).map { x =>
      val column = rawGrid(x)
      column.tail.scanLeft(column.head) { _ + _ }
    }.toVector
    val flipped = lineSums.transpose
    val flippedSums = (0 until flipped.length).toVector.map { x =>
      val row = flipped(x)
      row.tail.scanLeft(row.head) { _ + _ }
    }
    flippedSums.transpose
  }

  def powerSum(partialSums: Vector[Vector[Int]], x: Int, y: Int, size: Int): Int = {
    (partialSums(x + size)(y + size)
      - partialSums(x)(y + size)
      - partialSums(x + size)(y)
      + partialSums(x)(y))
  }

  def powerSums(partialSums: Vector[Vector[Int]], size: Int): Vector[(Int, Int, Int)] =
    for {
      x <- (0 until (300 - size)).toVector
      y <- (0 until (300 - size)).toVector
    } yield (x + 1, y + 1, powerSum(partialSums, x, y, size))

  def show(g: Vector[Vector[Int]]): String =
    g.map(_.mkString(",")).mkString("\n")

  def main(args: Array[String]): Unit = {
    val serial = 8868

    val g = grid(serial)
    val sumTable = sums(g)

    val tbt = powerSums(sumTable, 3)

    val (x, y, power) = tbt
      .maxBy { case(_, _, power) =>  power}

    val (x_, y_, size_, power_) = (1 until 300).map { size =>
      println(s"Running size $size")
      val (x, y, power) = powerSums(sumTable, size)
        .maxBy { case(_, _, power) =>  power}
      (x, y, size, power)
    }.maxBy { case (_, _, _, power) => power }

    val maxPower = tbt.collect { case(_, _, power) =>  power}.max

    println(s"Max power $maxPower")
    println(s"($x, $y) has power $power")
    println(s"($x_, $y_, $size_) has power $power_")
  }
}
