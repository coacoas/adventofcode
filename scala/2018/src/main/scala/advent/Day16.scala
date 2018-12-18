package advent

import cats.data.State
import cats.implicits._
import cats.effect.IO
import fastparse._,NoWhitespace._
import java.text.ParseException

object Day16 extends AdventOfCode("day16.txt") {

  type Operation = (Int, Int, Int, Vector[Int]) => Vector[Int]

  // Addition:

  // addr (add register) stores into register C the result of adding
  // register A and register B.
  val addr: Operation = (a: Int, b: Int, c: Int, registers: Vector[Int]) =>
  registers.updated(c, registers(a) + registers(b))

  // addi (add immediate) stores into register C the result of adding
  // register A and value B.
  val addi: Operation = (a: Int, b: Int, c: Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a) + b)

  // Multiplication:

  // mulr (multiply register) stores into register C the result of
  // multiplying register A and register B.
  val mulr: Operation = (a: Int, b: Int, c: Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a) * registers(b))

  // muli (multiply immediate) stores into register C the result
  // of multiplying register A and value B.
  val muli: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a) * b)

  // Bitwise AND:

  // banr (bitwise AND register) stores into register C the result
  // of the bitwise AND of register A and register B.
  val banr: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a) & registers(b))

  // bani (bitwise AND immediate) stores into register C the
  // result of the bitwise AND of register A and value B.
  val bani: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a) & b)

  // Bitwise OR:

  // borr (bitwise OR register) stores into register C the result
  // of the bitwise OR of register A and register B.
  val borr: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a) | registers(b))

  //     bori (bitwise OR immediate) stores into register C the result
  //     of the bitwise OR of register A and value B.
  val bori: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a) | b)

  // Assignment:

  // setr (set register) copies the contents of register A into
  // register C. (Input B is ignored.)
  val setr: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, registers(a))

  // seti (set immediate) stores value A into register C. (Input B is ignored.)
  val seti: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, a)

  // Greater-than testing:

  // gtir (greater-than immediate/register) sets register C to 1 if
  // value A is greater than register B. Otherwise, register C is set
  // to 0.
  val gtir: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, if (a > registers(b)) 1 else 0)

  // gtri (greater-than register/immediate) sets register C to 1
  // register A is greater than value B. Otherwise, register C et
  // to 0.
  val gtri: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, if (registers(a) > b) 1 else 0)

  // gtrr (greater-than register/register) sets register C to 1 if
  // register A is greater than register B. Otherwise, register C is
  // set to 0.
  val gtrr: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, if (registers(a) > registers(b)) 1 else 0)

  // Equality testing:

  // eqir (equal immediate/register) sets register C to 1 if value A
  // is equal to register B. Otherwise, register C is set to 0.
  val eqir: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, if (a === registers(b)) 1 else 0)

  // eqri (equal register/immediate) sets register C to 1 if register
  // A is equal to value B. Otherwise, register C is set to 0.
  val eqri: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, if (registers(a) === b) 1 else 0)

  // eqrr (equal register/register) sets register C to 1 if register A
  // is equal to register B. Otherwise, register C is set to 0.
  val eqrr: Operation = (a: Int, b: Int, c:Int, registers: Vector[Int]) =>
    registers.updated(c, if (registers(a) === registers(b)) 1 else 0)

  val operations: List[Operation] = List(addi, addr, muli, mulr, bani, banr, bori, borr, seti, setr, gtir, gtri, gtrr, eqri, eqir, eqrr)

  case class Instruction(opcode: Int, a: Int, b: Int, c: Int)

  case class Test(
    init: Vector[Int],
    instruction: Instruction,
    after: Vector[Int])

  def num[_ : P]: P[Int] = P(CharsWhileIn("0-9").rep.!).map(_.mkString.toInt)
  def whitespace[_ : P]: P[Unit] = P(CharPred(_.isWhitespace))

  // Day 16
  def vector[_: P]: P[Vector[Int]] =
    P("[" ~ num ~ ", " ~ num ~ ", "  ~ num ~ ", "  ~ num ~ "]")
      .map { case (a, b, c, d) => Vector(a, b, c, d) }

  def state[_ : P](name: String): P[Vector[Int]] =
    P(name ~ ":" ~ whitespace.rep ~ vector)

  def newline[_: P]: P[Unit] = P("\n")
  def instruction[_: P]: P[Instruction] =
    P(num ~ whitespace ~ num ~ whitespace ~ num ~ whitespace ~ num)
      .map(Instruction.tupled)

  def test[_ : P]: P[Test] = P(
    state("Before") ~ newline ~ instruction ~ newline ~ state("After") ~ newline
  ).map(Test.tupled)

  def alltests[_ : P]: P[List[Test]] = P(test.rep).map(_.toList)

  def options(test: Test, ops: List[Operation]): List[Operation] =
    ops.map { op =>
      op(test.instruction.a, test.instruction.b, test.instruction.c, test.init) -> op
    }.collect { case (result, op) if result === test.after => op }

  lazy val threeOrMore: (List[Test]) => Int =
    _.map(options(_, operations))
      .filter(_.size >= 3)
      .size

  val parseTests: (List[String]) => List[Test] = (raw) => {
    val data = raw.mkString("\n") + "\n"

    val result = fastparse.parse(data, alltests(_))

    result.get.value
  }

  def determineOpcodes(training: List[Test]): Map[Int, Operation] = {
    def go(acc: Map[Int, Operation], ops: List[Operation], remainingTests: List[Test]): Map[Int, Operation] = {
      if (acc.size === 16) acc
      else {
        val newOps = remainingTests.flatMap { test =>
          options(test, ops) match {
            case op :: Nil => List(test.instruction.opcode -> op)
            case _ => List.empty
          }
        }.toMap

        val allOps = newOps ++ acc

        val determinedCodes = allOps.keySet
        val determinedOps = allOps.values.toSet
        val unknownOps = remainingTests.filterNot { test =>
          determinedCodes.contains(test.instruction.opcode)
        }
        val remainingOps = ops.filterNot(op => determinedOps.contains(op))

        go(allOps, remainingOps, unknownOps)
      }
    }

    go(Map.empty, operations, training)
  }

  def parseInstructions(input: List[String]): List[Instruction] =
    input.map(fastparse.parse(_, instruction(_)).get.value)

  def execute(input: List[Instruction], codes: Map[Int, Operation]): Vector[Int] = {
    input.foldLeft(Vector(0, 0, 0, 0)) { (acc, instruction) =>
      val op = codes(instruction.opcode)
      op(instruction.a, instruction.b, instruction.c, acc)
    }
  }

  override def mainIO(input: List[String]): IO[Unit] = {
    val training = input.takeWhile(_ != "1 2 3 0")
    val ops = input.drop(training.length)

    for {
      _ <- IO(assert(ops.head === "1 2 3 0"))
      tests = parseTests(training)
      _ <- IO(println(s"Found ${tests.size} tests"))
      _ <- time("Three or more")(threeOrMore(tests))
      opcodes = determineOpcodes(tests)
      _ <- time("Run code")(execute(parseInstructions(ops), opcodes))
    } yield ()
  }

}
