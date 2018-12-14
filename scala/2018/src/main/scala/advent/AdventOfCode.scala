package advent

import cats.effect.Resource
import fs2.io
import cats.effect.IO
import scala.concurrent.ExecutionContext

abstract class AdventOfCode(val filename: String) {
  def mainIO(data: List[String]): IO[Unit]

  def main(args: Array[String]): Unit = {
    val exec = Resource(IO {
      val es = java.util.concurrent.Executors.newFixedThreadPool(3)
      val ec = ExecutionContext.fromExecutorService(es)
      (ec, IO(es.shutdown()))
    }).use { blockingEC =>
      implicit val CS = IO.contextShift(blockingEC)

      println(s"Loading file $filename")

      val inputData =
        fs2.io.readInputStream(
          IO(getClass.getClassLoader.getResourceAsStream(filename)),
          1024,
          scala.concurrent.ExecutionContext.Implicits.global,
          true)
          .through(fs2.text.utf8Decode)
          .through(fs2.text.lines)
//          .observe(_.flatMap(s => fs2.Stream.eval(IO(println(s"Line: $s")))))
          .filter(_.nonEmpty)

      for {
        input <- inputData.compile.toList
        _ <- mainIO(input)
      } yield ()
    }

    exec.unsafeRunSync()
  }

  def time[A](operation: String)(f: => A): IO[A] = for {
    start <- IO(System.currentTimeMillis())
    result = f
    end <- IO(System.currentTimeMillis())
    _ <- IO(println(s"$operation took ${end - start} milliseconds"))
    _ <- IO(println(s"$operation: $result"))
  } yield result

}
