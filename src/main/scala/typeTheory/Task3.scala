package typeTheory

import scalaz._
import scalaz.Scalaz._
import scalaz.effect._
import scalaz.effect.IO._
import scala.io.Source.fromFile
import typeTheory._

object Task3 {
  val parser = LambdaParser()

  def main(args: Array[String]): Unit = run.unsafePerformIO

  def run: IO[Unit] = readLn >>= solve

  def solve(path: String): IO[Unit] = fromFile(path).getLines()
    .map(parser.substitute(_)).toList.traverse_ {
    case Some(t) => putStrLn(t.shows)
    case _ => putStrLn("Parsing Error!")
  }
}
