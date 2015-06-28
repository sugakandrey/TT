package typeTheory

import scalaz._
import scalaz.Scalaz._
import scalaz.effect._
import scalaz.effect.IO._
import typeTheory._
import scala.io.Source.fromFile

object Task4 {
  val parser = LambdaParser()

  def main(args: Array[String]): Unit = run.unsafePerformIO()

  def run: IO[Unit] = readLn >>= solve

  def solve(path: String): IO[Unit] = fromFile(path).getLines()
    .map(parser.parse(_)).toList.traverse_ {
    case Some(term) => putStrLn(term.normalForm.shows)
    case _ => putStrLn("Parsing Error!")
  }
}
