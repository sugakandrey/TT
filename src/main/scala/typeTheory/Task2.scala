package typeTheory

import scalaz._
import scalaz.Scalaz._
import scalaz.effect._
import scalaz.effect.IO._
import scala.io.Source.fromFile
import scalaz.Show

object Task2 {
  val parser = LambdaParser()

  def main(args: Array[String]): Unit = run.unsafePerformIO

  def run: IO[Unit] = readLn >>= solve

  def solve(path: String): IO[Unit] = fromFile(path).getLines()
    .map(parser.parse(_)).toList.traverse_ {
    case Some(term) => putStrLn(term.freeVars().map(_.toString).mkString(", "))
    case _ => putStrLn("Parsing Error!")
  }
}

