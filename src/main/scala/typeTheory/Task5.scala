package typeTheory

import scalaz._
import scalaz.Scalaz._
import scalaz.effect._
import scalaz.effect.IO._
import scala.io.Source.fromFile

object Task5 {
  val parser = TermParser()

  def main(args: Array[String]): Unit = run.unsafePerformIO()

  def run: IO[Unit] = readLn >>= solve

  def solve(path: String): IO[Unit] = {
    val system = fromFile(path).getLines()
      .map(parser.parseEq(_).get).toSeq
    unify(system) match {
      case Some(solution) => putStrLn(solution.mkString("\n"))
      case _ => putStrLn("Система неразрешима.")
    }
  }

  def unify(system: Seq[Equation]): Option[Seq[Equation]] = system match {
    case Nil => Seq().some
    case eqs if eqs.exists { eq => eq.lhs == eq.rhs } => unify(eqs.filter { eq => eq.lhs != eq.rhs })
    case eqs if eqs.exists { term => term.swap != term } => unify(eqs.map(_.swap))
    case eqs if eqs.exists { _.sameFunc } => unify(eqs.flatMap(_.unifyArgs))
    case eqs if eqs.exists { _.diffFunc } => None
    case eqs if eqs.exists {
      case Equation(v @ AVar(_), Func(_, args)) => args.exists { _.contains(v) }
      case _ => false
    } => None
    case eqs =>
      val elim = eqs.find {
        case eq @ Equation(v @ AVar(_), t) => v != t && eqs.filter(_ != eq).exists { _.contains(v) }
        case _ => false
      }
      elim match {
        case Some(e @ Equation(v @ AVar(_), t)) => unify(Equation(v, t) +: eqs.filter(_ != e).map(_.substitute(v, t)))
        case None => eqs.some
      }
  }
}
