package typeTheory

import scalaz._
import scalaz.Scalaz._
import scalaz.effect._
import scalaz.effect.IO._
import scala.io.Source.fromFile

object Task6 {
  val parser = LambdaParser()

  def main(args: Array[String]): Unit = run.unsafePerformIO()

  def run: IO[Unit] = readLn >>= solve


  def isSolution(term: Equation): Boolean = term match {
    case Equation(AVar("x0"), _) => true
    case Equation(_, AVar("x0")) => true
    case _ => false
  }

  def solve(path: String): IO[Unit] = fromFile(path).getLines()
    .map(parser.parse(_)).toList.traverse_ {
    case Some(term) =>
      val system = buildEqs(term);
      unify(system) match {
        case Some(seq) =>
          val typeTerm = seq.find { isSolution }.get match {
            case eq @ Equation(AVar("x0"), _) => eq.rhs
            case eq @ Equation(_, _) => eq.lhs
          }
          putStrLn(alg2type(typeTerm).toString())
        case _ => putStrLn(s"Выражение ${term.shows} не имеет типа.")
      }

    case None => putStrLn("Parsing Error!")
  }

  implicit def type2alg(t: Type): Algebraic = t match {
    case Atomic(name) => AVar(name)
    case ArrowT(t1, t2) => Func("->", Seq(t1, t2))
  }

  @unchecked
  implicit def alg2type(term: Algebraic): Type = term match {
    case AVar(name) => Atomic(name)
    case Func("->", t1 :: t2 :: rest) => ArrowT(t1, t2)
  }


  def buildEqs(e: Lambda): Seq[Equation] = {
    case class Annotation(typeEqs: Seq[Equation], g: Seq[(Map[Var, Atomic], Lambda, Type)])

    var cur = 0
    def nextType(): Atomic = {
      val res = Atomic(s"x$cur")
      cur += 1
      res
    }

    def build(annotation: Annotation): Seq[Equation] = annotation.g match {
      case Nil => annotation.typeEqs
      case (map, term, t) :: rest => term match {
        case v @ Var(_) => build(Annotation(
                                  Equation(t, map.get(v).get) +: annotation.typeEqs,
                                  rest))
        case Application(f, g) =>
          val newT = nextType()
          build(Annotation(
                  annotation.typeEqs,
                  (map, f, ArrowT(newT, t)) +: (map, g, newT) +: rest))
        case abs @ Abstraction(bind, body) =>
          val (t1, t2) = (nextType(), nextType())
          build(Annotation(
                  Equation(t, ArrowT(t1, t2)) +: annotation.typeEqs,
                  (map + (bind -> t1), body, t2) +: rest))
      }
    }
    val mapped = e.freeVars().map(_ -> nextType())
    build(Annotation(
            Seq(),
            Seq((mapped.toMap, e, nextType()))))
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
        case _ => eqs.some
      }
  }
}
