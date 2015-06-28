package typeTheory
import scalaz.Enum
import scalaz.Scalaz._
import scalaz.\/
import scalaz.Show
import scala.collection.immutable._
import scala.collection.{ mutable => m }

/**
 * @author sugak andrey
 */

sealed trait Lambda {
  import Lambda._

  def freeVars(scope: Set[Var] = Set()): Set[Var] = this match {
    case Abstraction(v, b) => b.freeVars(scope + v)
    case v @ Var(_) if !scope.contains(v) => Set(v)
    case Application(x, y) => x.freeVars(scope) ++ y.freeVars(scope)
    case _ => Set()
  }

  def substitute(v: Var, e: Lambda, scope: Set[Var] = Set()): (Error \/ Lambda) = this match {
    case Var(_) if v == this => e.right
    case v @ Var(_) => v.right
    case Application(x, y) => for (
      lhs <- x.substitute(v, e, scope);
      rhs <- y.substitute(v, e, scope)
    ) yield Application(lhs, rhs)
    case Abstraction(bind, _) if e.freeVars().contains(bind) => NotFreeForSubstitution(e, this, v).left
    case Abstraction(bind, body) => body.substitute(v, e, scope) map { Abstraction(bind, _)}
  }

  def substituteRenaming(v: Var, e: Lambda, scope: Set[Var] = Set()): Lambda = this match {
    case Abstraction(bind, body) if e.freeVars().contains(bind) =>
      val free = getFreeName(body.freeVars() ++ e.freeVars()); Abstraction(free, body.substitute(v, e) | null)
    case _ => substitute(v, e) | null
  }

  case class Cached[T, R](fn: T => R) extends (T => R) {
    private val cache = m.HashMap.empty[T, R]
    def apply(x: T) = cache.getOrElseUpdate(x, fn(x))
  }

  def headNormalForm: Lambda = {

    lazy val headNormalized: Cached[Lambda, Lambda] = Cached {
      case v @ Var(_) => v
      case Abstraction(bind, body) => Abstraction(bind, headNormalized(body))
      case Application(lhs, rhs) =>
        val hnf = headNormalized(lhs); hnf match {
          case lhsN @ Abstraction(bind, body) => body.substitute(bind, rhs) | null
          case _ => Application(hnf, rhs)
        }
    }

    headNormalized(this)
  }

  def normalForm: Lambda = {

    lazy val normalize: Cached[Lambda, Lambda] = Cached {
      case v @ Var(_) => v
      case Abstraction(bind, body) => Abstraction(bind, normalize(body))
      case t @ Application(lhs, rhs) =>
        val hnf = lhs.headNormalForm; hnf match {
          case Abstraction(bind, body) => body.substitute(bind, rhs).map(_.normalForm) | null
          case _ => Application(hnf, normalize(rhs))
        }
    }

    normalize(this)
  }
}

object Lambda {
  def getFreeName(used: Set[Var], range: Seq[String] = ('a' to 'z').map(_.toString)): Var = {
    range.find(c => !used.contains(Var(c.toString))) match {
      case Some(c) => Var(c.toString)
      case _ => getFreeName(used, range.map(c => c + "'"))
    }
  }

  implicit object LambdaShow extends Show[Lambda] {
    import typeTheory._

    override def shows(lmbd: Lambda): String = lmbd.right[Error].shows
  }
}

case class Abstraction(binding: Var, body: Lambda) extends Lambda
case class Var(name: String) extends Lambda
case class Application(lhs: Lambda, rhs: Lambda) extends Lambda

sealed trait Error
case class NotFreeForSubstitution(that: Lambda, expr: Lambda, v: Lambda) extends Error {
  override def toString: String = s"Выражение ${that.shows} не свободно для подстановки в ${expr.shows} вместо ${v.shows}"
}
case class NotTypeable(that: Lambda) extends Error {
  override def toString: String = s"Выражение $that не имеет типа."
}

sealed trait Type
final case class Atomic(name: String) extends Type {
  override def toString: String = name
}
final case class ArrowT(from: Type, to: Type) extends Type {
  override def toString: String = from match {
    case Atomic(name) => s"$from -> $to"
    case ArrowT(_, _) => s"($from) -> $to"
  }
}
