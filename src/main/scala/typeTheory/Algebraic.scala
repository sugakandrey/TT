package typeTheory

sealed trait Algebraic {
  def contains(what: AVar): Boolean = this match {
    case v @ AVar(_) => v == what
    case Func(_, args) => args.exists { _.contains(what) }
    case Equation(lhs, rhs) => lhs.contains(what) || rhs.contains(what)
  }

  def subs(v: AVar, t: Algebraic): Algebraic = this match {
    case AVar(_) if this == v => t
    case Func(name, args) => Func(name, args.map(_.subs(v, t)))
    case _ => this
  }

  override def toString: String = this match {
    case AVar(name) => name
    case Func(name, args) => s"${name}(${args.mkString(",")})"
    case Equation(lhs, rhs) => s"$lhs = $rhs"
  }
}

final case class Equation(lhs: Algebraic, rhs: Algebraic) extends Algebraic {
  def swap: Equation = this match {
    case Equation(f @ Func(_, _), v @ AVar(_)) => Equation(v, f)
    case _ => this
  }

  def sameFunc: Boolean = this match {
    case Equation(f @ Func(_, _), g @ Func(_, _))
        if f.name == g.name && f.args.length == g.args.length => true
    case _ => false
  }

  def diffFunc: Boolean = this match {
    case Equation(f @ Func(_, _), g @ Func(_, _))
        if f.name != g.name || f.args.length != g.args.length => true
    case _ => false
  }

  def unifyArgs: Seq[Equation] = this match {
    case Equation(f @ Func(_, _), g @ Func(_, _)) if sameFunc => (f.args, g.args).zipped.map { Equation }
    case _ => Seq(this)
  }

  def substitute(v: AVar, t: Algebraic): Equation = Equation(lhs.subs(v, t), rhs.subs(v, t))

}
final case class Func(name: String, args: Seq[Algebraic]) extends Algebraic
final case class AVar(name: String) extends Algebraic
