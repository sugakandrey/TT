package typeTheory
/**
 * @author sugak andrey
 */
case class Printer() {

  def apply(expr: Lambda): String = expr match {
    case Abstraction(v, body) => p"λ$v.$body"
    case Application(lhs, rhs) => p"$lhs $rhs"
    case Var(name) => s"$name"
  }

  implicit class LambdaPrinting(val sc: StringContext) {
    def p(args: Lambda*) = sc.s(args map parens: _*)
  }

  def parens(expr: Lambda) = expr match {
    case Var(name) => name
    case _ => "(" + apply(expr) + ")"
  }
}

