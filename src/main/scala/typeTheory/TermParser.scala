package typeTheory

import org.parboiled2._
import scala.util.{ Success, Failure }
import scalaz.syntax.std.option._

case class TermParser() {
  def parseEq(s: String): Option[Equation] = new ParboiledParser(s).line.run() match {
    case Success(term) => term.some
    case _ => None
  }

  def parseT(s: String): Option[Algebraic] = new ParboiledParser(s).simple.run() match {
    case Success(term) => term.some
    case _ => None
  }

  private class ParboiledParser(val input: ParserInput) extends Parser {
    type RAlg = Rule1[Algebraic]

    def line: Rule1[Equation] = rule { equation ~ EOI }
    def simple: RAlg = rule { term ~ EOI }

    implicit private def wspStr(s: String): Rule0 = rule { zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ') }

    private def equation: Rule1[Equation] = rule { term ~ "=" ~ term ~> Equation }

    private def term: RAlg = rule {
      capture(func) ~ "(" ~ zeroOrMore(term).separatedBy(",") ~ ")" ~> Func |
      capture(variable) ~> AVar
    }

    private def func: Rule0 = rule { anyOf("abcdefgh") ~ zeroOrMore(CharPredicate.AlphaNum) ~ zeroOrMore("'") }

    private def variable: Rule0 = rule { anyOf("ijklmnopqrstuvwxyz") ~ zeroOrMore(CharPredicate.AlphaNum) ~ zeroOrMore("'") }
  }

}

