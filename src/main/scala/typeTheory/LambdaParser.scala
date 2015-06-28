package typeTheory

/**
 * @author sugak andrey
 */

import org.parboiled2._
import scala.util.{ Success, Failure }
import scalaz.syntax.std.option._
import scalaz.{ \/ }

case class LambdaParser() {

  def parse(s: String): Option[Lambda] = new ParboiledParser(s).line.run() match {
    case Success(term) => term.some
    case _ => None
  }

  def substitute(s: String): Option[Error \/ Lambda] = new ParboiledParser(s).substitution.run() match {
    case Success(term) => term.some
    case _ => None
  }

  private class ParboiledParser(val input: ParserInput) extends Parser {
    def line: Rule1[Lambda] = rule { expr ~ EOI }

    def substitution: Rule1[\/[Error, Lambda]] = rule {
      expr ~ "[" ~ variable ~ ":=" ~ expr ~ "]" ~> ((e: Lambda, v: Var, subs: Lambda) => e.substitute(v, subs))
    }

    private def wspStr(s: String): Rule0 = rule { zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ') }

    private def expr: Rule1[Lambda] = rule { application | atom }

    private def application: Rule1[Lambda] = rule { atom ~ oneOrMore(" " ~ atom ~> Application) }

    private def atom: Rule1[Lambda] = rule { variable | abstraction | brackets }

    private def variable: Rule1[Var] = rule {
      capture(CharPredicate.LowerAlpha) ~ zeroOrMore(capture(CharPredicate.AlphaNum)) ~
        zeroOrMore(capture("'")) ~> ((s: String, seq: Seq[_], seq2: Seq[_]) => Var(s + seq.mkString + seq2.mkString))
    }

    private def brackets: Rule1[Lambda] = rule { "(" ~ expr ~ ")" }

    private def abstraction: Rule1[Lambda] = rule { "\\" ~ variable ~ wspStr(".") ~ expr ~> Abstraction }
  }
}
