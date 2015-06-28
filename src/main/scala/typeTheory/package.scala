package typeTheory

import scalaz.{ \/, -\/, \/- }
import scalaz.syntax.either._
import scalaz.Show

package object typeTheory {
  implicit object LambdaErrShow extends Show[Error \/ Lambda]{
    val printer = Printer()
    override def shows(l: (Error \/ Lambda)): String = l match {
      case \/-(lmbd) => printer(lmbd)
      case -\/(msg) => msg.toString
    }
  }
}
