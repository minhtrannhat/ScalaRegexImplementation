import org.xml.sax.Parser
// abstract class doesn't need implementation.
abstract class RegrexExpr

// case classes are used because the data they process are immutable.

// ., a, b : string literal
case class Literal(c: Char) extends RegrexExpr

// a | b : Or
case class Or(expr1: RegrexExpr, expr2: RegrexExpr) extends RegrexExpr

// ab -> Concat(a, b); abc -> Concat(a, Concat(b, c))
case class Concat(expr1: RegrexExpr, expr2: RegrexExpr) extends RegrexExpr

// a*: repeat
case class Repeat(expr: RegrexExpr) extends RegrexExpr

// a+
case class Plus(expr: RegrexExpr) extends RegrexExpr

// 4 different levels of binding strength,
// we need 4 different types of expressions.
// We named them lit, lowExpr(+, *), midExpr(concatenation) and highExpr(|)
object RegrexParser extends RegrexParser {
  def charLit: Parser[RegrexExpr] = ("""\w""".r| ".") ^^ {
    char => Literal(char.head)
  }
}
