import scala.util.parsing.combinator._
import collection.mutable

// abstract class doesn't need implementation.
abstract class RegexExpr

// case classes are used because the data they process are immutable.
// and they are useful for pattern matching.

// ., a, b : string literal
case class Literal(c: Char) extends RegexExpr

// a | b : Or
case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr

// ab -> Concat(a, b); abc -> Concat(a, Concat(b, c))
case class Concat(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr

// a*: repeat
case class Repeat(expr: RegexExpr) extends RegexExpr

// a+
case class Plus(expr: RegexExpr) extends RegexExpr

// In regular expressions, the order of binding strength is:
// 1. Character literals and parentheses
// 2. + and *
// 3. Concatenation
// 4. | : OR

// 4 different levels of binding strength,
// we need 4 different types of expressions.
// We named them lit, lowExpr(+, *), midExpr(concatenation) and highExpr(|)
object RegexParser extends RegexParsers {
  def charLit: Parser[RegexExpr] = ("""\w""".r| ".") ^^ {
    char => Literal(char.head)
  }
}
