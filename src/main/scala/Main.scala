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

// a*: match 0 or more of the previous pattern
case class Repeat(expr: RegexExpr) extends RegexExpr

// a+: match one or more of the previous pattern
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
  // The ".r" indicates that the part inside 6 quotation marks is a regex expression
  // Find something that matched a word OR a period
  def charLit: Parser[RegexExpr] = ("""\w""".r| ".") ^^ {
    // The first character of the first word of a string is parsed
    string => Literal(string.head)
  }

  def parenExpr: Parser[RegexExpr] = "(" ~> highExpr <~ ")"

  def lit: Parser[RegexExpr] = charLit | parenExpr

  def repeat: Parser[RegexExpr] = lit <~ "*" ^^ {
    case l => Repeat(l) 
  }

  def plus: Parser[RegexExpr] = lit <~ "+" ^^ {
    case p => Plus(p) 
  }

  def lowExpr: Parser[RegexExpr] = repeat | plus | lit

  def concat: Parser[RegexExpr] = rep(lowExpr) ^^ {
    case list => listToConcat(list)
  }

  def midExpr: Parser[RegexExpr] = concat | lowExpr

  def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr ^^ {
    case l ~ "|" ~ r => Or(l, r) 
  }

  def highExpr: Parser[RegexExpr] = or | midExpr

  def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
    case head :: Nil => head
    case head:: rest => Concat(head, listToConcat(rest))
  }

  def apply(input: String): Option[RegexExpr] = {
    parseAll(highExpr, input) match {
      case Success(result, _) => Some(result)
      case failure : NoSuccess => None
    }
  }
}
