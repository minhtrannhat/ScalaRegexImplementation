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
case class Plug(expr: RegrexExpr) extends RegrexExpr


