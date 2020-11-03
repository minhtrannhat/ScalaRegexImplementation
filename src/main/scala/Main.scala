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

  // listToConcat() take a list of RegexExpr as its parameter
  // and return a list
  def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
    case head:: Nil => head
    case head:: rest => Concat(head, listToConcat(rest))
  }

  def apply(input: String): Option[RegexExpr] = {
    parseAll(highExpr, input) match {
      case Success(result, _) => Some(result)
      case failure : NoSuccess => None
    }
  }
}

abstract class State

class Consume(val c: Char, val out: State) extends State
class Split(val out1: State, val out2:State) extends State

// case class were used because they make all Match objects equal
case class Match() extends State

class Placeholder(var pointingTo: State) extends State

object NFA {
  def regexToNFA(regex: RegexExpr): State =
    regexToNFA(regex, Match())

  private def regexToNFA(regex: RegexExpr,
    andThen: State) : State = {
      regex match {
        case Literal(c) => new Consume(c, andThen)

        case Concat(first, second) => {
          // Convert the first to an NFA. The "out" of that is the result of converting second to an NFA
          regexToNFA(first, regexToNFA(second, andThen))
        }

        case Or(l, r) => new Split(
          regexToNFA(l, andThen),
          regexToNFA(r, andThen)
        )

        case Repeat(r) => 
          val placeholder = new Placeholder(null)
          val split = new Split(
            // one path goes to the placeholder
            // the other path goes back to the r
            regexToNFA(r, placeholder),
            andThen
          )
          placeholder.pointingTo = split
          placeholder
        
        case Plus(r) => 
          regexToNFA(Concat(r, Repeat(r)), andThen)
      }
  }
}

object NFAEvaluator {
    def evaluate(nfa: State, input: String): Boolean = 
        evaluate(Set(nfa), input)

    def evaluate(nfas: Set[State], input: String): Boolean = {
        input match {
            case "" => 
                evaluateStates(nfas, None).exists(_ == Match())
            case string => 
                evaluate(
                    evaluateStates(nfas, input.headOption), 
                    string.tail
                )
        }
    }

    def evaluateStates(nfas: Set[State], 
                       input: Option[Char]): Set[State] = {
        val visitedStates = mutable.Set[State]()
        nfas.flatMap { state => 
            evaluateState(state, input, visitedStates)
        }
    }

    def evaluateState(currentState: State, input: Option[Char],
        visitedStates: mutable.Set[State]): Set[State] = {

        if (visitedStates contains currentState) {
            Set()
        } else {
            visitedStates.add(currentState)
            currentState match {
                case placeholder: Placeholder => 
                    evaluateState(
                        placeholder.pointingTo, 
                        input,
                        visitedStates
                    )
                case consume: Consume => 
                    if (Some(consume.c) == input 
                        || consume.c == '.') { 
                        Set(consume.out) 
                    } else { 
                        Set()
                    }
                case s: Split => 
                    evaluateState(s.out1, input, visitedStates) ++ 
                    evaluateState(s.out2, input, visitedStates)
                case m: Match => 
                    if (input.isDefined) Set() else Set(Match())
            }
        }
    }
}

object Regex {
    def fullMatch(input: String, pattern: String) = {
        val parsed = RegexParser(pattern).getOrElse(
            throw new RuntimeException("Failed to parse regex")
        )
        val nfa = NFA.regexToNFA(parsed)
        NFAEvaluator.evaluate(nfa, input)
    }   

    def matchAnywhere(input: String, pattern: String) = 
        fullMatch(input, ".*" + pattern + ".*")
}

object RegexScala {
  def main(args: Array[String]):Unit = {
    Regex.fullMatch("aaaaab", "a*b")
    Regex.fullMatch("aaaaabc", "a*b")
    Regex.matchAnywhere("abcde", "cde")
  }
}
