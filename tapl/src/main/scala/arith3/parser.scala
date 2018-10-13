package arith3

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object ArithParsers extends StandardTokenParsers with ImplicitConversions {

  lexical.reserved += ("true", "false", "if", "then", "else", "iszero", "succ", "pred")
  lexical.delimiters += ("(", ")", ";")

  private def term: Parser[Tm] = appTerm |
    ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ (new TmIf(_,_,_))

  private def appTerm: Parser[Tm] =
    aTerm |
      "succ" ~> aTerm ^^ (new TmSucc(_)) |
      "pred" ~> aTerm ^^ (new TmPred(_)) |
      "iszero" ~> aTerm ^^ (new TmIsZero(_))

  //  Atomic terms are ones that never require extra parentheses
  private def aTerm: Parser[Tm] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => TmTrue } |
      "false" ^^ { _ => TmFalse } |
      numericLit ^^ { x => num(x.toInt) }

  private def num(x: Int): Tm = x match {
    case 0 => TmZero
    case _ => new TmSucc(num(x - 1))
  }

  private def eof: Parser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  def inputTm(s: String): Tm = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }
}
