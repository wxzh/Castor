package tapl.arith

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object ArithParsers extends StandardTokenParsers with ImplicitConversions {
  import Arith._

  lexical.reserved += ("true", "false", "if", "then", "else", "iszero", "succ", "pred")
  lexical.delimiters += ("(", ")", ";")

  private def topLevel: Parser[List[Command]] =
    (command <~ ";") ~ topLevel ^^ { case c ~ cs => c :: cs } |
      success(List())

  private def command: Parser[Command] =
    term ^^ Eval

  private def term: Parser[Tm] = appTerm |
    ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ TmIf

  private def appTerm: Parser[Tm] =
    aTerm |
      "succ" ~> aTerm ^^ TmSucc |
      "pred" ~> aTerm ^^ TmPred |
      "iszero" ~> aTerm ^^ TmIsZero

  //  Atomic terms are ones that never require extra parentheses
  private def aTerm: Parser[Tm] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => TmTrue } |
      "false" ^^ { _ => TmFalse } |
      numericLit ^^ { x => num(x.toInt) }

  private def num(x: Int): Tm = x match {
    case 0 => TmZero
    case _ => TmSucc(num(x - 1))
  }

  private def eof: Parser[String] = elem("<eof>", _ == lexical.EOF) ^^ { _.chars }

  def input(s: String) = phrase(topLevel)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => sys.error(t.toString)
  }

  def inputTm(s: String): Tm = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }
}
