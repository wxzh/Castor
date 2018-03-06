package comp.tapl.untyped

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// This parser is done exactly in the same way as in TAPL.
// The oddity of this parser (elementary parsers return functions) is driven by the desire
// to avoid double hierarchy of terms (named and nameless). 
// The input text represents named terms. The module works with nameless terms 
// So translation from named form into nameless form is done on the fly during parsing.
object Parser extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("_")
  lexical.delimiters += ("(", ")", ";", "/", ".", "\\")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  type Res[A] = Context => A

  lazy val term: PackratParser[Res[Term]] =
    appTerm |
      ("\\" ~> lcid) ~ ("." ~> term) ^^ { case v ~ t => ctx: Context => TmAbs(v, t(ctx.addName(v))) } |
      ("\\" ~ "_") ~> ("." ~> term) ^^ { t => ctx: Context => TmAbs("_", t(ctx.addName("_"))) }

  lazy val appTerm: PackratParser[Res[Term]] =
    (appTerm ~ aTerm) ^^ { case t1 ~ t2 => ctx: Context => TmApp(t1(ctx), t2(ctx)) } | aTerm

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => ctx: Context => TmVar(ctx.name2index(i), ctx.length) }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }

}
