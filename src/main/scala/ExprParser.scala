package edu.luc.cs.laufer.cs473.expressions

import org.parboiled2._
import ast._

class ExprParser(val input: ParserInput) extends Parser {

  // explicitly handle leading whitespace
  def InputLine = rule { zeroOrMore(' ') ~ Expression ~ EOI }

  /** expr ::= term { { "+" | "-" } term }* */
  def Expression = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> (Plus(_: Expr, _))
    | '-' ~ Term ~> (Minus(_: Expr, _))
    )
  }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> (Times(_: Expr, _))
    | '/' ~ Factor ~> (Div(_: Expr, _))
    | '%' ~ Factor ~> (Mod(_: Expr, _))
    )
  }

  /** factor ::= number | "+" factor | "-" factor | "(" expr ")" */
  def Factor: Rule1[Expr] = rule { Number | UnaryPlus | UnaryMinus | Parens | ident }

  // explicitly handle trailing whitespace

  def ident = rule { oneOrMore(CharPredicate.Alpha) ~ capture(Digits) ~ zeroOrMore(' ') ~> ((s: String) => Variable(s)) }

  def Number = rule { capture(Digits) ~ zeroOrMore(' ') ~> ((s: String) => Constant(s.toInt)) }

  def UnaryPlus = rule { '+' ~ Factor }

  def UnaryMinus = rule { '-' ~ Factor ~> (UMinus(_: Expr)) }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }

  def Alphabets   = rule { oneOrMore(CharPredicate.Alpha) }

  /** Automatically handles whitespace after single character terminals. */
  implicit def wspChar(c: Char): Rule0 = rule { ch(c) ~ zeroOrMore(' ') }

  /** Automatically handles whitespace after string terminals. */
  implicit def wspStr(s: String): Rule0 = rule { str(s) ~ zeroOrMore(' ') }

}
