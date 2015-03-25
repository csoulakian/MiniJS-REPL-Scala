package edu.luc.cs.laufer.cs473.expressions

import org.parboiled2.RuleFrame.ZeroOrMore
import org.parboiled2._
import ast._

class ExprParser(val input: ParserInput) extends Parser {

  def InputLine = rule { WhiteSpace ~ Statement ~ EOI }

  // statement  ::= expression ";" | assignment | conditional | loop | block
  def Statement: Rule1[Expr] = rule { Expression ~ ws(';') | Assignment | Lo | Blo | con }


  /** assignment  ::= ident "=" expression ";" */
  def Assignment = rule { ident ~ ws('=') ~ Expression ~ ws(';') ~> (Equals(_: Variable, _: Expr))}
 def con=rule { "if" ~ WhiteSpace ~ ws('(') ~ Expression ~ ws(')') ~ Blo ~> (Conditional(_: Expr, _: Block,Block(Seq())))}
  // conditional ::= "if" "(" expression ")" block [ "else" block ]
/*  def Condx = rule { "if" ~ WhiteSpace ~ ws('(') ~ Expression ~ ws(')') ~ Blo ~ zeroOrMore(
    "else" ~ WhiteSpace ~ Blo) ~> (Conditional(_: Expr, _: Block)) }*/

/*  def Cond = rule {
    "if" ~ WhiteSpace ~ ws('(') ~ Expression ~ ws(')') ~ (
      Blo ~> (Conditional(_: Expr, _: Block))
      )
      )
  }*/

  // | (Blo ~ "else" ~ WhiteSpace ~ Blo) ~> (Conditional(_: Expr, _: Block, _: Block)

  // loop   ::= "while" "(" expression ")" block
  def Lo = rule { "while" ~ WhiteSpace ~ ws('(') ~ Expression ~ ws(')') ~ Blo ~> (Loop(_: Expr, _: Block))}

  // block       ::= "{" statement* "}"
  def Blo = rule { ws('{') ~ zeroOrMore( Statement) ~ ws('}') ~> (Block(_))}


  /** expression ::= term { { "+" | "-" } term }* */
  def Expression = rule {
    Term ~ zeroOrMore(
      ws('+') ~ Term ~> (Plus(_: Expr, _))
        | ws('-') ~ Term ~> (Minus(_: Expr, _))
    )
  }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def Term = rule {
    Factor ~ zeroOrMore(
      ws('*') ~ Factor ~> (Times(_: Expr, _))
        | ws('/') ~ Factor ~> (Div(_: Expr, _))
        | ws('%') ~ Factor ~> (Mod(_: Expr, _))
    )
  }

  /** factor ::= ident | number | "+" factor | "-" factor | "(" expression ")" */
  def Factor: Rule1[Expr] = rule { ident | Number | UnaryPlus | UnaryMinus | Parens }


  // explicitly handle trailing whitespace

  def ident = rule { capture(Alphabet) ~ WhiteSpace ~> ((s: String) => Variable(s.trim)) }

  def Number = rule { capture(Digits) ~ WhiteSpace ~> ((s: String) => Constant(s.toInt)) }

  def UnaryPlus = rule { ws('+') ~ Factor }

  def UnaryMinus = rule { ws('-') ~ Factor ~> (UMinus(_: Expr)) }

  //def UEquals = rule { ident ~ ws('=') ~ Expression ~ ws(';') }

  def Parens = rule { ws('(') ~ Expression ~ ws(')') }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }

  def Alphabet = rule { oneOrMore(CharPredicate.Alpha) ~ zeroOrMore(CharPredicate.AlphaNum) }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }
}