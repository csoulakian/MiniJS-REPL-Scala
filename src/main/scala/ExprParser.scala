package edu.luc.cs.laufer.cs473.expressions

import org.parboiled2._
import ast._

class ExprParser(val input: ParserInput) extends Parser {

  def InputLine = rule { WhiteSpace ~ zeroOrMore(Statement) ~ EOI }

  /**   statement   ::= expression ";" | assignment | conditional | loop | block  */
  def Statement: Rule1[Expr] = rule { Expression ~ ws(';') | Assignment | Cond | Lo | Blo }


  /**   assignment    ::= ident { "." ident }* "=" expression ";"  */
  def Assignment = rule { ident ~ zeroOrMore(ws('.') ~ ident) ~ ws('=') ~ Expression ~ ws(';') ~>
    (Equals(_: Expr, _: Expr))}

  /**   equation     ::= expression "=" expression
    * only place it's used is in beginning of if conditional */
  def Equation = rule { Expression ~ ws('=') ~ Expression ~> (Equals(_: Expr, _: Expr))}

  /**   conditional   ::= "if" "(" expression ")" block [ "else" block ]
    *
    * if there is no else, then Conditional.elseBlock = Block(List()) (a block of an empty list).
    * The "or" operator takes 1st option as precedence - if the word "else" is not found when parsing
    * the input, then it will take the 2nd option of just a block at the end.
    * Ignore false errors on the or operator and ~>
    * */
  def Cond = rule {
    "if" ~ WhiteSpace ~ ws('(') ~ Equation ~ ws(')') ~ (
      (Blo ~ "else" ~ WhiteSpace ~ Blo ~> (Conditional(_: Equals, _: Block, _: Block)))
        | (Blo ~> (Conditional(_: Equals, _: Block, Block(): Block)))
    )
  }

  /**   loop    ::= "while" "(" expression ")" block  */
  def Lo = rule { "while" ~ WhiteSpace ~ ws('(') ~ Expression ~ ws(')') ~ Blo ~> (Loop(_: Expr, _: Block))}

  /**   block   ::= "{" statement* "}"  */
  def Blo = rule { ws('{') ~ zeroOrMore(Statement) ~ ws('}') ~> (Block(_: _*))}


  /**   expression  ::= term { { "+" | "-" } term }*  */
  def Expression = rule {
    Term ~ zeroOrMore(
      ws('+') ~ Term ~> (Plus(_: Expr, _))
        | ws('-') ~ Term ~> (Minus(_: Expr, _))
    )
  }

  /**   term    ::= factor { { "*" | "/" | "%" } factor }*  */
  def Term = rule {
    Factor ~ zeroOrMore(
      ws('*') ~ Factor ~> (Times(_: Expr, _))
        | ws('/') ~ Factor ~> (Div(_: Expr, _))
        | ws('%') ~ Factor ~> (Mod(_: Expr, _))
    )
  }

  /** factor       ::= simplefactor { "." ident }*    */
  def Factor: Rule1[Expr] = rule { SimpleFactor ~ zeroOrMore(ws('.') ~ ident) }

  /**   simplefactor    ::= ident | number | "+" factor | "-" factor | "(" expression ")" | struct */
  def SimpleFactor: Rule1[Expr] = rule { ident | Number | UnaryPlus | UnaryMinus | Parens | Struct }

  /** struct ::= "{" "}" | "{" field { "," field }* "}"     */
  def Struct = rule { ws('{') ~ ws('}') | ws('{') ~ field ~ zeroOrMore(ws(',') ~ field) ~ ws('}') ~>
    ((allFields: Seq[(Variable,Expr)]) => Structure(allFields.toMap)) }


  // explicitly handle trailing whitespace

  def select = rule { Expression ~ oneOrMore(ws('.') ~ field) }

  def field = rule { ident ~ ws(':') ~ Expression ~> ((_,_)) }

  def ident = rule { capture(Alphabet) ~ WhiteSpace ~> ((s: String) => Variable(s.trim)) }

  def Number = rule { capture(Digits) ~ WhiteSpace ~> ((s: String) => Constant(s.toInt)) }

  def UnaryPlus = rule { ws('+') ~ Factor }

  def UnaryMinus = rule { ws('-') ~ Factor ~> (UMinus(_: Expr)) }

  def Parens = rule { ws('(') ~ Expression ~ ws(')') }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }

  def Alphabet = rule { oneOrMore(CharPredicate.Alpha) ~ zeroOrMore(CharPredicate.AlphaNum) }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")

  def WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }
}