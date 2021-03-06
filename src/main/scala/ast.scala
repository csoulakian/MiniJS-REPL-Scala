package edu.luc.cs.laufer.cs473.expressions.ast

/** An initial algebra of arithmetic expressions.
 */
sealed trait Expr
case class Variable(str: String) extends Expr
case class Constant(value: Int) extends Expr
case class Equals(left: Seq[Expr], right: Expr) extends Expr
case class Conditional(expr: Equals, block: Block, elseBlock: Block) extends Expr
case class Loop(expr: Expr, block: Block) extends Expr
case class Block(expr: Expr*) extends Expr
case class UMinus(expr: Expr) extends Expr
case class Plus(left: Expr, right: Expr) extends Expr
case class Minus(left: Expr, right: Expr) extends Expr
case class Times(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Mod(left: Expr, right: Expr) extends Expr
case class Structure(m: Map[Variable,Expr]) extends Expr
case class Select(expr: Expr, fieldSelector: Variable*) extends Expr
