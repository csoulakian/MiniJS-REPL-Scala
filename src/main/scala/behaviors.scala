package edu.luc.cs.laufer.cs473.expressions

import ast._

object behaviors {

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r)   => -evaluate(r)
    case Plus(l, r)  => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r)   => evaluate(l) / evaluate(r)
    case Mod(l, r)   => evaluate(l) % evaluate(r)
    case Variable(v) => ???
    case Equals(v, c) => ???
    case Conditional(r, b, eb) => ???
    case Loop(r, b) => ???
    case Block(r) => ???
  }

  def size(e: Expr): Int = e match {
    case Variable(v) => 1
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
    case Equals(v, c) => ???
    case Conditional(r, b, eb) => ???
    case Loop(r, b) => ???
    case Block(r) => ???
  }

  def depth(e: Expr): Int = e match {
    case Variable(v) => 1
    case Constant(c) => 1
    case UMinus(r)   => 1 + depth(r)
    case Plus(l, r)  => 1 + math.max(depth(l), depth(r))
    case Minus(l, r) => 1 + math.max(depth(l), depth(r))
    case Times(l, r) => 1 + math.max(depth(l), depth(r))
    case Div(l, r)   => 1 + math.max(depth(l), depth(r))
    case Mod(l, r)   => 1 + math.max(depth(l), depth(r))
    case Equals(v, c) => ???
    case Conditional(r, b, eb) => ???
    case Loop(r, b) => ???
    case Block(r) => ???
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Variable(v) => prefix + v.toString
    case Constant(c) => prefix + c.toString
    case Equals(v, c) => buildExprString(prefix, " = ", toFormattedString(prefix)(v), toFormattedString(prefix)(c))
    case Conditional(i, b, eb)  => buildCondExprString(prefix, toFormattedString(prefix + INDENT)(i),
      toFormattedString(prefix + INDENT)(b), toFormattedString(prefix + INDENT)(eb))
    case Loop(exp, b)  => buildLoopExprString(prefix, toFormattedString(prefix)(exp), toFormattedString(prefix)(b))
    case b: Block => buildBlockExprString(prefix, toFormattedStrings(prefix)(b.expr))
    case UMinus(exp)   => buildUnaryExprString(prefix, toFormattedString(prefix)(exp))
    case Plus(l, r)  => buildExprString(prefix, " + ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Minus(l, r) => buildExprString(prefix, " - ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Times(l, r) => buildExprString(prefix, " * ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Div(l, r)   => buildExprString(prefix, " / ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Mod(l, r)   => buildExprString(prefix, "%", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
  }

  def toFormattedStrings(prefix: String)(e: Seq[_]): String = {
    val result = new StringBuilder(prefix)
    if(e.nonEmpty) {
      for (exp <- e) {
        result.append(toFormattedString(prefix)(exp.asInstanceOf[Expr]))
        result.append(EOL)
      }
    }
    result.toString()
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)
  def toFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)


  def buildExprString(prefix: String, opString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    result.append("(")
    result.append(leftString)
    result.append(opString)
    result.append(rightString)
    result.append(")")
    result.toString()
  }

  def buildUnaryExprString(prefix: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append("(-")
    result.append(exprString)
    result.append(")")
    result.toString()
  }

  def buildBlockExprString(prefix: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append("{")
    result.append(EOL)
    // adds an indent to each line in the exprString
    result.append(exprString.lines.map(s => INDENT + s).mkString("\n"))
    result.append(EOL)
    result.append("}")
    result.toString()
  }

  def buildLoopExprString(prefix: String, exprString: String, blockString: String) = {
    val result = new StringBuilder(prefix)
    result.append("while (")
    result.append(exprString)
    result.append(") ")
    result.append(INDENT + blockString)
    result.append(EOL)
    result.toString()
  }

  def buildCondExprString(prefix: String, ifString: String, blockString: String, elseBlock: String) = {
    val result = new StringBuilder(prefix)
    result.append("if (")
    result.append(ifString)
    result.append(") ")
    result.append(blockString)
    if(!elseBlock.isEmpty) {
      result.append(" else ")
      result.append(elseBlock)
    }
    result.append(EOL)
    result.toString()
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}