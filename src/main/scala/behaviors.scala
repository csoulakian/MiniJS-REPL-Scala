package edu.luc.cs.laufer.cs473.expressions

import ast._


/**
 * Something that can be used on the right-hand side of an assignment.
 */
trait RValue[T] {
  def get: T
}

/**
 * Something that can be used on the left-hand side of an assignment.
 */
trait LValue[T] extends RValue[T] {
  def set(value: T): LValue[T]
}

/**
 * A cell for storing a value.
 */
case class Cell[T](var value: T) extends LValue[T] {
  override def get = value
  override def set(value: T) = { this.value = value; this }
}

/**
 * A companion object defining a useful Cell instance.
 */
object Cell {
  val NULL = Cell(0)
}


object Execute {

  type Store = Map[String, LValue[Int]]

  def apply(store: Store)(s: Expr): LValue[Int] = s match {
    case Constant(value)    => Cell(value)
    case Plus(left, right)  => Cell(apply(store)(left).get + apply(store)(right).get)
    case Minus(left, right) => Cell(apply(store)(left).get - apply(store)(right).get)
    case Times(left, right) => Cell(apply(store)(left).get * apply(store)(right).get)
    case Div(left, right)   => Cell(apply(store)(left).get / apply(store)(right).get)
    case Variable(name)     => store(name)
    case Equals(left, right) =>
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      lvalue.set(rvalue.get)
    case Block(statements @ _*) =>
      statements.foldLeft(Cell.NULL.asInstanceOf[LValue[Int]])((c, s) => apply(store)(s))
    case Loop(guard, body) =>
      var gValue = apply(store)(guard)
      while (gValue.get != 0) {
        apply(store)(body)
        gValue = apply(store)(guard)
      }
      Cell.NULL
  }


}

object behaviors {

  type Store = Map[String, LValue[Int]]

  def evaluate(e: Seq[_]): Unit = {

    val store: Store = Map[String, LValue[Int]]()

    println(store)

    if (e.nonEmpty) {
      for(exp <- e) Execute(store)(exp.asInstanceOf[Expr])
    }

    println(store)

  }

  def size(e: Expr): Int = e match {
    case Variable(v)           => 1
    case Constant(c)           => 1
    case UMinus(r)             => 1 + size(r)
    case Plus(l, r)            => 1 + size(l) + size(r)
    case Minus(l, r)           => 1 + size(l) + size(r)
    case Times(l, r)           => 1 + size(l) + size(r)
    case Div(l, r)             => 1 + size(l) + size(r)
    case Mod(l, r)             => 1 + size(l) + size(r)
    case Equals(v, c)          => ???
    case Conditional(r, b, eb) => ???
    case Loop(r, b)            => ???
    case Block(r)              => ???
  }

  def depth(e: Expr): Int = e match {
    case Variable(v)           => 1
    case Constant(c)           => 1
    case UMinus(r)             => 1 + depth(r)
    case Plus(l, r)            => 1 + math.max(depth(l), depth(r))
    case Minus(l, r)           => 1 + math.max(depth(l), depth(r))
    case Times(l, r)           => 1 + math.max(depth(l), depth(r))
    case Div(l, r)             => 1 + math.max(depth(l), depth(r))
    case Mod(l, r)             => 1 + math.max(depth(l), depth(r))
    case Equals(v, c)          => ???
    case Conditional(r, b, eb) => ???
    case Loop(r, b)            => ???
    case Block(r)              => ???
  }

  // output of parser is always a Seq[Expr],
  // toFormattedString in Calculator calls this method first
  def toFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)

  // e = a sequence of zero or more Expr
  def toFormattedStrings(prefix: String)(e: Seq[_]): String = {
    val result = new StringBuilder(prefix)
    if (e.nonEmpty) {
      for (exp <- e) {
        result.append(toFormattedString(prefix)(exp.asInstanceOf[Expr]))
        result.append(EOL)
      }
    }
    result.toString()
  }

  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Variable(v)           => prefix + v.toString
    case Constant(c)           => prefix + c.toString
    case Equals(v, c)          => buildExprString(prefix, " = ", toFormattedString(prefix)(v), toFormattedString(prefix)(c))
    case Conditional(i, b, eb) => buildCondExprString(prefix, toFormattedString(prefix)(i),
      toFormattedString(prefix)(b), toFormattedString(prefix)(eb))
    case Loop(exp, b)          => buildLoopExprString(prefix, toFormattedString(prefix)(exp), toFormattedString(prefix)(b))
    // block always comes in as a block of a sequence of zero or more Expr
    case b: Block    => buildBlockExprString(prefix, toFormattedStrings(prefix)(b.expr))
    case UMinus(exp) => buildUnaryExprString(prefix, toFormattedString(prefix)(exp))
    case Plus(l, r)  => buildExprString(prefix, " + ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Minus(l, r) => buildExprString(prefix, " - ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Times(l, r) => buildExprString(prefix, " * ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Div(l, r)   => buildExprString(prefix, " / ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
    case Mod(l, r)   => buildExprString(prefix, "%", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, opString: String, leftString: String, rightString: String) = {
    val result = new StringBuilder(prefix)
    opString match {
      case " = " =>
        result.append(leftString)
        result.append(opString)
        result.append(rightString)
        result.append(";")
      case _     =>
        result.append("(")
        result.append(leftString)
        result.append(opString)
        result.append(rightString)
        result.append(")")
    }
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
    if (exprString.trim.length > 0) {
      result.append("{")
      result.append(EOL)
      // adds an indent to each line in the exprString
      result.append(exprString.lines.map(s => INDENT + s).mkString(EOL))
      result.append(EOL)
      result.append("}")
    }
    result.toString()
  }

  def buildLoopExprString(prefix: String, exprString: String, blockString: String) = {
    val result = new StringBuilder(prefix)
    result.append("while (")
    result.append(exprString)
    result.append(") ")
    result.append(blockString)
    result.toString()
  }

  def buildCondExprString(prefix: String, ifString: String, blockString: String, elseBlock: String) = {
    val result = new StringBuilder(prefix)
    result.append("if (")
    result.append(ifString)
    result.append(") ")
    result.append(blockString)
    if (elseBlock.trim.length > 0) {
      result.append(" else ")
      result.append(elseBlock)
    }
    result.toString()
  }

  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."

}

