import edu.luc.cs.laufer.cs473.expressions._
import edu.luc.cs.laufer.cs473.expressions.ast._
//import edu.luc.cs.laufer.cs473.expressions.behaviors._

import org.parboiled2.ParseError
import scala.util.{Success, Failure}
val EOL = scala.util.Properties.lineSeparator
val INDENT = ".."
val input2: String = "if(0) {2+3;} else {3+4;}"
val input3: String = "if(0) {2+3;}"
val input1: String = "{2+3; 4+7;}"
// after parsing, result is always a seq
// result of input1 is this:
// a seq of a single Expr (block). block is a seq of 2 Expr
val myExpr = Vector(Block(Vector(Plus(Constant(2),Constant(3)),
  Plus(Constant(4),Constant(7)))))

// when parsed result first comes in
// normal case of having seq of expr coming in
def toFormattedString(e: Seq[Expr]): String = toFormattedStrings("")(e)

// e = original seq of expr that we started with
// in this case, only one expr (block) in this sequence
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

// exp is a sequence of Expr or empty seq
// here, exp = seq of 2 constant Expr
def toFormattedString(prefix: String)(e: Expr): String = e match {
  case b: Block => buildBlockExprString(prefix, toFormattedStrings(prefix)(b.expr))
  case Constant(c) => prefix + c.toString
  case Plus(l, r)  => buildExprString(prefix, " + ", toFormattedString(prefix)(l), toFormattedString(prefix)(r))
}

def toFormattedString(e: Expr): String = toFormattedString("")(e)


def buildBlockExprString(prefix: String, exprString: String) = {
  val result = new StringBuilder(prefix)
  result.append("{")
  result.append(EOL)
  result.append(INDENT + exprString)
  result.append(EOL)
  result.append("}")
  result.toString()
}

def buildExprString(prefix: String, opString: String, leftString: String, rightString: String) = {
  val result = new StringBuilder(prefix)
  result.append("(")
  result.append(leftString)
  result.append(opString)
  result.append(rightString)
  result.append(")")
  result.toString()
}


/*
val parser = new ExprParser(input1)
parser.InputLine.run() match {
  case Failure(error: ParseError) =>
    println("This expression could not be parsed:")
    println(parser.formatError(error))
  case Failure(error) =>
    println("This expression could not be evaluated: " + error)
  case Success(expr) =>
    println("The parsed expression is: ")
    println(toFormattedString(expr))
}*/
