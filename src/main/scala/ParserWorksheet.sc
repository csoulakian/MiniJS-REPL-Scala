import edu.luc.cs.laufer.cs473.expressions._
import edu.luc.cs.laufer.cs473.expressions.ast.{Expr, Block}
import edu.luc.cs.laufer.cs473.expressions.behaviors
import org.parboiled2.ParseError
import scala.util.{Success, Failure}
val EOL = scala.util.Properties.lineSeparator
val INDENT = ".."
val input2: String = "if(0) {2+3;} else {3+4;}"
val input: String = "if(0) {2+3;}"
val parser = new ExprParser(input2)
val m = parser.InputLine.run() match {
  case Failure(error: ParseError) =>
    println("This expression could not be parsed:")
    println(parser.formatError(error))
  case Failure(error) =>
    println("This expression could not be evaluated: " + error)
  case Success(expr) =>
    println("The parsed expression is: ")
    expr
}

def toFormattedString(prefix: String)(e: Expr): String = e match {
  case Block(exp) => buildBlockExprString(prefix, toFormattedStrings(prefix)(exp))
}

def toFormattedStrings(prefix: String)(e: Seq[Expr]):String=  {
  val result = new StringBuilder(prefix)

  for(exp <- e) {
    result.append(toFormattedString(prefix)(exp))
    result.append(EOL)
  }
  result.toString()
}

def toFormattedString(e: Expr): String = toFormattedString("")(e)
def toFormattedString(e: Seq[_]): String = {
  if(e.nonEmpty) toFormattedStrings("")(e.asInstanceOf[Seq[Expr]])
  else ""
}

def buildBlockExprString(prefix: String, exprString: String) = {
  val result = new StringBuilder(prefix)
  result.append("{")
  result.append(EOL)
  result.append(INDENT + exprString)
  result.append(EOL)
  result.append("}")
  result.toString()
}

val e = Block(Seq())

toFormattedString(e)



