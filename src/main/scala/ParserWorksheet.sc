import edu.luc.cs.laufer.cs473.expressions._
import org.parboiled2.ParseError
import scala.util.{Success, Failure}


val input2: String = "if(0) {2+3;} else {3+4;}"
val input: String = "if(0) {2+3;}"
val parser = new ExprParser(input)
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





