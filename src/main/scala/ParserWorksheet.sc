import edu.luc.cs.laufer.cs473.expressions._
import edu.luc.cs.laufer.cs473.expressions.behaviors
import scala.util.{Try, Failure, Success}
import scala.collection.mutable.Map
val inputString5 = "if(2+3=5){x=2;}"
val inputString6 = "if (2-9=9) { x =2 ; }else{x = 3  ; }"
val inputString8 = "if (2+3=5) { r =r+ x; y= y+ 1; }"
val parser = new ExprParser(inputString6)
parser.InputLine.run() match {
  case Success(expr) =>
    println(expr)
    println(behaviors.toFormattedString(expr))
}
