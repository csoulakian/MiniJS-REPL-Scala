import edu.luc.cs.laufer.cs473.expressions._
import edu.luc.cs.laufer.cs473.expressions.ast.Expr
import edu.luc.cs.laufer.cs473.expressions.behaviors._
import org.parboiled2.ParseError
import scala.util.{Try, Success, Failure}
val inputString1 = "x = 5;"
val inputString2 = "x = 5 ; y = 7;"
val inputString3 = "((1 + y2) - (3 * y4)) / 5;"
val inputString4 = "x = ((1 + y2) - (3 * y4)) / 5;"
val inputString5 = "if (1) { x = 2; }"
val inputString6 = "if (1) { x = 2; } else { x = 3; }"
val inputString7 = "{ r = r + x; y = y + 1 ; }"
val inputString8 = "if (4) { r = r + x; y = y + 1; }"
val inputString9 = "while (y) {r = r + x; y = y - 1;}"
val badInput1 = "x = 5"
