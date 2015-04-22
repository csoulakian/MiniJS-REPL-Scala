import edu.luc.cs.laufer.cs473.expressions.ast._
import edu.luc.cs.laufer.cs473.expressions._
import scala.util.{Try, Failure, Success}
import scala.collection.mutable.Map
type Store = Map[String, LValue[Int]]
val store: Store = Map.empty

val badEval1 = "x;"
val parsedExpr = new ExprParser(badEval1).InputLine.run()
Try(behaviors.evaluate(store)(parsedExpr.get))

