import edu.luc.cs.laufer.cs473.expressions.{Cell, LValue}
import edu.luc.cs.laufer.cs473.expressions.ast._
import scala.collection.mutable.Map
//import edu.luc.cs.laufer.cs473.expressions.behaviors._
val parsed1: Seq[Expr] = Seq(Equals(Variable("x"), Constant(5)))
val single: Expr = Equals(Variable("x"), Constant(5))
type Store = Map[String, LValue[Int]]
val store: Store = Map.empty

def apply(store: Store)(s: Expr): LValue[Int] = s match {
  case Constant(value)    => Cell(value)
  case Plus(left, right)  => Cell(apply(store)(left).get + apply(store)(right).get)
  case Minus(left, right) => Cell(apply(store)(left).get - apply(store)(right).get)
  case Times(left, right) => Cell(apply(store)(left).get * apply(store)(right).get)
  case Div(left, right)   => Cell(apply(store)(left).get / apply(store)(right).get)
  //case var if var is already defined
  case Variable(name)     => store(name)
  case Equals(left, right) =>
    val rvalue = apply(store)(right)
    if (store contains left.str) {
      val lvalue = apply(store)(left)
      lvalue.set(rvalue.get)
    }
    else store(left.str)




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
println(store)
apply(store)(single)
println(store)