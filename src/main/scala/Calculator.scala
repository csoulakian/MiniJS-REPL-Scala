package edu.luc.cs.laufer.cs473.expressions

import org.parboiled2.ParseError
import scala.collection.mutable.Map
import scala.util.{Try, Failure, Success}

object Calculator extends App {


  type Store = Map[String, LValue[Int]]
  val store: Store = Map.empty
  println("Memory: " + store)

  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val parser = new ExprParser(input)
    parser.InputLine.run() match {
      case Failure(error: ParseError) =>
        println("This expression could not be parsed:")
        println(parser.formatError(error))
      case Failure(error) =>
        println("This expression could not be evaluated: " + error)
      case Success(expr) =>
        import behaviors._
        println("The parsed expression is: ")
        println(toFormattedString(expr))
        //println("It has size " + size(expr) + " and depth " + depth(expr))
        println("It evaluates to " + Try(evaluate(store)(expr)))
    }
    println("Memory: " + store)
    // map is cleared after each input
    store.clear()
  }



  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    print("Enter infix expression: ")
    scala.io.Source.stdin.getLines foreach { line =>
      processExpr(line)
      print("Enter infix expression: ")
    }
  }
}