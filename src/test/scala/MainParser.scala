package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.ast.Expr
import org.scalatest.FunSuite

import TestFixtures._
import behaviors._

import scala.util.{Failure, Success}

object MainParser extends App {
  val parsedExpr = new ExprParser(complex1string).InputLine.run()
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(for(expr <- parsedExpr.get) {evaluate(expr)})
}

class TestParser extends FunSuite {
  val parsedExpr1 = new ExprParser(inputString1).InputLine.run()
  val parsedExpr2 = new ExprParser(inputString2).InputLine.run()
  val parsedExpr3 = new ExprParser(inputString3).InputLine.run()
  val parsedExpr4 = new ExprParser(inputString4).InputLine.run()
  val parsedExpr5 = new ExprParser(inputString5).InputLine.run()
  val parsedExpr6 = new ExprParser(inputString6).InputLine.run()
  val parsedExpr7 = new ExprParser(inputString7).InputLine.run()
  val parsedExpr8 = new ExprParser(inputString8).InputLine.run()
  val parsedExpr9 = new ExprParser(inputString9).InputLine.run()

  test("parser works 1") { assert(parsedExpr1.get === parsed1) }
  test("parser works 2") { assert(parsedExpr2.get === parsed2) }
  test("parser works 3") { assert(parsedExpr3.get === parsed3) }
  test("parser works 4") { assert(parsedExpr4.get === parsed4) }
  test("parser works 5") { assert(parsedExpr5.get === parsed5) }
  test("parser works 6") { assert(parsedExpr6.get === parsed6) }
  test("parser works 7") { assert(parsedExpr7.get === parsed7) }
  test("parser works 8") { assert(parsedExpr8.get === parsed8) }
  test("parser works 9") { assert(parsedExpr9.get === parsed9) }

  test("parser rejects incorrectly formatted input") {

    def parse(input: String): Seq[Expr] = {
      val parser = new ExprParser(input)
      parser.InputLine.run() match {
        case Failure(error) => throw error
        case Success(expr) => expr
      }
    }

    intercept[Exception] {parse(badInput1)}
    intercept[Exception] {parse(badInput2)}
    intercept[Exception] {parse(badInput3)}
    intercept[Exception] {parse(badInput4)}
    intercept[Exception] {parse(badInput5)}
    intercept[Exception] {parse(badInput6)}
    intercept[Exception] {parse(badInput7)}
    intercept[Exception] {parse(badInput8)}
    intercept[Exception] {parse(badInput9)}

  }
}

class TestUnparser extends FunSuite {
  test("unparser works 1") { assert(toFormattedString(parsed1) == unparsed1) }
  test("unparser works 2") { assert(toFormattedString(parsed2) == unparsed2) }
  test("unparser works 3") { assert(toFormattedString(parsed3) == unparsed3) }
  test("unparser works 4") { assert(toFormattedString(parsed4) == unparsed4) }
  test("unparser works 5") { assert(toFormattedString(parsed5) == unparsed5) }
  test("unparser works 6") { assert(toFormattedString(parsed6) == unparsed6) }
  test("unparser works 7") { assert(toFormattedString(parsed7) == unparsed7) }
  test("unparser works 8") { assert(toFormattedString(parsed8) == unparsed8) }
  test("unparser works 9") { assert(toFormattedString(parsed9) == unparsed9) }
}
