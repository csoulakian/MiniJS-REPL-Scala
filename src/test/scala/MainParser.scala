package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import TestFixtures._

object MainParser extends App {
  val parsedExpr = new ExprParser(complex1string).InputLine.run()
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(for(expr <- parsedExpr.get) {behaviors.evaluate(expr)})
}

class TestParser extends FunSuite {
  val parsedExpr = new ExprParser(complex1string).InputLine.run()
  val parsedExpr2 = new ExprParser(complex1string2).InputLine.run()
  test("parser works 1") { assert(parsedExpr.get === complex1) }
  test("parser works 2") { assert(parsedExpr2.get === complex1) }
}
