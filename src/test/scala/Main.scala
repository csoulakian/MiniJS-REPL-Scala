package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.{BeforeAndAfter, FunSuite}

import behaviors._
import TestFixtures._

import scala.collection.mutable.Map
import scala.util.{Failure, Success, Try}

object Main extends App {

  type Store = Map[String, LValue[Int]]
  type Value = LValue[Int]
  val store: Store = Map.empty

  println("p = " + complex1)
  println(toFormattedString(complex1))
  println("evaluate(p) = " + evaluate(store)(complex1))
  println()

  store.clear()

  println("q = " + complex2)
  println(toFormattedString(complex2))
  println("evaluate(q) = " + evaluate(store)(complex2))

}

class Test extends FunSuite with BeforeAndAfter {

  type Store = Map[String, LValue[Int]]
  type Value = LValue[Int]
  type Result = Try[Value]
  val store: Store = Map.empty

  before {
    store.clear()
  }

  test("evaluate expr1") {
    assert(Try(evaluate(store)(parsed1)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 1)
    assert(store("x") === Cell(5))
  }

  test("evaluate expr2") {
    assert(Try(evaluate(store)(parsed2)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 2)
    assert(store("x") === Cell(5))
    assert(store("y") === Cell(7))
  }

  test("evaluate expr3") {
    assert(Try(evaluate(store)(parsed3)) === Success(Cell(-4).asInstanceOf[Value]))
    assert(store.size === 2)
    assert(store("y2") === Cell(6))
    assert(store("y4") === Cell(9))
  }

  test("evaluate expr4") {
    assert(Try(evaluate(store)(parsed4)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 2)
    assert(store("y2") === Cell(6))
    assert(store("y4") === Cell(-4))
  }

  test("evaluate expr5") {
    assert(Try(evaluate(store)(parsed5)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 1)
    assert(store("x") === Cell(2))
  }

  test("evaluate expr6") {
    assert(Try(evaluate(store)(parsed6)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 1)
    assert(store("x") === Cell(3))
  }

  test("evaluate expr7") {
    assert(Try(evaluate(store)(parsed7)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 3)
    assert(store("x") === Cell(6))
    assert(store("y") === Cell(3))
    assert(store("r") === Cell(8))
  }

  test("evaluate expr8") {
    assert(Try(evaluate(store)(parsed8)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 3)
    assert(store("x") === Cell(4))
    assert(store("y") === Cell(0))
    assert(store("r") === Cell(-2))
  }

  test("evaluate expr9") {
    assert(Try(evaluate(store)(parsed9)) === Success(Cell(0).asInstanceOf[Value]))
    assert(store.size === 3)
    assert(store("x") === Cell(2))
    assert(store("y") === Cell(0))
    assert(store("r") === Cell(9))
  }

  test("evaluator fails to evaluate unknown input") {

    def check(input: String): Result = {
      val parsedExpr = new ExprParser(input).InputLine.run()
      Try(behaviors.evaluate(store)(parsedExpr.get))
    }

    intercept[Exception] {check(badEval1).get}
    intercept[Exception] {check(badEval2).get}
    intercept[Exception] {check(badEval3).get}
    intercept[Exception] {check(badEval3).get}
  }

}
