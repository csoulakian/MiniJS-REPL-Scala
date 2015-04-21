package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import behaviors._
import TestFixtures._

object Main extends App {
  println("p = " + complex1)
  //println("evaluate(p) = " + evaluate(complex1))
  println(toFormattedString(complex1))
  println("q = " + complex2)
  //println("evaluate(q) = " + evaluate(complex2))
  println(toFormattedString(complex2))
}

class Test extends FunSuite {
  //test("evaluate(p)") { assert(evaluate(complex1) === -1) }
}
