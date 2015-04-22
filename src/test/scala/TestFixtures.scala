package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

  import ast._
  val EOL = scala.util.Properties.lineSeparator

  val inputString1 = "x=5 ;"
  val inputString2 = "x = 5 ;y = 7;  "
  val inputString3 = "y2=  6;y4=9;((1+y2)- (3 * y4 ) ) / 5 ;"
  val inputString4 = "y2 =6 ; y4=9;y4=((1 + y2  ) - (  3 * y4))/5;"
  val inputString5 = "if(2+3=5){x=2;}"
  val inputString6 = "if (2-9=9) { x =2 ; }else{x = 3  ; }"
  val inputString7 = "r=6%4;x=6;y=2;{ r = r+x; y=y + 1; }"
  val inputString8 = "{x=4;y=-1;r=-6;}if (2+3=5) { r =r+ x; y= y+ 1; }"
  val inputString9 = "{x=2;y=3;r=3;}while (  y  ){   r=r+x;y=y-1;   }"

  val parsed1: Seq[Expr] = Seq(Equals(Variable("x"), Constant(5)))
  val parsed2: Seq[Expr] = Seq(Equals(Variable("x"), Constant(5)), Equals(Variable("y"), Constant(7)))
  val parsed3: Seq[Expr] = Seq(Equals(Variable("y2"),Constant(6)), Equals(Variable("y4"),Constant(9)), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5)))
  val parsed4: Seq[Expr] = Seq(Equals(Variable("y2"),Constant(6)), Equals(Variable("y4"),Constant(9)), Equals(Variable("y4"), Div(Minus(Plus(Constant(1), Variable("y2")),
    Times(Constant(3), Variable("y4"))), Constant(5))))
  val parsed5: Seq[Expr] = Seq(Conditional(Equals(Plus(Constant(2),Constant(3)),Constant(5)), Block(Equals(Variable("x"), Constant(2))), Block()))
  val parsed6: Seq[Expr] = Seq(Conditional(Equals(Minus(Constant(2),Constant(9)),Constant(9)), Block(Equals(Variable("x"), Constant(2))),
    Block(Equals(Variable("x"), Constant(3)))))
  val parsed7: Seq[Expr] = Seq(Equals(Variable("r"),Mod(Constant(6),Constant(4))), Equals(Variable("x"),Constant(6)), Equals(Variable("y"),Constant(2)),Block(Equals(Variable("r"), Plus(Variable("r"), Variable("x"))),
    Equals(Variable("y"), Plus(Variable("y"), Constant(1)))))
  val parsed8: Seq[Expr] = Seq(Block(Equals(Variable("x"),Constant(4)), Equals(Variable("y"),UMinus(Constant(1))),
    Equals(Variable("r"),UMinus(Constant(6)))), Conditional(Equals(Plus(Constant(2),Constant(3)),Constant(5)),
    Block(Equals(Variable("r"),Plus(Variable("r"), Variable("x"))), Equals(Variable("y"), Plus(Variable("y"), Constant(1)))), Block()))
  val parsed9: Seq[Expr] = Seq(Block(Equals(Variable("x"),Constant(2)), Equals(Variable("y"),Constant(3)),
    Equals(Variable("r"),Constant(3))), Loop(Variable("y"), Block(Equals(Variable("r"),
    Plus(Variable("r"), Variable("x"))), Equals(Variable("y"), Minus(Variable("y"), Constant(1))))))

  val unparsed1 = "x = 5;" + EOL
  val unparsed2 = "x = 5;" + EOL + "y = 7;" + EOL
  val unparsed3 = "y2 = 6;" + EOL + "y4 = 9;" + EOL + "(((1 + y2) - (3 * y4)) / 5)" + EOL
  val unparsed4 = "y2 = 6;" + EOL + "y4 = 9;" + EOL + "y4 = (((1 + y2) - (3 * y4)) / 5);" + EOL
  val unparsed5 = "if ((2 + 3) = 5) {" + EOL + "..x = 2;" + EOL + "}" + EOL
  val unparsed6 = "if ((2 - 9) = 9) {" + EOL + "..x = 2;" + EOL + "} else {" + EOL + "..x = 3;" + EOL + "}" + EOL
  val unparsed7 = "r = (6%4);" + EOL + "x = 6;" + EOL + "y = 2;" + EOL + "{" + EOL + "..r = (r + x);" + EOL + "..y = (y + 1);" + EOL + "}" + EOL
  val unparsed8 = "{" + EOL + "..x = 4;" + EOL + "..y = (-1);" + EOL + "..r = (-6);" + EOL + "}" + EOL + "if ((2 + 3) = 5) {" + EOL + "..r = (r + x);" + EOL + "..y = (y + 1);" + EOL + "}" + EOL
  val unparsed9 = "{" + EOL + "..x = 2;" + EOL + "..y = 3;" + EOL + "..r = 3;" + EOL + "}" + EOL + "while (y) {" + EOL + "..r = (r + x);" + EOL + "..y = (y - 1);" + EOL + "}" + EOL


  val badInput1 = "x = 5"
  val badInput2 = "x = 5\ny = 7;  "
  val badInput3 = "((1 + y 2) - (3 * y 4)) / 5;"
  val badInput4 = "((1 + y2) - (3 * y4)) / 5"
  val badInput5 = "if (1)"
  val badInput6 = "if (2-3) { x = 2; } else ( x = 3; )"
  val badInput7 = "( r = r + x; y = y + 1 ; )"
  val badInput8 = "while (2+3=5)"
  val badInput9 = "while (y) (r = r + x)"

  val badEval1 = "x;"
  val badEval2 = "r = r + x;"
  val badEval3 = "x = 2 + x;"
  val badEval4 = "if(2+2=4) {x + 3;}"


  val complex1: Seq[Expr] = Seq(Mod(Minus(Plus(Constant(1), Constant(2)), Times(UMinus(Constant(3)),Constant(4))),Constant(5)))
  val complex2: Seq[Expr] = Seq(Conditional(Equals(Plus(Constant(2),Constant(2)),Constant(5)),
    Block(Div(Minus(Plus(Constant(1),Constant(2)),Times(Constant(3),Constant(4))),Constant(5))),
    Block(Block(Equals(Variable("x"),Constant(2)), Equals(Variable("y"),Constant(3))),
      Loop(Variable("y"),Block(Equals(Variable("x"),Plus(Variable("x"),Variable("x"))),
        Equals(Variable("y"),Minus(Variable("y"),Constant(1))))), Variable("x"))))
  val complexString1 = "((1 + 2) - (-3 * 4))%5;"
  val complexString2 = "if(2+2=5){((1 + 2) - (3 * 4)) / 5; } else {{x=2;y=3;}while (y){x=x+x;y=y-1;} x;}"

}
