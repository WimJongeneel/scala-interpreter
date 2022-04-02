package runtime

import parser._

var memory: Map[String, Float] = Map.empty

def runExpression(e: Expression): Float = e match {
  case Literal(n) => n
  case BinaryOperator(l, "+", r) => runExpression(l) + runExpression(r)
  case BinaryOperator(l, "-", r) => runExpression(l) - runExpression(r)
  case BinaryOperator(l, "*", r) => runExpression(l) * runExpression(r)
  case BinaryOperator(l, "/", r) => runExpression(l) / runExpression(r)
  case IfThenElse(c, t, f)       => if runExpression(c) == 1 then runExpression(t) else runExpression(f)
  case Reference(r)              => memory(r)
}

def run(e: AST): Unit = e match {
  case Declaration(name, expression) => memory = memory updated (name, runExpression(expression))
  case Print(expression) => println(runExpression(expression))
}