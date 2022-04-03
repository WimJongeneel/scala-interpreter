package runtime

import parser._

var memory: Map[String, Float] = Map.empty

def runExpression(e: Expression): Float = e match {
  case Literal(n) => n
  case BinaryOperator(l, "+", r)    => runExpression(l) + runExpression(r)
  case BinaryOperator(l, "-", r)    => runExpression(l) - runExpression(r)
  case BinaryOperator(l, "*", r)    => runExpression(l) * runExpression(r)
  case BinaryOperator(l, "/", r)    => runExpression(l) / runExpression(r)
  case BinaryOperator(l, "&", r)    => if runExpression(l) == 1 && runExpression(r) == 1 then 1 else 0
  case BinaryOperator(l, "|", r)    => if runExpression(l) == 1 || runExpression(r) == 1 then 1 else 0
  case BinaryOperator(l, "<", r)    => if runExpression(l) < runExpression(r) then 1 else 0
  case BinaryOperator(l, ">", r)    => if runExpression(l) > runExpression(r) then 1 else 0
  case BinaryOperator(l, "=", r)    => if runExpression(l) == runExpression(r) then 1 else 0
  case BinaryOperator(l, "!=", r)   => if runExpression(l) != runExpression(r) then 1 else 0
  case UnaryOperator("!", e)        => if runExpression(e) == 1 then 0 else 1
  case UnaryOperator("-", e)        => -1 * runExpression(e) 
  case IfThenElse(c, t, f)          => if runExpression(c) == 1 then runExpression(t) else runExpression(f)
  case Reference(r)                 => memory(r)
  case CodeBlock(s)                 => s.foldLeft(.0f)((_, e) => runStatement(e))
}

def runStatement(e: AST): Float = e match {
  case Declaration(name, expression) => {
    memory = memory updated (name, runExpression(expression))
    0
  }
  case ExpressionStatement(expression) => runExpression(expression)
  case PrintExpression(expression) => {
    println(runExpression(expression))
    0
  }
  case WhileLoop(cond, body) => {
    var result = .0f
    while(runExpression(cond) == 1) result = runExpression(body)
    result
  }
}

def run(ast: List[AST]): Float = ast.foldLeft(.0f)((_, e) => runStatement(e))