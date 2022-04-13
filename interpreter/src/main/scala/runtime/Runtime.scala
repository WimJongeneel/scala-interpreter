package runtime

import utils.State
import parser._

type MemoryValue = Float
type Memory = Map[String, MemoryValue]

val RUNTIME_TRUE = 1.0f
val RUNTIME_FAlSE = 0.0f
val RUNTIME_NULL = RUNTIME_FAlSE

def toRuntimeBoolean(b: Boolean): MemoryValue = if b then RUNTIME_TRUE else RUNTIME_FAlSE

def runExpression(e: Expression): State[Memory, MemoryValue] = e match {
  case Literal(n) => State.unit(n)
  case BinaryOperator(l, "+", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl + vr))
  case BinaryOperator(l, "-", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl - vr))
  case BinaryOperator(l, "*", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl * vr))
  case BinaryOperator(l, "/", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl / vr))
  case BinaryOperator(l, "&", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl == RUNTIME_TRUE && vr == RUNTIME_TRUE).map(toRuntimeBoolean))
  case BinaryOperator(l, "|", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl == RUNTIME_TRUE || vr == RUNTIME_TRUE).map(toRuntimeBoolean))
  case BinaryOperator(l, "<", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl < vr).map(toRuntimeBoolean))
  case BinaryOperator(l, ">", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl > vr).map(toRuntimeBoolean))
  case BinaryOperator(l, "=", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl == vr).map(toRuntimeBoolean))
  case BinaryOperator(l, "!=", r)   => runExpression(l).bind(vl => runExpression(r).map(vr => vl != vr).map(toRuntimeBoolean))
  case UnaryOperator("!", e)        => runExpression(e).map(v => if v == RUNTIME_TRUE then RUNTIME_TRUE else RUNTIME_FAlSE)
  case UnaryOperator("-", e)        => runExpression(e).map(v => v * -1)
  case IfThenElse(c, t, f)          => runExpression(c).bind(v => if v == RUNTIME_TRUE then runExpression(t) else runExpression(f))
  case Reference(r)                 => State(memory => (memory, memory(r)))
  // todo: scoping
  case CodeBlock(s)                 => s.foldLeft(State.unit(RUNTIME_NULL))((s, e) => s.after(runStatement(e)))
}

def runStatement(e: AST): State[Memory, MemoryValue] = e match {
  case Declaration(name, expression) => runExpression(expression).bind(v => State(m => (m updated (name, v), RUNTIME_NULL)))
  case ExpressionStatement(expression) => runExpression(expression)
  case PrintExpression(expression) => State(memory => {
    val (memory1, a) = runExpression(expression).run(memory)
    println(a)
    (memory1, RUNTIME_NULL)
  })
  // case WhileLoop(cond, body) => {

  //   state.unit(FAlSE).
  //   var result = FALSE
  //   while(runExpression(cond) == 1) result = runExpression(body)
  //   result
  // }
}

def run(ast: List[AST]): MemoryValue = 
  val emptyState = State.unit[Memory, MemoryValue](RUNTIME_NULL)

  ast.foldLeft(emptyState)((s, e) => s.after(runStatement(e)))
    .eval(Map.empty)