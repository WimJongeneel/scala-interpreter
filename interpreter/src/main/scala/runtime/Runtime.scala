package runtime

import utils.State
import parser._

object Runtime {

  def runExpression(e: Expression): State[Memory, MemoryValue] = e match {
    case Literal(n) => State.unit(n)
    case BinaryOperator(l, "+", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl + vr))
    case BinaryOperator(l, "-", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl - vr))
    case BinaryOperator(l, "*", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl * vr))
    case BinaryOperator(l, "/", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl / vr))
    case BinaryOperator(l, "&", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl == Memory.RUNTIME_TRUE && vr == Memory.RUNTIME_TRUE).map(toRuntimeBoolean))
    case BinaryOperator(l, "|", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl == Memory.RUNTIME_TRUE || vr == Memory.RUNTIME_TRUE).map(toRuntimeBoolean))
    case BinaryOperator(l, "<", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl < vr).map(toRuntimeBoolean))
    case BinaryOperator(l, ">", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl > vr).map(toRuntimeBoolean))
    case BinaryOperator(l, "=", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => vl == vr).map(toRuntimeBoolean))
    case BinaryOperator(l, "!=", r)   => runExpression(l).bind(vl => runExpression(r).map(vr => vl != vr).map(toRuntimeBoolean))
    case UnaryOperator("!", e)        => runExpression(e).map(v => if v == Memory.RUNTIME_TRUE then Memory.RUNTIME_TRUE else Memory.RUNTIME_FAlSE)
    case UnaryOperator("-", e)        => runExpression(e).map(v => v * -1)
    case IfThenElse(c, t, f)          => runExpression(c).bind(v => if v == Memory.RUNTIME_TRUE then runExpression(t) else runExpression(f))
    case Reference(r)                 => State(memory => (memory, memory.read(r)))
    case CodeBlock(s)                 => s.foldLeft(State.unit[Memory, MemoryValue](Memory.RUNTIME_NULL).modify(m => m.addFrame))
                                          ((s, e) => s.after(runStatement(e)))
                                          .modify(m => m.dropFrame)
  }

  def runStatement(e: AST): State[Memory, MemoryValue] = e match {
    case Declaration(name, expression) => runExpression(expression).bind(v => State(m => (m.declare(name, v), Memory.RUNTIME_NULL)))
    case AssignStatement(name, expression) => runExpression(expression).bind(v => State(m => (m.assign(name, v), Memory.RUNTIME_NULL)))
    case ExpressionStatement(expression) => runExpression(expression)
    case PrintExpression(expression) => State(memory => {
      val (memory1, a) = runExpression(expression).run(memory)
      println(a)
      (memory1, Memory.RUNTIME_NULL)
    })
    // case WhileLoop(cond, body) => {

    //   state.unit(FAlSE).
    //   var result = FALSE
    //   while(runExpression(cond) == 1) result = runExpression(body)
    //   result
    // }
  }

  def run(ast: List[AST]): MemoryValue = 
    val emptyState = State.unit[Memory, MemoryValue](Memory.RUNTIME_NULL)

    ast.foldLeft(emptyState)((s, e) => s.after(runStatement(e)))
      .eval(Memory.empty)

  private def toRuntimeBoolean(b: Boolean): MemoryValue = if b then Memory.RUNTIME_TRUE else Memory.RUNTIME_FAlSE
}