package runtime

import utils.State
import parser._

object Runtime {

  def runExpression(e: Expression): State[Memory, MemoryValue] = e match {
    case Literal(n)                   => n match {
                                          case f: Float => State.unit(Number(f))
                                          case true     => State.unit(MemoryValue.runtimeTrue)
                                          case false    => State.unit(MemoryValue.runtimeFalse)
                                          case _        => State.unit(MemoryValue.runtimeNull)
    }
    case BinaryOperator(l, "+", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => Number(vl.toFloat.value + vr.toFloat.value)))
    case BinaryOperator(l, "-", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => Number(vl.toFloat.value - vr.toFloat.value)))
    case BinaryOperator(l, "*", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => Number(vl.toFloat.value * vr.toFloat.value)))
    case BinaryOperator(l, "/", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => Number(vl.toFloat.value / vr.toFloat.value)))
    case BinaryOperator(l, "&", r)    => runExpression(l).bind(
                                          vl => runExpression(r).map(vr => if vl.toBool.value && vr.toBool.value then MemoryValue.runtimeTrue else MemoryValue.runtimeFalse)
                                        )
    case BinaryOperator(l, "|", r)    => runExpression(l).bind(
                                          vl => runExpression(r).map(vr => if vl.toBool.value || vr.toBool.value then MemoryValue.runtimeTrue else MemoryValue.runtimeFalse)
                                        )
    case BinaryOperator(l, "<", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => Bool(vl.toBool.value < vr.toBool.value)))
    case BinaryOperator(l, ">", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => Bool(vl.toBool.value > vr.toBool.value)))
    case BinaryOperator(l, "=", r)    => runExpression(l).bind(vl => runExpression(r).map(vr => Bool(vl.toBool.value == vr.toBool.value)))
    case BinaryOperator(l, "!=", r)   => runExpression(l).bind(vl => runExpression(r).map(vr => Bool(vl.toBool.value != vr.toBool.value)))
    case UnaryOperator("!", e)        => runExpression(e).map(v => if v.toBool.value then MemoryValue.runtimeFalse else MemoryValue.runtimeTrue)
    case UnaryOperator("-", e)        => runExpression(e).map(v => Number(v.toFloat.value * -1))
    case IfThenElse(c, t, f)          => runExpression(c).bind(v => if v.toBool.value then runExpression(t) else runExpression(f))
    case Reference(r)                 => State(memory => (memory, memory.read(r)))
    case CodeBlock(s)                 => s.foldLeft(State.unit[Memory, MemoryValue](MemoryValue.runtimeNull).modify(m => m.addFrame))
                                          ((s, e) => s.after(runStatement(e)))
                                          .modify(m => m.dropFrame)
    case FunctionDefinition(a, b)     => {
                                          // TODO: extract closure
                                          val closure = Map.empty[String, MemoryValue]
                                          State.unit(Function(a, b, closure))
                                        }
    case FunctionCall(e, p)           => runExpression(e).bind(f => runExpression(p).bind(pv => {
                                          val Function(a, b, c) = f.toFunction
                                          val m0 = Memory(List(c)).addFrame.declare(a, pv).addFrame
                                          val (m1, r) = runExpression(b).run(m0)
                                          // TODO: process mutations to captured variables in the closure
                                          State.unit(r)
                                        }))
  }

  def runStatement(e: AST): State[Memory, MemoryValue] = e match {
    case Declaration(name, expression) => runExpression(expression).bind(v => State(m => (m.declare(name, v), MemoryValue.runtimeNull)))
    case AssignStatement(name, expression) => runExpression(expression).bind(v => State(m => (m.assign(name, v), MemoryValue.runtimeNull)))
    case ExpressionStatement(expression) => runExpression(expression)
    case PrintExpression(expression) => State(memory => {
      val (memory1, a) = runExpression(expression).run(memory)
      println(a match {
        case Number(n)  => n
        case Bool(b)    => b
        case Null()     => "null"
        case _          => "<function>"
      })
      (memory1, MemoryValue.runtimeNull)
    })
    // case WhileLoop(cond, body) => {

    //   state.unit(FAlSE).
    //   var result = FALSE
    //   while(runExpression(cond) == 1) result = runExpression(body)
    //   result
    // }
  }

  def run(ast: List[AST]): MemoryValue = 
    val emptyState = State.unit[Memory, MemoryValue](MemoryValue.runtimeNull)

    ast.foldLeft(emptyState)((s, e) => s.after(runStatement(e)))
      .eval(Memory.empty)
}