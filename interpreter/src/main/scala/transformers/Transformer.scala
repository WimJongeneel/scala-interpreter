package transformers

import parser._
import utils._

abstract class Transformer {

    // TODO: find better way of composing statements
    def transform(a: List[AST]): List[AST] = {
        if a.isEmpty then return a

        var state = emptyState
        var res = List[AST]()

        a.foreach(a => {
            val (s1, newA) = tranformStatement(a).run(state)
            state = s1
            res = res.appended(newA)
        })

        res
    }

    protected type TransformerState

    protected def emptyState: TransformerState

    protected def tranformExpression(e: Expression): State[TransformerState, Expression] = e.match {
        case Reference(n) => State.unit(e)
        case BinaryOperator(l, o, r) => tranformExpression(l).bind(l => tranformExpression(r).map(r => BinaryOperator(l, o, r)))
        case UnaryOperator(o, e) => tranformExpression(e).map(e => UnaryOperator(o, e))
        case FunctionDefinition(p, b, c) => tranformExpression(b).map(b => FunctionDefinition(p, b, c))
        case Literal(_) => State.unit(e)
        case IfThenElse(c, t, f) => tranformExpression(c).bind(c => tranformExpression(t).bind(t => tranformExpression(f).map(f => IfThenElse(c, t, f))))
        case CodeBlock(c) => State(s0 => {
            if c.isEmpty then CodeBlock(c)

            var state = s0
            var res = List[AST]()

            c.foreach(a => {
                val (s1, newA) = tranformStatement(a).run(state)
                state = s1
                res = res.appended(newA)
            })

            (state, CodeBlock(res))
        })
        case FunctionCall(f, a) => tranformExpression(f).bind(f => tranformExpression(a).map(a => FunctionCall(f, a)))
    }

    protected def tranformStatement(s: AST): State[TransformerState, AST] = s.match {
        case Declaration(n, e) => tranformExpression(e).map(e => Declaration(n, e))
        case ExpressionStatement(e) => tranformExpression(e).map(e => ExpressionStatement(e))
        case PrintExpression(e) => tranformExpression(e).map(e => PrintExpression(e))
        case AssignStatement(n, e) => tranformExpression(e).map(e => AssignStatement(n, e))
    }
}