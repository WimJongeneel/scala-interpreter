package transformers

import parser._
import utils._

object ClosureTransformer extends Transformer {

    // TODO: find better way of composing statements
    def transform(a: List[AST]) = {
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

    type LocalVars = List[Set[String]]

    def emptyState: (LocalVars, Set[String]) = (List(Set()), Set())

    private def contains(l: LocalVars, n: String) = l.exists(p => p.contains(n))

    private def tranformExpression(e: Expression): State[(LocalVars, Set[String]), Expression] = e.match {
        case Reference(n) => State(m => if contains(m._1, n) then (m, e) else ((m._1, m._2 + n), e))
        case BinaryOperator(l, o, r) => tranformExpression(l).bind(l => tranformExpression(r).map(r => BinaryOperator(l, o, r)))
        case UnaryOperator(o, e) => tranformExpression(e).map(e => UnaryOperator(o, e))
        case FunctionDefinition(p, b, _) => State(m => {
                                                val inner: (LocalVars, Set[String]) = (List(Set(p)), Set())
                                                val (l: (LocalVars, Set[String]), b1: Expression) = tranformExpression(b).run(inner)
                                                (m, FunctionDefinition(p, b1, l._2))
                                            })
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

    private def tranformStatement(s: AST): State[(LocalVars, Set[String]), AST] = s.match {
        case Declaration(n, e) => tranformExpression(e).bind(e => State(m => ((m._1.updated(0, m._1(0) + n), m._2 ), Declaration(n, e))))
        case ExpressionStatement(e) => tranformExpression(e).map(e => ExpressionStatement(e))
        case PrintExpression(e) => tranformExpression(e).map(e => PrintExpression(e))
        // TODO: case While
        case AssignStatement(n, e) => tranformExpression(e).map(e => AssignStatement(n, e))
    }
}