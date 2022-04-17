package transformers

import parser._
import utils._

object ClosureTransformer extends Transformer {

    type LocalVars = List[Set[String]]

    protected override def emptyState: (LocalVars, Set[String]) = (List(Set()), Set())

    private def contains(l: LocalVars, n: String) = l.exists(p => p.contains(n))

    protected override type TransformerState = (LocalVars, Set[String])

    protected override def tranformExpression(e: Expression): State[(LocalVars, Set[String]), Expression] = e.match {
        case Reference(n) => State(m => if contains(m._1, n) then (m, e) else ((m._1, m._2 + n), e))
        case FunctionDefinition(p, b, _) => State(m => {
                                                val inner: (LocalVars, Set[String]) = (List(Set(p)), Set())
                                                val (l: (LocalVars, Set[String]), b1: Expression) = tranformExpression(b).run(inner)
                                                (m, FunctionDefinition(p, b1, l._2))
                                            })
        case _ => super.tranformExpression(e)
    }

    protected override def tranformStatement(s: AST): State[(LocalVars, Set[String]), AST] = s.match {
        case Declaration(n, e) => tranformExpression(e).bind(e => State(m => ((m._1.updated(0, m._1(0) + n), m._2 ), Declaration(n, e))))
        case _ => super.tranformStatement(s)
    }
}