package transformers

import parser._
import utils._

object ShadowReferenceTransformer extends Transformer {

    def transform(a: List[AST]) = {
        if a.isEmpty then return a

        var state: RewriteRules = List(Map())
        var res = List[AST]()

        a.foreach(a => {
            val (s1, newA) = tranformStatement(a).run(state)
            state = s1
            res = res.appended(newA)
        })

        res
    }

    type RewriteRules = List[Map[String, String]]

    private def addRewriteRule(rules: RewriteRules, from: String, to: String) =
        rules.updated(0, rules(0).updated(from, to))

    private def addRewriteRuleFor(rules: RewriteRules, from: String) = 
        val current = getRewrite(rules, from)
        addRewriteRule(rules, from, current + "_")

    private def getRewrite(rules: RewriteRules, from: String) = rules
        .find(f => f.contains(from))
        .map(f => f(from))
        .getOrElse(from)

    // override
    private def tranformExpression(e: Expression): State[RewriteRules, Expression] = e.match {
        case Reference(n) => State(r => (r, Reference(getRewrite(r, n))))
        case CodeBlock(c0) => State(r0 => {
            var state: RewriteRules = r0.prepended(Map.empty)
            var res = List[AST]()

            c0.foreach(a => {
                val (s1, newA) = tranformStatement(a).run(state)
                state = s1
                res = res.appended(newA)
            })

            (r0, CodeBlock(res))
        })
        case FunctionDefinition(a, b0, c) => State(r0 => {
            val r1 = r0.prepended(Map.empty)
            val r2 = addRewriteRuleFor(r1, a)
            val (r3, b1) = tranformExpression(b0).run(r2)
            val c1 = c.map(n => getRewrite(r0, n))
            (r0, FunctionDefinition(getRewrite(r3, a), b1, c1))
        })
        case BinaryOperator(l, o, r) => tranformExpression(l).bind(l => tranformExpression(r).map(r => BinaryOperator(l, o, r)))
        case UnaryOperator(o, e) => tranformExpression(e).map(e => UnaryOperator(o, e))
        case IfThenElse(c, t, f) => tranformExpression(c).bind(c => tranformExpression(t).bind(t => tranformExpression(f).map(f => IfThenElse(c, t, f))))
        case FunctionCall(f, a) => tranformExpression(f).bind(f => tranformExpression(a).map(a => FunctionCall(f, a)))
        case Literal(n) => State.unit(e)
    }

    private def tranformStatement(s: AST): State[RewriteRules, AST] = s.match {
        case Declaration(n, e) => State(r0 => {
            val r1 = addRewriteRuleFor(r0, n)
            val (r2, e1) = tranformExpression(e).run(r1)
            (r2, Declaration(getRewrite(r2, n), e1))
        })
        case ExpressionStatement(e) => tranformExpression(e).map(e => ExpressionStatement(e))
        case PrintExpression(e) => tranformExpression(e).map(e => PrintExpression(e))
        // TODO: case While
        case AssignStatement(n, e) => tranformExpression(e).map(e => AssignStatement(n, e))
    }
}