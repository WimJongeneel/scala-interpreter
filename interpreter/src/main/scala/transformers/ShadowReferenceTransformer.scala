package transformers

import parser._
import utils._

object ShadowReferenceTransformer extends Transformer {

    private type RewriteRules = List[Map[String, String]]

    protected override type TransformerState = RewriteRules

    protected override def emptyState: TransformerState = List(Map.empty)

    private def addRewriteRule(rules: RewriteRules, from: String, to: String) =
        rules.updated(0, rules(0).updated(from, to))

    private def addRewriteRuleFor(rules: RewriteRules, from: String) = 
        val current = getRewrite(rules, from)
        addRewriteRule(rules, from, current + "_")

    private def getRewrite(rules: RewriteRules, from: String) = rules
        .find(f => f.contains(from))
        .map(f => f(from))
        .getOrElse(from)

    protected override def tranformExpression(e: Expression): State[RewriteRules, Expression] = e.match {
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
        case _ => super.tranformExpression(e)
    }

    protected override def tranformStatement(s: AST): State[RewriteRules, AST] = s.match {
        case Declaration(n, e) => State(r0 => {
            val r1 = addRewriteRuleFor(r0, n)
            val (r2, e1) = tranformExpression(e).run(r1)
            (r2, Declaration(getRewrite(r2, n), e1))
        })
        case _ => super.tranformStatement(s)
    }
}