package transformers

import parser._
import utils._

object LiteralOptimizerTransformer extends Transformer {

    protected override type TransformerState = Unit

    protected override def emptyState: TransformerState = ()

    protected override def tranformExpression(e: Expression): State[Unit, Expression] = e.match {
        case BinaryOperator(l, o, r) => tranformExpression(l).bind(l => tranformExpression(r).map(r => (l, o, r) match {
            case (Literal(l: Float), "+", Literal(r: Float)) => Literal(l + r)
            case (Literal(l: Float), "*", Literal(r: Float)) => Literal(l * r)
            case (Literal(l: Float), "-", Literal(r: Float)) => Literal(l - r)
            case (Literal(l: Float), "/", Literal(r: Float)) => Literal(l / r)
            case (Literal(l: Boolean), "&", Literal(r: Boolean)) => Literal(l && r)
            case (Literal(l: Boolean), "|", Literal(r: Boolean)) => Literal(l || r)
            case _ => e
        }))
        case UnaryOperator(o, r) => tranformExpression(r).map(r => (o, r) match {
            case ("-", Literal(n: Float)) => Literal(-1 * n)
            case ("!", Literal(b: Boolean)) => Literal(!b)
            case _ => e
        })
        case _ => super.tranformExpression(e)
    }
}