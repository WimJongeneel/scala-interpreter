package parser

abstract class Expression
case class Literal(n: Float) extends Expression
case class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression