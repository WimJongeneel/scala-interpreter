package parser

abstract class Expression
case class Literal(n: Float) extends Expression
case class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression
case class Reference(name: String) extends Expression

abstract class AST
case class Declaration(name: String, expr: Expression) extends AST
case class Print(expr: Expression) extends AST