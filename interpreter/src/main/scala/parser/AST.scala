package parser

abstract class Expression
case class Literal(n: Float) extends Expression
case class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression
case class UnaryOperator(operator: String, expression: Expression) extends Expression
case class Reference(name: String) extends Expression
case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression
case class CodeBlock(statements: List[AST]) extends Expression

abstract class AST
case class Declaration(name: String, expr: Expression) extends AST
case class ExpressionStatement(expr: Expression) extends AST
case class PrintExpression(expr: Expression) extends AST
case class WhileLoop(cond: Expression, body: Expression) extends AST