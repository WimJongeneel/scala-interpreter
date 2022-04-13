package parser

abstract class Expression
case class Literal(n: Float) extends Expression
case class BinaryOperator(left: Expression, operator: String, right: Expression) extends Expression
case class UnaryOperator(operator: String, expression: Expression) extends Expression
case class Reference(name: String) extends Expression
case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression
case class CodeBlock(statements: List[AST]) extends Expression
case class Function(parameter: String, body: Expression) extends Expression
case class FunctionCall(function: Expression, argument: Expression) extends Expression

abstract class AST
case class Declaration(name: String, expr: Expression) extends AST
case class ExpressionStatement(expr: Expression) extends AST
case class PrintExpression(expr: Expression) extends AST
case class WhileLoop(cond: Expression, body: Expression) extends AST

def show(e: Expression, tabs: String = "\t"): String = {
    e match {
        case Literal(n) => n.toString
        case UnaryOperator(o, e) => s"UnaryOperator(\n$tabs" + o + s"\n$tabs" + show(e, tabs + "\t") + "\n)"
        case BinaryOperator(l, o, r) => s"BinaryOperator(\n$tabs" + show(l, tabs + "\t") + s"\n$tabs" + o + s"\n$tabs" + show(r, tabs + "\t") + "\n)"
        case Reference(n) => "Reference("+n+")"
        case IfThenElse(c, t, f) => s"IfThenElse(\n$tabs" + show(c, tabs + "\t") + s"\n$tabs" + show(t, tabs + "\t") + s"\n$tabs" + show(f, tabs + "\t") + "\n)"
        case CodeBlock(e) => "CodeBlock(" + e.foldLeft(tabs)((r, e) => r + "\n" + tabs + showAST(e)) + "\n)"
        case Function(a, b) => s"Function(\n$tabs" + a + s"\n$tabs" + show(b, tabs + "\t") + "\n)"
        case FunctionCall(b, a) => s"FunctionCall(\n$tabs" + show(b, tabs + "\t") + s"\n$tabs" + show(a, tabs + "\t") + "\n)"
    }
}

def showAST(e: AST, tabs: String = "\t"): String = {
    e match {
        case Declaration(n, e) => s"Declaration(\n$tabs" + n + s"\n$tabs" + show(e, tabs + "\t") + "\n)"
        case ExpressionStatement(e) => "Expression(" + show(e, tabs + "\t") + ")"
        case PrintExpression(e) => "Print(" + show(e, tabs + "\t") + ")"
        case WhileLoop(c, b) => s"WhileLoop(\n$tabs" + show(c, tabs + "\t") + s",\n$tabs" + show(b, tabs + "\t") + "\n)"
    }
}
