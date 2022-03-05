package lexer

abstract class Token
case class Number(n: Float) extends Token
case class Plus() extends Token
case class Minus() extends Token
case class Multiply() extends Token
case class Divide() extends Token
case class EOF() extends Token