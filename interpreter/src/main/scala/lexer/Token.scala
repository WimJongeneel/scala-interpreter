package lexer

abstract class Token
case class Number(n: Float) extends Token
case class Plus() extends Token
case class Minus() extends Token
case class Multiply() extends Token
case class Divide() extends Token
case class Let() extends Token
case class Equals() extends Token
case class Id(name: String) extends Token
case class SemiColon() extends Token
case class EOF() extends Token
case class If() extends Token
case class Then() extends Token
case class Else() extends Token
case class LP() extends Token
case class RP() extends Token
case class LB() extends Token
case class RB() extends Token
case class Print() extends Token