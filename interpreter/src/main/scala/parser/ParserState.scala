package parser

import scala.collection.mutable.ArrayBuffer
import lexer._

class ParserState(val tokens: ArrayBuffer[Token], val position: Int = 0) {
  var _position = position

  def peekToken() = tokens(_position + 1)

  def currentToken(): Token = tokens(_position)

  def moveNext() = _position = _position + 1

  def currentPrecedence() = tokenPrecedence(tokens(_position))

  def peekPrecedence() = tokenPrecedence(tokens(_position + 1))
}

def tokenPrecedence(token: Token) = token match {
  case Plus()       => 40
  case Minus()      => 40
  case Multiply()   => 50
  case Divide()     => 50
  case If()         => 60
  case Then()       => 60
  case Else()       => 60
  case LP()         => 70
  case RP()         => 70
  case SemiColon()  => 80
  case Do()         => 60
  case And()        => 10
  case Or()         => 10
  case Larger()     => 30
  case Smaller()    => 30
  case Equals()     => 20
  case NotEquals()  => 20
  case _            => 0
}