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
  case Plus()       => 20
  case Minus()      => 20
  case Multiply()   => 30
  case Divide()     => 30
  case If()         => 100
  case Then()       => 100
  case Else()       => 100
  case LP()         => 110
  case RP()         => 110
  case SemiColon()  => 999
  case Do()         => 999
  case And()        => 70
  case Or()         => 70
  case Larger()     => 90
  case Smaller()    => 90
  case Equals()     => 80
  case NotEquals()  => 80
  case _            => 0
}