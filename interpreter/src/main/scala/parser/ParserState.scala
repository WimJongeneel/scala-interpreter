package parser

import scala.collection.mutable.ArrayBuffer
import lexer.Token

class ParserState(val tokens: ArrayBuffer[Token], val position: Int = 0) {
  var _position = position

  def peekToken() = tokens(_position + 1)

  def currentToken(): Token = tokens(_position)

  def moveNext() = _position = _position + 1

  def currentPrecedence() = tokenPrecedence(tokens(_position))

  def peekPrecedence() = tokenPrecedence(tokens(_position + 1))
}