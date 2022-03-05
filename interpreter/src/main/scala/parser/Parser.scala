package parser

import lexer._

def tokenPrecedence(token: Token) = token match {
  case Plus()     => 20
  case Minus()    => 20
  case Multiply() => 30
  case Divide()   => 30
  case _          => 0
}

def parsePrefix(token :Token) :Expression = token match {
  case Number(n) => Literal(n)
  case _ => throw new Exception("Invalid prefix expression: " + token.getClass().getName())
}

def infixConstructor(token:Token) :Option[(left: Expression, rigth: Expression) => Expression] = token match {
  case Plus()      => Some((left, rigth) => BinaryOperator(left, "+", rigth))
  case Minus()     => Some((left, rigth) => BinaryOperator(left, "-", rigth))
  case Multiply()  => Some((left, rigth) => BinaryOperator(left, "*", rigth))
  case Divide()    => Some((left, rigth) => BinaryOperator(left, "/", rigth))
  case _           => None
}

def parseInfix(state: ParserState, left: Expression): Option[Expression] = infixConstructor(state.currentToken())
  .map(f => {
    val precedence = state.currentPrecedence()
    state.moveNext()
    val right = parse(state, precedence)
    f(left, right)
  })

def parse(state: ParserState, precedence: Int = 0): Expression = {
  
  var left = parsePrefix(state.currentToken())

  while(state.peekToken() != EOF() && precedence < state.peekPrecedence()) {
    state.moveNext()

    parseInfix(state, left) match {
      case None           => return left
      case Some(newLeft)  => left = newLeft 
    }
  }

  left
}