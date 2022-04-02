package parser

import lexer._

def parsePrefix(state: ParserState) :Expression = state.currentToken() match {
  case Number(n) => Literal(n)
  case Id(i)     => Reference(i)
  case If()      => {
    ensurePopToken[If](state)
    val cond = parseExpression(state)
    ensurePopToken[Then](state)
    val ifTrue = parseExpression(state)
    ensurePopToken[Else](state)
    val ifFalse = parseExpression(state)
    IfThenElse(cond, ifTrue, ifFalse)
  }
  case LP() => {
    ensurePopToken[LP](state)
    parseExpression(state)
  }
  case t: Token   => throw new Exception("Invalid prefix expression: " + t.getClass().getName())
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
    val right = parseExpression(state, precedence)
    f(left, right)
  })

def parseExpression(state: ParserState, precedence: Int = 0): Expression = {
  
  var left = parsePrefix(state)

  while(state.peekToken() != EOF() && precedence < state.peekPrecedence()) {
    state.moveNext()

    parseInfix(state, left) match {
      case None           => return left
      case Some(newLeft)  => left = newLeft 
    }
  }

  left
}

def parseStatement(state: ParserState): AST = {
  state.currentToken() match {
    case Let() => {
      ensurePopToken[Let](state)
      val Id(name) = ensurePopToken[Id](state)
      ensurePopToken[Equals](state)
      Declaration(name, parseExpression(state))
    }
    case _ => Print(parseExpression(state))
  }
}

def ensurePopToken[T <: Token](state: ParserState): T = {
  val token = state.currentToken()
  state.moveNext()
  token match {
    case t: T => t
    case _    => throw new Error("invalid token")
  }
}