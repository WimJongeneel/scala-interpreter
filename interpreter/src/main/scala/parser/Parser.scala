package parser

import lexer._

def tryParseFunctionCall(state: ParserState, current: Expression) = {
  var result = current

  while state.currentToken() == LP() do
    ensurePopToken[LP](state)
    result = FunctionCall(result, parseExpression(state))
    ensurePopToken[RP](state)

  result
}

def parsePrefix(state: ParserState): Expression = state.currentToken() match {
  case Number(n) => Literal(n)
  case Id(i) => {
    if state.peekToken() == Arrow() then 
      ensurePopToken[Id](state)
      ensurePopToken[Arrow](state)
      Function(i, parseExpression(state))
    else Reference(i)
  }
  case If() => {
    ensurePopToken[If](state)
    val cond = parseExpression(state)
    ensurePopToken[Then](state)
    val ifTrue = parseExpression(state)
    ensurePopToken[Else](state)
    val ifFalse = parseExpression(state)
    IfThenElse(cond, ifTrue, ifFalse)
  }
  case LB() => {
    ensurePopToken[LB](state)
    var statements = List[AST]()
    while(state.currentToken() != RB())
      statements = statements.appended(parseStatement(state))
    CodeBlock(statements)

  }
  case LP() => {
    ensurePopToken[LP](state)
    parseExpression(state)
  }
  case Not() => {
    ensurePopToken[Not](state)
    UnaryOperator("!", parseExpression(state))
  }
  case Minus() => {
    ensurePopToken[Minus](state)
    UnaryOperator("-", parseExpression(state))
  }
  case t: Token => throw new Exception("Invalid prefix expression: " + t.getClass().getName())
}

def infixConstructor(token:Token): Option[(left: Expression, rigth: Expression) => Expression] = token match {
  case Plus()      => Some((left, rigth) => BinaryOperator(left, "+", rigth))
  case Minus()     => Some((left, rigth) => BinaryOperator(left, "-", rigth))
  case Multiply()  => Some((left, rigth) => BinaryOperator(left, "*", rigth))
  case Divide()    => Some((left, rigth) => BinaryOperator(left, "/", rigth))
  case And()       => Some((left, rigth) => BinaryOperator(left, "&", rigth))
  case Or()        => Some((left, rigth) => BinaryOperator(left, "|", rigth))
  case Smaller()   => Some((left, rigth) => BinaryOperator(left, "<", rigth))
  case Larger()    => Some((left, rigth) => BinaryOperator(left, ">", rigth))
  case Equals()    => Some((left, rigth) => BinaryOperator(left, "=", rigth))
  case NotEquals() => Some((left, rigth) => BinaryOperator(left, "!=", rigth))
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
      case None           => return tryParseFunctionCall(state, left)
      case Some(newLeft)  => left = newLeft 
    }
  }

  tryParseFunctionCall(state, left)
}

def parseStatement(state: ParserState): AST = {
  val statement = state.currentToken() match {
    case Let() => {
      ensurePopToken[Let](state)
      val Id(name) = ensurePopToken[Id](state)
      ensurePopToken[Assign](state)
      Declaration(name, parseExpression(state))
    }
    case Print() => {
      ensurePopToken[Print](state)
      PrintExpression(parseExpression(state))
    }
    case While() => {
      ensurePopToken[While](state)
      val cond = parseExpression(state)
      ensurePopToken[Do](state)
      val body = parseExpression(state)
      WhileLoop(cond, body)
    }
    case _ => ExpressionStatement(parseExpression(state))
  }

  ensurePopToken[SemiColon](state)

  statement
}

def parse(state: ParserState): List[AST] = {
  var statements = List[AST]()
  while(state.currentToken() != EOF())
    statements = statements.appended(parseStatement(state))
  statements
}

def ensurePopToken[T <: Token](state: ParserState): T = {
  val token = state.currentToken()
  state.moveNext()
  token match {
    case t: T => t
    case _    => throw new Error("invalid token")
  }
}