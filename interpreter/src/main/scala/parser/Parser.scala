package parser

import scala.reflect._

import lexer._

private def tryParseFunctionCall(state: ParserState, current: Expression) = {
  var result = current

  while state.currentToken() == LP() do
    ensurePopToken[LP](state)
    result = FunctionCall(result, parseExpression(state))
    ensurePopToken[RP](state)

  result
}

private def parsePrefix(state: ParserState): Expression = state.currentToken() match {
  case Number(n) => {
    val t = Literal(n)
    state.moveNext()
    t
  }
  case True() => {
    val t = Literal(true)
    state.moveNext()
    t
  }
  case False() => {
    val t = Literal(false)
    state.moveNext()
    t
  }
  case Null() => {
    val t = Literal(null)
    state.moveNext()
    t
  }
  case Id(i) => {
    if state.peekToken() == Arrow() then 
      ensurePopToken[Id](state)
      ensurePopToken[Arrow](state)
      FunctionDefinition(i, parseExpression(state), Set.empty)
    else 
      val t = Reference(i)
      state.moveNext()
      t
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

    ensurePopToken[RB](state)
    CodeBlock(statements)
  }
  case LP() => {
    ensurePopToken[LP](state)
    val t = parseExpression(state)
    ensurePopToken[RP](state)
    t
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

private def infixConstructor(token:Token): Option[(left: Expression, rigth: Expression) => Expression] = token match {
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

private def parseInfix(state: ParserState, left: Expression): Option[Expression] = infixConstructor(state.currentToken())
  .map(f => {
    val precedence = state.currentPrecedence()
    state.moveNext()
    val right = parseExpression(state, precedence)
    f(left, right)
  })

def parseExpression(state: ParserState, precedence: Int = 0): Expression = {

  var left = parsePrefix(state)

  while(state.currentToken() != EOF() && precedence < state.currentPrecedence()) {
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
    case Id(n) if state.peekToken() == Assign() => {
      ensurePopToken[Id](state)
      ensurePopToken[Assign](state)
      AssignStatement(n, parseExpression(state))
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

private def ensurePopToken[T <: Token](state: ParserState)(implicit tag: ClassTag[T]): T = {
  val token = state.currentToken()
  state.moveNext()
  if token.getClass.getName == tag.runtimeClass.getName 
  then token.asInstanceOf[T]
  else throw new Error("Expected " + tag.runtimeClass.getName + ", got " + token.getClass.getName)
}