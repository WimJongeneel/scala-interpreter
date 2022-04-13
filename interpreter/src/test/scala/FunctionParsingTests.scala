import org.junit.Test
import org.junit.Assert.*

import parser._
import lexer._

class FunctionParsingTests:
  @Test def t1(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("x -> 1"))),
      Function("x", Literal(1))
    )

  @Test def t2(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("x -> x < 2 + 2"))),
      Function("x", BinaryOperator(Reference("x"), "<", BinaryOperator(Literal(2), "+", Literal(2))))
    )

  @Test def t3(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("x -> y -> z -> 1"))),
      Function("x", Function("y", Function("z", Literal(1))))
    )

  @Test def t4(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("foo(1)"))),
      FunctionCall(Reference("foo"), Literal(1))
    )

  @Test def t5(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("foo(1)(2)"))),
      FunctionCall(FunctionCall(Reference("foo"), Literal(1)), Literal(2))
    )