import org.junit.Test
import org.junit.Assert.*

import parser._
import lexer._

class PrecedenceTests:
  @Test def t1(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 + 2 * 3"))),
      BinaryOperator(Literal(1), "+", BinaryOperator(Literal(2), "*", Literal(3)))
    )

  @Test def t3(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("2 * 3 + 1"))),
      BinaryOperator(BinaryOperator(Literal(2), "*", Literal(3)), "+", Literal(1))
    )

  @Test def t4(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 > 2 = 1"))),
      BinaryOperator(BinaryOperator(Literal(1), ">", Literal(2)), "=", Literal(1))
    )

  @Test def t5(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 = 1 > 2"))),
      BinaryOperator(Literal(1), "=", BinaryOperator(Literal(1), ">", Literal(2)))
    )

  @Test def t6(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 + 1 > 2 = 1 + 1"))),
      parseExpression(ParserState(lex("((1 + 1) > 2) = (1 + 1)")))
    )

  @Test def t7(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("({ 1 + 1; }) > 2 + 1"))),
      parseExpression(ParserState(lex("({ 1 + 1; }) > (2 + 1)")))
    )

  @Test def t8(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 * 1 = 2 > 4 & 1"))),
      parseExpression(ParserState(lex(("((1 * 1) = (2 > 4)) & 1"))))
    )

  @Test def t9(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("((1)) > 2"))),
      parseExpression(ParserState(lex(("1 > 2"))))
    )