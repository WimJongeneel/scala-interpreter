import org.junit.Test
import org.junit.Assert.*

import parser._
import lexer._

class PrecedenceTests:
  @Test def t1(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 + 2 * 3"))),
      parseExpression(ParserState(lex("1 + (2 * 3)")))
    )

  @Test def t3(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("2 * 3 + 1"))),
      parseExpression(ParserState(lex("(2 * 3) + 1")))
    )

  @Test def t4(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 > 2 = 1"))),
      parseExpression(ParserState(lex("(1 > 2) = 1")))
    )

  @Test def t5(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 = 1 > 2"))),
      parseExpression(ParserState(lex("1 = (1 > 2)")))
    )

  @Test def t6(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("1 + 1 > 2 = 1 + 1"))),
      parseExpression(ParserState(lex("((1 + 1) > 2) = (1 + 1)")))
    )

  @Test def t7(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("{ 1 + 1 } > 2 + 1"))),
      parseExpression(ParserState(lex("({ 1 + 1 }) > (2 + 1)")))
    )