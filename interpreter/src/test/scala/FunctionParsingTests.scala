import org.junit.Test
import org.junit.Assert.*

import parser._
import lexer._

class FunctionParsingTests:
  @Test def t1(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("x -> 1"))),
      FunctionDefinition("x", Literal(1), Set())
    )

  @Test def t2(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("x -> x < 2 + 2"))),
      FunctionDefinition("x", BinaryOperator(Reference("x"), "<", BinaryOperator(Literal(2), "+", Literal(2))), Set())
    )

  @Test def t3(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("x -> y -> z -> 1"))),
      FunctionDefinition(
        "x",
        FunctionDefinition(
          "y",
          FunctionDefinition(
            "z", 
            Literal(1),
            Set()
          ),
          Set()
        ), Set()
      )
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

  @Test def t6(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("{x -> y -> x + y;}(1)(2)"))),
      FunctionCall(
        FunctionCall(
          CodeBlock(
            List(
              ExpressionStatement(
                FunctionDefinition(
                  "x", 
                  FunctionDefinition("y", BinaryOperator(Reference("x"), "+", Reference("y")), Set()), 
                  Set()
                )
              )
            )
          ), 
          Literal(1)
        ), 
        Literal(2)
      )
    )

  @Test def t61(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("(x -> y -> x + y)(1)(2)"))),
      FunctionCall(
        FunctionCall(
          FunctionDefinition(
            "x", 
            FunctionDefinition(
              "y",
              BinaryOperator(Reference("x"), "+", Reference("y")), 
              Set()
            ), 
            Set()
          ), 
          Literal(1)
        ), 
        Literal(2)
      )
    )

  @Test def t7(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("{foo;}(1)(2)"))),
      FunctionCall(
        FunctionCall(
          CodeBlock(
            List(
              ExpressionStatement(Reference("foo"))
            )
          ), 
          Literal(1)
        ),
        Literal(2)
      )
    )

  @Test def t71(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("(foo)(1)(2)"))),
      FunctionCall(
        FunctionCall(
          Reference("foo"), 
          Literal(1)
        ), 
        Literal(2)
      )
    )

  @Test def t8(): Unit = 
    assertEquals(
      parseExpression(ParserState(lex("{x -> y -> x + y;}(1)(2)"))),
      FunctionCall(
        FunctionCall(
          CodeBlock(
            List(
              ExpressionStatement(
                FunctionDefinition(
                  "x", 
                  FunctionDefinition(
                    "y", 
                    BinaryOperator(Reference("x"), "+", Reference("y")), 
                    Set()
                  ), 
                  Set()
                )
              )
            )
          ), 
          Literal(1)
        ), 
        Literal(2)
      )
    )