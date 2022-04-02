import scala.io.StdIn.readLine

import parser._
import runtime._
import lexer._

@main def main: Unit = 
  val code = "let x = 9 * if 1 * 1 then 2 else 3"
  println(lex(code))
  println(parseStatement(ParserState(lex(code))))
  run(parseStatement(ParserState(lex(code))))
  run(parseStatement(ParserState(lex("x"))))
  // while(true) {
  //   val code = readLine()
  //   try {
  //     // println(run(parseExpression(ParserState(lex(code)))))
  //   } catch{
  //      case e: Throwable => println(e.getMessage())
  //   }
  // }



