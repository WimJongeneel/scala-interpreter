import scala.io.StdIn.readLine

import parser._
import runtime._
import lexer._

@main def main: Unit = 
  val code = "let x = 8 * 1 + 2"
  println(lex(code))
  println(parseStatement(ParserState(lex(code))))
  // while(true) {
  //   val code = readLine()
  //   try {
  //     // println(run(parseExpression(ParserState(lex(code)))))
  //   } catch{
  //      case e: Throwable => println(e.getMessage())
  //   }
  // }



