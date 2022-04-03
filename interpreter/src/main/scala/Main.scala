import scala.io.StdIn.readLine

import parser._
import runtime._
import lexer._

@main def main: Unit = 
  // val code = "let x = 2; let y = 3; x + y; print 3 * { let r = if x then 1 + 1 else 1 + 1 + 1; r * 2; };"
  val code = "let x = 1; while 1 do { print x + 2; };"
  println(lex(code))
  println(parse(ParserState(lex(code))))
  run(parse(ParserState(lex(code))))
  // run(parseStatement(ParserState(lex("x"))))
  // while(true) {
  //   val code = readLine()
  //   try {
  //     // println(run(parseExpression(ParserState(lex(code)))))
  //   } catch{
  //      case e: Throwable => println(e.getMessage())
  //   }
  // }



