import parser._
import runtime._
import lexer._

@main def main: Unit = 
  println(run(parse(ParserState(lex("1+2*3+100")))))



