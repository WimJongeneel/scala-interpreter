import scala.io.StdIn.readLine

import parser._
import runtime._
import lexer._
import transformers._

@main def main: Unit = 
  val code = "let q := 1; let f := { let w := 2; a -> { let x := 1; x + a + w; }; }; print f(1);"

  val program = ProgramBuilder.parse(code)
    .withTransformer(ClosureTransformer)
    .withTransformer(ShadowReferenceTransformer)
    // .withTransformer(LiteralOptimizerTransformer)
    .program

  program.print

  program.run



