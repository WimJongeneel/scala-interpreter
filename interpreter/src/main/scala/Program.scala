import parser._
import lexer._
import transformers._
import runtime._

case class Program(ast: List[AST] = List()) {

    def run = Runtime.run(ast)

    def print = ast.foreach(a => println(AST.show(a)))

    override def toString = ast.foldLeft("")((r, c) => r + '\n' + AST.show(c))
}

case class ProgramBuilder(ast: List[AST] = List(), transformers: List[Transformer] = List()) {

    def withTransformer(t: Transformer) = ProgramBuilder(ast, transformers.appended(t))

    def program = Program(transformers.foldLeft(ast)((a, t) => t.transform(a)))

    def parse(code: String) = 
        val newAST = parser.parse(ParserState(lex(code)))
        ProgramBuilder(ast.concat(newAST), transformers)

    def print = program.print
}

object ProgramBuilder {

    def parse(code: String) = ProgramBuilder(parser.parse(ParserState(lex(code))))

    def withTransformer(t: Transformer) = ProgramBuilder(List(), List(t))
}
