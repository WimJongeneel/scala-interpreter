package transformers

import parser.AST

abstract class Transformer {
    def transform(a: List[AST]): List[AST]
}