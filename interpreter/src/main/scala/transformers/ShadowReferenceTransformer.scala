package transformers

import parser.AST
import utils._

object ShadowReferenceTransformer extends Transformer {

    def transform(a: List[AST]) = a 


    // type RewriteRules = List[Map[String, String]]
    // State[RewriteRules, Expression]
    // case Ref -> map with rewrite rule
    // case decl -> update rewrite rules
    // case fun -> update rewrite rules
    // case fun -> add new scope / drop scope
    // codeblock -> add new scope / drop scope
}