package runtime

import parser._

def run(e: Expression): Float = e match {
  case Literal(n) => n
  case BinaryOperator(l, "+", r) => run(l) + run(r)
  case BinaryOperator(l, "-", r) => run(l) - run(r)
  case BinaryOperator(l, "*", r) => run(l) * run(r)
  case BinaryOperator(l, "/", r) => run(l) / run(r)
}