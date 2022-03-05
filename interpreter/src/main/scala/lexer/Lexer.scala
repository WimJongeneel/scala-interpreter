package lexer

import scala.collection.mutable.ArrayBuffer

def lex(input: String): ArrayBuffer[Token] = {
  val result = new ArrayBuffer[Token]
  var remaining = input

  while(remaining.length > 0) {
    val (token, r) = consume(remaining)
    result += token
    remaining = r
  }

  result += EOF()
  
  result
}

def consume(input: String) : (Token, String) = {
  "^[0-9]+".r.findFirstMatchIn(input) match {
    case Some(n) => return (Number(n.matched.toFloat), input.substring(n.matched.length))
    case None    => {}
  }

  input.charAt(0) match {
      case '+' => (Plus(), input.substring(1))
      case '-' => (Minus(), input.substring(1))
      case '*' => (Multiply(), input.substring(1))
      case '/' => (Divide(), input.substring(1))
  }
}