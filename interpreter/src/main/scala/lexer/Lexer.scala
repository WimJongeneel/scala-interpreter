package lexer

import scala.collection.mutable.ArrayBuffer

def lex(input: String): ArrayBuffer[Token] = {
  val result = new ArrayBuffer[Token]
  var remaining = input.replaceAll("\\s", "")

  while(remaining.length > 0) {
    val (token, r) = consume(remaining)
    result += token
    remaining = r
  }

  result += EOF()

  result
}

def consume(input: String) : (Token, String) = {
  // comsumeRegex helper?
  "^[0-9]+".r.findFirstMatchIn(input) match {
    case Some(n) => return (Number(n.matched.toFloat), input.substring(n.matched.length))
    case None    => {}
  }
  
  "^let".r.findFirstMatchIn(input) match {
    case Some(_) => return (Let(), input.substring(3))
    case None    => {}
  }

  "^if".r.findFirstMatchIn(input) match {
    case Some(_) => return (If(), input.substring(2))
    case None    => {}
  }

   "^then".r.findFirstMatchIn(input) match {
    case Some(_) => return (Then(), input.substring(4))
    case None    => {}
  }

   "^else".r.findFirstMatchIn(input) match {
    case Some(_) => return (Else(), input.substring(4))
    case None    => {}
  }

  "^[a-zA-Z_]+".r.findFirstMatchIn(input) match {
    case Some(n) => return (Id(n.matched), input.substring(n.matched.length))
    case None    => {}
  }


  input.charAt(0) match {
    case '+' => (Plus(), input.substring(1))
    case '-' => (Minus(), input.substring(1))
    case '*' => (Multiply(), input.substring(1))
    case '/' => (Divide(), input.substring(1))
    case '=' => (Equals(), input.substring(1))
    case ';' => (SemiColon(), input.substring(1))
    case '(' => (LP(), input.substring(1))
    case ')' => (RP(), input.substring(1))
    case c => throw new Error("Unexpected char: " + c)
  }
}

