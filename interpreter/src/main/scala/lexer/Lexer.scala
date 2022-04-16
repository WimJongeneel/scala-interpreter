package lexer

import scala.collection.mutable.ArrayBuffer

def lex(input: String): ArrayBuffer[Token] = {
  val result = new ArrayBuffer[Token]
  var remaining = input.replaceAll("^\\s", "")

  while(remaining.length > 0) {
    val (token, r) = consume(remaining)
    result += token
    remaining = r.replaceAll("^\\s", "")
  }

  result += EOF()

  result
}

def consume(input: String): (Token, String) = {

  consumeKeyword(
    List(
      ("let", () => Let()),
      ("if", () => If()),
      ("then", () => Then()),
      ("else", () => Else()),
      ("print", () => Print()),
      ("while", () => While()),
      ("do", () => Do()),
      (":=", () => Assign()),
      ("!=", () => NotEquals()),
      ("->", () => Arrow()),
      ("true", () => True()),
      ("false", () => False()),
      ("null", () => Null()),
    ), 
    input
  ) match {
    case Some(r) => return r
    case None    => {}
  }

  "^[0-9]+".r.findFirstMatchIn(input) match {
    case Some(n) => return (Number(n.matched.toFloat), input.substring(n.matched.length))
    case None    => {}
  }

  "^[a-zA-Z]+".r.findFirstMatchIn(input) match {
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
    case '{' => (LB(), input.substring(1))
    case '}' => (RB(), input.substring(1))
    case '<' => (Smaller(), input.substring(1))
    case '>' => (Larger(), input.substring(1))
    case '&' => (And(), input.substring(1))
    case '|' => (Or(), input.substring(1))
    case '!' => (Not(), input.substring(1))
    case c => throw new Error("Unexpected char: " + c)
  }
}

def consumeKeyword(keywords: List[(String, () => Token)], input: String): Option[(Token, String)] = {
  for((keyword, token) <- keywords) {
    ("^" + keyword).r.findFirstMatchIn(input) match {
      case Some(_) => return Some(token(), input.substring(keyword.length))
      case None    => {}
    }
  }

  None
}