# Scala Interpreter

The [Program](https://github.com/WimJongeneel/scala-interpreter/blob/master/interpreter/src/main/scala/Program.scala) class is the entry point of the interpreter. The process of executing code goes through the following steps:
* [Lexer](https://github.com/WimJongeneel/scala-interpreter/blob/master/interpreter/src/main/scala/lexer/Lexer.scala): scans the input into tokes
* [Parser](https://github.com/WimJongeneel/scala-interpreter/blob/master/interpreter/src/main/scala/parser/Parser.scala): uses an operator-precedence parser to construct the AST
* Transformers:
    * [ClosureTransformer](https://github.com/WimJongeneel/scala-interpreter/blob/master/interpreter/src/main/scala/transformers/ClosureTransformer.scala): adds closures to function definitions
    * [ShadowReferenceTransformer](https://github.com/WimJongeneel/scala-interpreter/blob/master/interpreter/src/main/scala/transformers/ShadowReferenceTransformer.scala): renames variables to prevent shadowing
    * [LiteralOptimizerTransformer](https://github.com/WimJongeneel/scala-interpreter/blob/master/interpreter/src/main/scala/transformers/LiteralOptimizerTransformer.scala): rewrites expressions who's leaves are all literals to one single literal
* [Runtime](https://github.com/WimJongeneel/scala-interpreter/blob/master/interpreter/src/main/scala/runtime/Runtime.scala): executes the code with a treewalker

## Values and types

There are four runtime types in this language: `Number`, `Bool`, `Null` and `Function`. Values are loosely typed and will be converted when needed:

| value    | Number  | Bool    | Function    |
|----------|---------|---------|-------------|
| 0        | `self`  | `false` | `_ -> self` |
| 1        | `self`  | `true`  | `_ -> self` |
| Number   | `self`  | error   | `_ -> self` |
| true     | `1`     | `self`  | `_ -> self` |
| false    | `0`     | `self`  | `_ -> self` |
| null     | error   | error   | error       |
| Function | error   | error   | `self`      |

Bools are constructed by the `true` and `false` keyword. A null value can be created with `null`. Numbers are created via literal values, e.g. `1` or `42`

## Expressions

**Algebraic:**
```
3 / 2
1 + 2 * 3
3 - 9
```

**Comparison:**
```
1 = 1
1 != 2
1 < 2
2 > 1
```

**Boolean:**
```
1 = 1 | 2 = 4
1 = 1 & 2 = 4
!1
```

**Parentheses:**
```
(1 + 2) * 3
```

**Functions**

A function is declared with a single parameter name and an arrow.

```
a -> a + 2
a -> b -> a + b
```

A function is called by providing its formal parameter between parentheses:
```
(a -> a + 2)(1)
(a -> b -> a + b)(1)(2)
```

**Codeblocks:**

Codeblocks can be used as expressions. They have their own lexial scoping. A codeblock can contain both statements and expressions. The value the last statement evaluates to is what the entire codeblock evaluates to.

```
{ let a := 1 + 2; print a; 2; }
```

**Conditional**

```
if a < b then 1 else 2
```

## Statements

Statements must always and with a semicolon.

**Declaration:**

All variables are lexial scoped inside codeblocks.

```
let a := 1;
let b := 1 + 2 * 3;
let c := {
    let a := 1;
    let b := 2;
    a + b;
};
```

**Assinging:**

```
a := 42;
```

**Expression statements:**

All expressions can be used as a statement. 

**Print**

The result of any expression can be printed in the console with `print`:

```
print 3 / 2;
print 1 + 2 * 3;
print 3 - 9;
```

**While**

```
while a < 5 do {
    print a;
};
```
