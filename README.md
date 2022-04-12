# Scala Interpreter

A simple interpreter I wrote while learning Scala. The interpreter has a lexer, a parser and a runtime. The language supports variable declarations, expressions with algebraric and logical operators, an if-expressions, while-loops and a print statement. Next to this it also supports codeblocks as expressions (just like in Scala).

```scala
let x := 1 + 2 * 3;
let y := if x > 8 then 2 * 2 else {
    let z := 3
    z * z;
};
print x + y;
```