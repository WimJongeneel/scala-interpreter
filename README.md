# Scala Interpreter

## Expressions

All expression evaluate to a `Float`. `1` and `0` are used for the result of boolean operators.

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
{ let a := 1 + 2; print a; 2 }
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