# Mutton

> Much ado thereabouts

An experiment in hygenic macro expansion.

## Syntax

We begin with a simple language supporting lambdas, definitions, and syntax
transofmration definitions. Plain `def`initions and `lam`bdas introduce bindigns
into the current environment. `def-syn` definitions however introduce a hygenic
pattern expansion.

```
form = (lam (SYM ... ) <form>...)
     | (def SYM <form>)
     | (def-syn id <rule>...)
     | (<form>...)
     | atom

atom = NUMBER | SYM

rule = ((SYM...) <form>...)
```

## Reading, Parsing, Binding, and Illumination

Programs are transformed iniitally by the parser from a string into a concrete
syntax tree. This tree is then "illuminated" to add *syntactic context* before
macro expansion. Syntactic context allows annotation of expressions with two
items:

 * The active *renamings* withn the item. This is used to tell identifiers apart
   in syntax where the source code uses the same symbol
 * The current *marks* applied to a given syntax. Marks are used to keep track
   of syntax that was transformed by an expansion to ensure the correct
   renamings are applied.

Once expansion is complete the macro-free source text can be bound into an
output format ready for use.