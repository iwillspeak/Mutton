# Mutton

> Much ado thereabouts

An experiment in hygenic macro expansion.

## Syntax

We begin with a simple language supporting lambdas, definitions, and syntax
transofmration definitions. Plain `def`initions and `lam`bdas introduce bindigns
into the current environment. `def-syn` definitions however introduce a hygenic
pattern expansion.

```bnf
form = (lam (SYM ... ) <form>...)   ; Lambda definition
     | (def SYM <form>)             ; Value definition (binding)
     | (def-syn id <rule>...)       ; Syntax mutation definition (macro binding) 
     | (quot <form>...)             ; Value quotation
     | (stx <form>...)              ; Syntax quotation
     | (<form>...)                  ; Application. Evaluates a function value.
     | atom                         ; Leaf literal

atom = NUMBER | SYM

rule = (<form> <form>)              ; Transfomer definition. Binds syntax to the
                                    ; first form (the pattern), and expands it
                                    ; in the template form.
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

## IL, ASTs, and Representations

We have _three_ main representations of the program:

 * CST - The concrete syntax, represented as a Red/Green tree with a typed
   layer defined in `Syntax`. This is a lossless representation of the input
   of the program.

 * STX Tree - The "Illuminated" tree. This is produced by the `illuminate` pass
   and acts as the input and output format for macro expansion in `Expand`.

   At this stage we replace some of the sugar from the CST. In a full production
   compiler this is where quote expressions would be tranformed into quote forms
   amongst other simplifications. The idea here is to have a uniform
   representation of the syntax for the later passes to work upon.

 * Bound Tree - Resolved representation of the program structure. Bound trees
   no longer think of the program as a "soup of forms" and instead impose some
   strucutre on the items within it. 

   Although the expansion phase has some understanding of bindings in order to
   expand _compile time_ elements it is up to the `Bind` pass to resolve these
   references down to canonical storage locations.
