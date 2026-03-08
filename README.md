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
macro expansion. We use this source provenence to enforce hygene with
*syntactic closures*.

Once expansion is complete the macro-free source text can be bound into an
output format ready for use. We aim to perform this in a single `bind` pass
that takes the illuminated `Stx` tree and recursively walks it expanding
each node as it goes and then resolving the resulting macro-free syntax into
a `Bound` program with syntax constructs applied and variables assigned to
their storage locations.

## Naming Things ™️

We try to follow the Terminology from Bawden & Rees, with a few additions:

 * A `name` is a token used to "name" something. This is our `SYM` in the
   grammar.
 * A `keyword` is a `name` used to introduce a syntactic construct. The
   keywords supported in our language are: `lam`, `def`, `def-syn`, `quot`, and
   `stx`.
 * An `identifier` is a `name` used to refer to a variable.
 * A `variable` is the storage location for a specific value. The same
   `identifier` may refer to different `variable`s depending on the environment.
 * A `macro` is a syntax tranformer.
 * A `syntactic construct` is an item with meaning at syntax expansion time,
   such as a `keyword` or `macro`.

When binding there are two "environments" to consider: syntactic and value. 

 * The _syntactic environment_ contains the mapping of `names` to `variables`
   `keywords`, and `macros`. It is used during the `expand` section
   of the `bind` pass to recognise references to `syntactic constructs`, and
   distinguish the hygenically correct `variable` that an `identifier` refers
   to.

 * The _value environment_ maps variables to storage locations for their
   values. It is used to resolve the locations that values should be read or
   written from.

   In this toy lanugage our locations are simple named suffixes of the variable
   name. e.g. an idntifier `x` may refer to the storage location `x.1` or `x.2`
   etc. depending on scope. A production compiler would need to assign these to
   local variable slots, argument indexes, capture environment locations, or
   globals.

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
