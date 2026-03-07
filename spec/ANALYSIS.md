# R7RS syntax-rules Implementation Analysis

Based on testing Mutton's current macro system against progressively complex R7RS-style macros, here's a detailed assessment:

## System Capabilities (What Works ✅)

### 1. Simple Variable Substitution
**Example**: Identity macro, wrapping in lambda
```scheme
(def-syn id-m
  ((id-m x) x))

(def-syn wrap-single
  ((wrap-single x) (lam y x)))
```
**Status**: ✅ WORKS
**Notes**: Pattern variables are correctly bound to arguments and substituted into templates.

### 2. Multiple Arguments
**Example**: 
```scheme
(def-syn double-app
  ((double-app f x) (f (f x))))
```
**Status**: ✅ WORKS (with limitations - see below)
**Notes**: Pattern matching works correctly based on argument arity (number of arguments).

### 3. Syntax Quotation
**Example**:
```scheme
(def-syn quote-expr
  ((quote-expr x) (stx x)))
```
**Status**: ✅ WORKS
**Notes**: Can capture unevaluated syntax as template output.

### 4. Template-Introduced Bindings with Hygiene
**Example**:
```scheme
(def-syn wrap-single
  ((wrap-single x) (lam y x)))
```
**Status**: ✅ WORKS
**Notes**: Lambda parameters in templates get alpha-renamed (y.1) to avoid capture.

### 5. Argument Reordering
**Example**:
```scheme
(def-syn unless
  ((unless cond a b) (cond b a)))
```
**Status**: ✅ WORKS
**Notes**: Template can rearrange arguments arbitrarily.

## System Limitations (What Fails ❌)

### 1. Tree-Structured Pattern Matching
**What R7RS supports**:
```scheme
(define-syntax compose
  (syntax-rules ()
    ((compose f g x) ((f (g x))))))
```

**Mutton's limitation**: Pattern matching is based ONLY on argument count (arity), not on the structure of arguments. A pattern like `(compose f g x)` will try to match `((f (g x)))` (three subterms nested), but Mutton doesn't unwrap nested structures in patterns.

**Severity**: 🔴 CRITICAL - This is fundamental to R7RS. You cannot match compound expressions like `(+ x 1)` in patterns.

### 2. Ellipsis Patterns and Repetition
**What R7RS supports**:
```scheme
(define-syntax add-one
  (syntax-rules ()
    ((add-one arg ...) (+ arg 1) ...)))
```

**Mutton's limitation**: No support for `...` syntax to match zero or more repetitions. Variadic macros are impossible.

**Testing result**: Parse error when encountering `...` in patterns.

**Severity**: 🔴 CRITICAL - Essential for many fundamental macros (let, begin, etc.).

### 3. Literal Pattern Matching
**What R7RS supports**:
```scheme
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else body1 body2 ...)) (begin body1 body2 ...))
    ((cond (test => conseq) more-clauses ...) ...)))
```

**Mutton's behavior**: ALL pattern elements are treated as pattern variables. The identifier `else` in a pattern is just a variable like `x`, matching anything.

**Testing result**: Both forms match indiscriminately when they should only match literals.

**Severity**: 🔴 CRITICAL - Prevents implementation of proper conditionals, begin, etc.

### 4. Multiple Clauses with Different Arities
**What R7RS supports**:
```scheme
(define-syntax let
  (syntax-rules ()
    ((let () body) body)
    ((let ((x e) rest ...) body) ...)))
```

**Mutton's behavior**: Can support multiple clauses with different argument counts, BUT:
- Destructuring patterns (like `((x e))` as a single argument) don't work
- No ellipsis support makes multi-level recursion impossible

**Testing result**: "No matching macro rule" because pattern matching fails on destructured forms.

**Severity**: 🔴 CRITICAL - Requires both tree matching and ellipsis.

### 5. Nested Form Construction in Templates
**Example**:
```scheme
((compose f g x) ((f (g x))))
```

**Mutton's limitation**: The template `((f (g x)))` is interpreted as a top-level application of `(f (g x))`, but the pattern matching doesn't properly decompose the incoming `(f (g x))` structure from the arguments.

**Testing result**: "No matching macro rule" - the pattern cannot match the nested structure.

**Severity**: 🔴 CRITICAL - Prevents abstraction over complex expressions.

## Mutton's Actual Strengths

Mutton has excellent support for:
1. **Hygiene** - Alpha renaming prevents inadvertent capture
2. **Simple term-level abstraction** - Pattern variables work well for linear substitution
3. **Compile-time computation** - Can evaluate macros in a bound environment
4. **Syntax objects** - The `stx` form provides first-class syntax access

## What Would Be Needed for R7RS Compliance

To support R7RS syntax-rules, Mutton would need:

1. **Structural Pattern Matching**: Extend patterns to match nested form structures, not just arity
2. **Literal Matching**: Add syntax for literal keywords in patterns that match exactly, not as variables
3. **Ellipsis Support**: Implement `...` for zero-or-more patterns with template reproduction
4. **Multiple Clause Ranking**: Implement proper clause selection (try clauses in order, use first match)

These are non-trivial additions that would essentially result in a full pattern-matching subsystem.

## Verdict

**Current system is NOT sufficient for R7RS syntax-rules**.

The macro system is well-designed for simple syntactic abstractions (meta-predicates, inline transformations, term-level rewriting), but lacks the foundational pattern-matching infrastructure required for R7RS.

The gap is fundamental, not just a matter of implementation. R7RS requires:
- **Arity-based matching** ✅ Mutton has this
- **Structured pattern matching** ❌ Mutton lacks this  
- **Literal keywords** ❌ Mutton lacks this
- **Ellipsis repetition** ❌ Mutton lacks this

Without all four, you cannot implement core R7RS macros like `let`, `cond`, `begin`, `case`, etc.
