# Test Results Summary

Run all tests with:
```bash
cd /home/will/Repositories/Mutton
for test in spec/test-*.mut; do
  echo "=== $(basename $test) ==="
  dotnet run < "$test"
  echo ""
done
```

## Passing Tests ✅

### test-simple-macro.mut
Simple identity macro that returns its argument unchanged.
```
Result: Quot (DNum 42) ✅
```

### test-literal-pattern.mut
Macro where all pattern elements (even those named 'else') are bound as variables.
```
Result: Quot (DNum 99) ✅
Note: 'else' is treated as a variable, not a literal keyword
```

### test-wrap-lambda.mut
Wraps argument in a lambda expression.
```
Result: Fun ("y.1", Quot (DNum 42)) ✅
Note: 'y' gets alpha-renamed to 'y.1' for hygiene
```

### test-double-app.mut
Applies a function twice to an argument.
```
Result: App (Fun ("y.1", Var "y.1"), [...]) ✅
```

### test-quote-syntax.mut
Captures unevaluated syntax.
```
Result: Stx (StxIdent ("x", x, 0)) ✅
```

### test-unless.mut
Reorders arguments in a macro expansion.
```
Result: App (Fun ("p.1", App (Var "p.1", [...])), [...]) ✅
```

## Failing Tests ❌

### test-nested-macro.mut (FAILS)
Attempts to use standard Scheme lambda syntax `(lam (y) body)` with parameter list as a form.
```
Error: Invalid lambda form [StxForm ([StxIdent ("y", y, 0)], (y), 0); ...]
Reason: Mutton only supports single-parameter lambdas: (lam y body)
```

### test-multi-arg-macro.mut (FAILS)
Same issue - tries to use multi-parameter lambda syntax.
```
Error: Invalid lambda form
Reason: Same as above
```

### test-ellipsis-pattern.mut (FAILS - Simplified Version)
Attempts variadic macro without actual ellipsis.
```
Error: Invalid lambda form
Reason: Nested structure in arguments, plus general lambda limitation
```

### test-let-macro.mut (FAILS)
Attempts destructuring pattern `((a e))` and multiple clauses with ellipsis.
```
Error: No matching macro rule for arguments [StxForm ([StxForm (...)], ...), ...]
Reason 1: Pattern matching doesn't handle destructured forms
Reason 2: No ellipsis support for recursive patterns
```

### test-cond-macro.mut (FAILS)
Simple conditional - fails because of nested form issues.
```
Error: Invalid lambda form
Reason: Nested structures and lambda syntax limitations
```

### test-compose.mut (FAILS)
Function composition macro with nested template.
```
Error: No matching macro rule for arguments [StxForm ([...], (...)), ...]
Reason: Pattern arity matching only - doesn't match nested structure of arguments
```

## Key Insights

1. **Mutton's lambda syntax requires**:
   - Single parameter: `(lam x body)`
   - NOT Scheme's `(lambda (x) body)`

2. **Pattern matching is arity-based**:
   - `(m a b c)` matches a 3-parameter pattern
   - `(m (f (g x)))` does NOT decompose the nested structure
   - Pattern elements are ALWAYS variables, never literals

3. **What works well**:
   - Linear substitution of variables
   - Simple term-level macros
   - Hygenic expansion
   - Syntax quotation

4. **What's missing for R7RS**:
   - Structured pattern matching
   - Literal keywords
   - Ellipsis repetition
   - Multi-parameter lambda support

## Recommendation

Mutton's macro system is suitable for:
- ✅ Simple syntactic abstractions
- ✅ Meta-predicates
- ✅ Inline transformations
- ✅ Linear term rewriting

But NOT suitable for:
- ❌ R7RS syntax-rules
- ❌ Complex pattern matching
- ❌ Variadic macros
- ❌ Destructuring patterns
