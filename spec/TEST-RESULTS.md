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

### test-nested-macro.mut
Introduces a binding in a macro expansion.
```
Result: Fun ("y.1", Quot (DNum 42)) ✅
Note: Variable introduced by macro template gets alpha-renamed
```

### test-multi-arg-macro.mut
Applies a function argument to another argument.
```
Result: App (Fun ("n.1", Var "n.1"), [Quot (DNum 10)]) ✅
Note: Macro correctly passes arguments through to application
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

### test-compose.mut
Function composition: takes two functions, returns a new function that applies them in sequence.
```
Result: App (Fun ("x.3", App (Fun ("a.1", ...), [...])), [Quot (DNum 99)]) ✅
Note: Fresh variable 'x' is alpha-renamed to 'x.3'. Template generates nested structure.
```

## Failing Tests ❌

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

## Key Insights

1. **Mutton's lambda syntax requires**:
   - Single parameter: `(lam x body)`
   - NOT Scheme's `(lambda (x) body)`

2. **Pattern matching is arity-based**:
   - `(m a b c)` matches a 3-parameter pattern
   - `(m (f (g x)))` does NOT destructure the nested structure
   - Pattern elements are ALWAYS variables, never literals
   - **BUT**: You can generate nested structures in the template!

3. **What works well**:
   - Linear substitution of variables
   - Simple and higher-order term-level macros
   - Hygenic expansion
   - Syntax quotation
   - Function composition and abstraction

4. **What's missing for full R7RS**:
   - Destructuring patterns (match nested forms in pattern position)
   - Literal keywords
   - Ellipsis repetition
   - Multi-parameter lambda support

## Recommendation

Mutton's macro system is suitable for:
- ✅ Simple syntactic abstractions
- ✅ Meta-predicates
- ✅ Higher-order function abstraction
- ✅ Inline transformations
- ✅ Linear term rewriting

But NOT suitable for:
- ❌ Full R7RS syntax-rules
- ❌ Multi-parameter destructuring
- ❌ Variadic macros
- ❌ Literal keyword patterns
