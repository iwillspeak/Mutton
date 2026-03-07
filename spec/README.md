# Mutton Specification and Analysis

This directory contains test cases and analysis for Mutton's macro system, specifically evaluating its suitability for implementing R7RS syntax-rules.

## Quick Start

Run all test programs:
```bash
cd /home/will/Repositories/Mutton
for test in spec/test-*.mut; do
  echo "=== $(basename $test) ==="
  dotnet run < "$test"
  echo ""
done
```

Run a specific test:
```bash
dotnet run < spec/test-simple-macro.mut
```

See ALL stages of macro expansion (syntax dump, illuminated, bound):
```bash
dotnet run --show-all < spec/test-simple-macro.mut
```

Run with custom debug options:
```bash
dotnet run --show-syntax --show-bound < spec/test-simple-macro.mut
```

## Files

### Documentation
- **ANALYSIS.md** - In-depth analysis of macro system capabilities and limitations
- **TEST-RESULTS.md** - Detailed results for each test with explanations
- **README.md** - This file

### Test Programs

#### Passing Tests ✅
- `test-simple-macro.mut` - Basic identity macro (returns argument unchanged)
- `test-literal-pattern.mut` - Pattern matching with variable substitution
- `test-wrap-lambda.mut` - Wrapping expression in lambda (demonstrates hygiene)
- `test-double-app.mut` - Repeated function application
- `test-quote-syntax.mut` - Syntax quotation with `stx` form
- `test-unless.mut` - Argument reordering (simple transformation)

#### Failing Tests ❌
- `test-nested-macro.mut` - Scheme-style multi-param lambda syntax (not supported)
- `test-multi-arg-macro.mut` - Same lambda syntax issue
- `test-ellipsis-pattern.mut` - Variadic patterns (no `...` support)
- `test-let-macro.mut` - Destructuring patterns + ellipsis (too complex)
- `test-cond-macro.mut` - Conditional with multiple clauses and nesting
- `test-compose.mut` - Function composition with nested structures

### Original
- `mn.mut` - Original specification example from the project

## Macro System Summary

### What Works ✅
- Simple variable substitution
- Arity-based pattern matching (number of arguments)
- Hygenic macro expansion (alpha-renaming)
- Syntax quotation (`stx` form)
- Argument reordering in templates
- Template-introduced bindings get fresh names

### What Doesn't Work ❌
- **Tree-structured pattern matching** - Can only match on argument count, not structure
- **Literal keywords** - All pattern elements are variables (can't match `else` as literal)
- **Ellipsis patterns** - No support for `...` repetition
- **Destructuring** - Can't match nested forms like `((x y))`
- **Multi-parameter lambdas** - Only supports `(lam x body)`, not `(lambda (x y) body)`

## Conclusion

**The current Mutton macro system is NOT sufficient to implement R7RS syntax-rules.**

Mutton's system is well-suited for simple syntactic abstractions but lacks the structured pattern-matching infrastructure required for R7RS compliance. Implementing full R7RS support would require fundamental additions to the pattern language.

For more details, see **ANALYSIS.md**.
