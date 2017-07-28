## glmnetUtils 1.1
- Now allows interaction and expression terms without requiring `use.model.frame=TRUE`. This works in an additive fashion, ie the formula `~ a + b:c + d*e` is treated as consisting of three terms, `a`, `b:c` and `d*e` each of which is processed independently of the others. A dot in the formula includes all main effect terms, ie `~ . + a:b + f(x)` expands to `~ a + b + x + a:b + f(x)` (assuming a, b and x are the only columns in the data). Note that a formula like `~ (a + b) + (c + d)` will be treated as two terms, `a + b` and `c + d`.
- The call component of a `glmnet`/`cv.glmnet` object that uses the original matrix/vector interface is now useful.
- You can now explicitly specify the vector of crossvalidation folds (for the inner loop over lambda) when calling `cva.glmnet`.
- Correctly handle non-syntactic factor variables in a formula.

## glmnetUtils 1.0.2
- Initial release to CRAN.
