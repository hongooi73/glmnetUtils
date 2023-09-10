## glmnetUtils 1.1.9

- Remove vignette dependency on an external download.

## glmnetUtils 1.1.8

- Skip some tests on 32-bit Solaris R-patched due to numerical convergence issues.

## glmnetUtils 1.1.7

- Add some plotting options for `plot.cva.glmnet`: `log.x` controls whether to plot the X-axis (lambda) on the log scale, and the legend can be omitted by setting either `legend.x` or `legend.y` to `NULL`.
- Compatibility fixes for glmnet 4.1-1.

## glmnetUtils 1.1.6

- Update maintainer email address.

## glmnetUtils 1.1.5

- Fix handling of non-factor categorical predictors (from R 4.0, data frames will not have character columns converted to factors by default). The practical impact of this should be minor.

## glmnetUtils 1.1.4

- Fix printout of `glmnet.formula` object.

## glmnetUtils 1.1.3

- Support relaxed (non-regularised) fits in `glmnet.formula` and `cv.glmnet.formula` (requires glmnet 3.0 or later).
- Add a legend when plotting a `cva.glmnet` object.

## glmnetUtils 1.1.2

- Fixes a bug in the assignment of observations to crossvalidation folds in `cva.glmnet`. The impact is most serious for small datasets, where the number of observations per fold is relatively low. If you are using this function, it's highly recommended you update the package.

## glmnetUtils 1.1.1
- Fixes bug where `nfolds` argument was not being passed to `glmnet::cv.glmnet`.

## glmnetUtils 1.1
- Now allows interaction and expression terms without requiring `use.model.frame=TRUE`. This works in an additive fashion, ie the formula `~ a + b:c + d*e` is treated as consisting of three terms, `a`, `b:c` and `d*e` each of which is processed independently of the others. A dot in the formula includes all main effect terms, ie `~ . + a:b + f(x)` expands to `~ a + b + x + a:b + f(x)` (assuming a, b and x are the only columns in the data). Note that a formula like `~ (a + b) + (c + d)` will be treated as two terms, `a + b` and `c + d`.
- The call component of a `glmnet`/`cv.glmnet` object that uses the original matrix/vector interface is now useful.
- You can now explicitly specify the vector of crossvalidation folds (for the inner loop over lambda) when calling `cva.glmnet`.
- Correctly handle non-syntactic factor variables in a formula.

## glmnetUtils 1.0.2
- Initial release to CRAN.
