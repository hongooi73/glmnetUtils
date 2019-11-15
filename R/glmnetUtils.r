#' @title Utilities for glmnet
#' @description
#' Some quality-of-life functions to streamline the process of fitting elastic net models with the `glmnet` package, specifically:
#'
#' * `glmnet.formula` provides a formula/data frame interface to `glmnet`.
#' * `cv.glmnet.formula` does a similar thing for `cv.glmnet`.
#' * Methods for `predict` and `coef` for both the above.
#' * A function `cva.glmnet` to choose both the alpha and lambda parameters via cross-validation, following the approach described in the help page for `cv.glmnet`. Optionally does the cross-validation in parallel.
#' * Methods for `plot`, `predict` and `coef` for the above.
#'
#' @docType package
#' @name glmnetUtils
#' @aliases glmnetUtils-package
NULL


## assorted imports from base packages
#' @importFrom graphics lines plot
#' @importFrom grDevices palette topo.colors
#' @importFrom stats coef na.pass predict terms .getXlevels delete.response
#' @importFrom stats model.frame model.extract model.matrix model.response
NULL
