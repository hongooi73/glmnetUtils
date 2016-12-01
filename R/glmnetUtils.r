#' @title Utilities for glmnet
#' @description
#' Some quality-of-life functions to streamline the process of fitting elastic net models with the `glmnet` package, specifically:
#' 
#' \itemize{
#'   \item \code{glmnet.formula} provides a formula/data frame interface to \code{glmnet}.
#'   \item \code{cv.glmnet.formula} does a similar thing for \code{cv.glmnet}.
#'   \item Methods for \code{predict} and \code{coef} for both the above.
#'   \item A function \code{cvAlpha.glmnet} to choose both the alpha and lambda parameters via cross-validation, following the approach described in the help page for \code{cv.glmnet}. Optionally does the cross-validation in parallel.
#'   \item Methods for \code{plot}, \code{predict} and \code{coef} for the above.
#' }
#' 
#' @docType package
#' @name glmnetUtils
#' @aliases glmnetUtils-package
NULL


## assorted imports from base packages
#' @importFrom graphics lines plot
#' @importFrom grDevices palette topo.colors
#' @importFrom stats coef model.extract model.matrix model.response na.pass predict terms
NULL
