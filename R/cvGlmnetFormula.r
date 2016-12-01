#' @include glmnetUtils.r
NULL

#' @name cv.glmnet
#' @export
cv.glmnet <- function(x, ...)
UseMethod("cv.glmnet")

#' @rdname cv.glmnet
#' @method cv.glmnet default
#' @export
cv.glmnet.default <- function(x, ...)
glmnet::cv.glmnet(x, ...)


#' Formula interface for elastic net cross-validation with cv.glmnet
#'
#' @param x For the default method, a matrix of predictor variables.
#' @param formula A model formula; interaction terms are allowed and will be expanded per the usual rules for linear models.
#' @param data A data frame or matrix containing the variables in the formula.
#' @param weights An optional vector of case weights to be used in the fitting process. If missing, defaults to an unweighted fit.
#' @param offset An optional vector of offsets, an \emph{a priori} known component to be included in the linear predictor.
#' @param subset An optional vector specifying the subset of observations to be used to fit the model.
#' @param na.action A function which indicates what should happen when the data contains missing values. For the \code{predict} method, \code{na.action = na.pass} will predict missing values with \code{NA}; \code{na.omit} or \code{na.exclude} will drop them.
#' @param drop.unused.levels Should factors have unused levels dropped? Defaults to \code{FALSE}.
#' @param xlev A named list of character vectors giving the full set of levels to be assumed for each factor.
#' @param alpha The elastic net mixing parameter. See \code{\link[glmnet:glmnet]{glmnet::glmnet}} for more details.
#' @param nfolds The number of crossvalidation folds to use. See \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}} for more details.
#' @param sparse Should the model matrix be in sparse format? This can save memory when dealing with many factor variables, each with many levels (but see the warning below).
#' @param use.model.frame Should the base \code{\link{model.frame}} function be used when constructing the model matrix? This is the standard method that most R modelling functions use, but has some disadvantages. The default is to avoid \code{model.frame} and construct the model matrix term-by-term; see \link[=glmnet.model.matrix]{discussion}.
#' @param ... For \code{cv.glmnet.formula} and \code{cv.glmnet.default}, other arguments to be passed to \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}; for the \code{predict} and \code{coef} methods, arguments to be passed to their counterparts in package \code{glmnet}.
#'
#' @details
#' The \code{cv.glmnet} function in this package is an S3 generic with a formula and a default method. The former calls the latter, and the latter is simply a direct call to the \code{cv.glmnet} function in package \code{glmnet}. All the arguments to \code{glmnet::cv.glmnet} are (or should be) supported.
#'
#' The code works in a similar manner to \code{lm}, \code{glm} and other modelling functions. The arguments are used to generate a \emph{model frame}, which is a data frame augmented with information about the roles the columns play in fitting the model. This is then turned into a \emph{model matrix} and a response vector, which are passed to \code{glmnet::glmnet} along with any arguments in \code{...}. If \code{sparse} is TRUE, then \code{Matrix::sparse.model.matrix} is used instead of \code{stats::model.matrix} to create the model matrix.
#'
#' The \code{predict} and \code{coef} methods are wrappers for the corresponding methods in the \code{glmnet} package. The former constructs a predictor model matrix from its \code{newdata} argument and passes that as the \code{newx} argument to \code{glmnet:::predict.cv.glmnet}.
#'
#' @return
#' For \code{cv.glmnet.formula}, an object of class \code{cv.glmnet.formula}. This is basically the same object created by \code{glmnet::cv.glmnet}, but with extra components to allow formula usage.
#'
#' @seealso
#' \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}, \code{\link[glmnet:predict.cv.glmnet]{glmnet:::predict.cv.glmnet}}, \code{\link[glmnet:coef.cv.glmnet]{glmnet:::coef.cv.glmnet}}, \code{\link{model.frame}}, \code{\link{model.matrix}}
#' 
#' @examples
#' cv.glmnet(mpg ~ ., data=mtcars)
#'
#' cv.glmnet(Species ~ ., data=iris, family="multinomial")
#'
#' \dontrun{
#'
#' # Leukemia example dataset from Trevor Hastie's website
#' download.file("http://web.stanford.edu/~hastie/glmnet/glmnetData/Leukemia.RData",
#'               "Leukemia.RData")
#' load("Leukemia.Rdata")
#' leuk <- do.call(data.frame, Leukemia)
#' cv.glmnet(y ~ ., leuk, family="binomial")
#' }
#' @rdname cv.glmnet
#' @method cv.glmnet formula
#' @importFrom glmnet cv.glmnet
#' @export
cv.glmnet.formula <- function(formula, data, alpha=1, nfolds=10, ..., weights=NULL, offset=NULL, subset=NULL,
                              na.action=getOption("na.action"), drop.unused.levels=FALSE, xlev=NULL,
                              sparse=FALSE, use.model.frame=FALSE)
{
    # must use NSE to get model.frame emulation to work
    cl <- match.call(expand.dots=FALSE)
    cl[[1]] <- if(use.model.frame)
        makeModelComponentsMF
    else makeModelComponents
    xy <- eval.parent(cl)

    model <- glmnet::cv.glmnet(xy$x, xy$y, weights=xy$weights, offset=xy$offset, alpha=alpha, ...)
    model$call <- match.call()
    model$terms <- xy$terms
    model$alpha <- alpha
    model$nfolds <- nfolds
    model$sparse <- sparse
    model$use.model.frame <- use.model.frame
    model$na.action <- na.action
    class(model) <- c("cv.glmnet.formula", class(model))
    model
}


#' @param object For the \code{predict} and \code{coef} methods, an object of class \code{cv.glmnet.formula}.
#' @param newdata For the \code{predict} method, a data frame containing the observations for which to calculate predictions.
#' @rdname cv.glmnet
#' @method predict cv.glmnet.formula
#' @export
predict.cv.glmnet.formula <- function(object, newdata, na.action=na.pass, ...)
{
    if(!inherits(object, "cv.glmnet.formula"))
        stop("invalid cv.glmnet.formula object")

    # must use NSE to get model.frame emulation to work
    cl <- match.call(expand.dots=FALSE)
    cl$formula <- object$terms
    cl$data <- cl$newdata
    cl[[1]] <- if(object$use.model.frame)
        makeModelComponentsMF
    else makeModelComponents
    xy <- eval.parent(cl)
    x <- xy$x
    offset <- xy$offset

    class(object) <- class(object)[-1]
    predict(object, x, ...)
}


#' @rdname cv.glmnet
#' @method coef cv.glmnet.formula
#' @export
coef.cv.glmnet.formula <- function(object, ...)
{
    if(!inherits(object, "cv.glmnet.formula"))
        stop("invalid cv.glmnet.formula object")
    class(object) <- class(object)[-1]
    coef(object, ...)
}


#' @rdname cv.glmnet
#' @method print cv.glmnet.formula
#' @export
print.cv.glmnet.formula <- function(x, ...)
{
    cat("Call:\n")
    dput(x$call)
    cat("\nModel fitting options:")
    cat("\n    Sparse model matrix:", x$sparse)
    cat("\n    Use model.frame:", x$use.model.frame)
    cat("\n    Number of crossvalidation folds:", x$nfolds)
    cat("\n    Alpha:", x$alpha)
    cat("\n    Deviance-minimizing lambda:", x$lambda.min, " (+1 SE):", x$lambda.1se)
    cat("\n")
    invisible(x)
}


