#' @include glmnetUtils.r
NULL

#' @name glmnet
#' @export
glmnet <- function(x, ...)
UseMethod("glmnet")

#' @rdname glmnet
#' @method glmnet default
#' @export
glmnet.default <- function(x, y, ...)
{
    cl <- match.call()
    cl[[1]] <- quote(glmnet::glmnet)
    obj <- glmnet::glmnet(x, y, ...)
    obj$call <- cl
    obj
}


#' Formula interface for elastic net modelling with glmnet
#'
#' @param x For the default method, a matrix of predictor variables.
#' @param y For the default method, a response vector or matrix (for a multinomial response).
#' @param formula A model formula; interaction terms are allowed and will be expanded per the usual rules for linear models.
#' @param data A data frame or matrix containing the variables in the formula.
#' @param weights An optional vector of case weights to be used in the fitting process. If missing, defaults to an unweighted fit.
#' @param offset An optional vector of offsets, an _a priori_ known component to be included in the linear predictor.
#' @param subset An optional vector specifying the subset of observations to be used to fit the model.
#' @param na.action A function which indicates what should happen when the data contains missing values. For the `predict` method, `na.action = na.pass` will predict missing values with `NA`; `na.omit` or `na.exclude` will drop them.
#' @param drop.unused.levels Should factors have unused levels dropped? Defaults to `FALSE`.
#' @param xlev A named list of character vectors giving the full set of levels to be assumed for each factor.
#' @param alpha The elastic net mixing parameter. See [glmnet::glmnet] for more details.
#' @param sparse Should the model matrix be in sparse format? This can save memory when dealing with many factor variables, each with many levels.
#' @param use.model.frame Should the base [model.frame] function be used when constructing the model matrix? This is the standard method that most R modelling functions use, but has some disadvantages. The default is to avoid `model.frame` and construct the model matrix term-by-term; see [discussion][glmnet.model.matrix].
#' @param ... For `glmnet.formula` and `glmnet.default`, other arguments to be passed to [glmnet::glmnet]; for the `predict` and `coef` methods, arguments to be passed to their counterparts in package glmnet.
#'
#' @details
#' The `glmnet` function in this package is an S3 generic with a formula and a default method. The former calls the latter, and the latter is simply a direct call to the `glmnet` function in package glmnet. All the arguments to `glmnet::glmnet` are (or should be) supported.
#'
#' The code works in a similar manner to `lm`, `glm` and other modelling functions. The arguments are used to generate a _model frame_, which is a data frame augmented with information about the roles the columns play in fitting the model. This is then turned into a _model matrix_ and a response vector, which are passed to `glmnet::glmnet` along with any arguments in `...`. If `sparse` is TRUE, then `Matrix::sparse.model.matrix` is used instead of `stats::model.matrix` to create the model matrix.
#'
#' The `predict` and `coef` methods are wrappers for the corresponding methods in the glmnet package. The former constructs a predictor model matrix from its `newdata` argument and passes that as the `newx` argument to `glmnet:::predict.glmnet`.
#'
#' @section Value:
#' For `glmnet.formula`, an object of class `glmnet.formula`. This is basically the same object created by `glmnet::glmnet`, but with extra components to allow formula usage.
#'
#' @seealso
#' [glmnet::glmnet], [glmnet::predict.glmnet], [glmnet::coef.glmnet], [model.frame], [model.matrix]
#' 
#' @examples
#' glmnet(mpg ~ ., data=mtcars)
#'
#' glmnet(Species ~ ., data=iris, family="multinomial")
#'
#' \dontrun{
#'
#' # Leukemia example dataset from Trevor Hastie's website
#' download.file("http://web.stanford.edu/~hastie/glmnet/glmnetData/Leukemia.RData",
#'               "Leukemia.RData")
#' load("Leukemia.Rdata")
#' leuk <- do.call(data.frame, Leukemia)
#' glmnet(y ~ ., leuk, family="binomial")
#' }
#' @rdname glmnet
#' @method glmnet formula
#' @importFrom glmnet glmnet
#' @export
glmnet.formula <- function(formula, data, alpha=1, ..., weights=NULL, offset=NULL, subset=NULL,
                           na.action=getOption("na.action"), drop.unused.levels=FALSE, xlev=NULL,
                           sparse=FALSE, use.model.frame=FALSE)
{
    # must use NSE to get model.frame emulation to work
    cl <- match.call(expand.dots=FALSE)
    cl[[1]] <- if(use.model.frame)
        makeModelComponentsMF
    else makeModelComponents
    xy <- eval.parent(cl)

    model <- glmnet::glmnet(x=xy$x, y=xy$y, weights=xy$weights, offset=xy$offset, alpha=alpha, ...)
    model$call <- match.call()
    model$terms <- xy$terms
    model$xlev <- xy$xlev
    model$alpha <- alpha
    model$sparse <- sparse
    model$use.model.frame <- use.model.frame
    model$na.action <- na.action
    class(model) <- c("glmnet.formula", class(model))
    model
}


#' @param object For the `predict` and `coef` methods, an object of class `glmnet.formula`.
#' @param newdata For the `predict` method, a data frame containing the observations for which to calculate predictions.
#' @rdname glmnet
#' @importFrom Matrix sparse.model.matrix
#' @export
#' @method predict glmnet.formula
predict.glmnet.formula <- function(object, newdata, offset=NULL, na.action=na.pass, ...)
{
    if(!inherits(object, "glmnet.formula"))
        stop("invalid glmnet.formula object")

    # must use NSE to get model.frame emulation to work
    cl <- match.call(expand.dots=FALSE)
    cl$formula <- delete.response(object$terms)
    cl$data <- cl$newdata
    cl$newdata <- NULL
    cl$xlev <- object$xlev
    cl[[1]] <- if(object$use.model.frame)
        makeModelComponentsMF
    else makeModelComponents

    xy <- eval.parent(cl)
    x <- xy$x
    offset <- xy$offset

    class(object) <- class(object)[-1]
    predict(object, x, offset=offset, ...)
}

#' @rdname glmnet
#' @export
#' @method coef glmnet.formula
coef.glmnet.formula <- function(object, ...)
{
    if(!inherits(object, "glmnet.formula"))
        stop("invalid glmnet.formula object")
    class(object) <- class(object)[-1]
    coef(object, ...)
}


#' @param digits Significant digits in printed output.
#' @param print.deviance.ratios Whether to print the table of deviance ratios, as per [glmnet::print.glmnet].
#' @rdname glmnet
#' @export
#' @method print glmnet.formula
print.glmnet.formula <- function(x, digits=max(3, getOption("digits") - 3), print.deviance.ratios=FALSE, ...)
{
    cat("Call:\n")
    dput(x$call)
    cat("\nModel fitting options:")
    cat("\n    Sparse model matrix:", x$sparse)
    cat("\n    Use model.frame:", x$use.model.frame)
    cat("\n    Alpha:", x$alpha)
    cat("\n    Lambda summary:\n")
    print(summary(x$lambda))
    if(print.deviance.ratios)
    {
        cat("\nDeviance ratios:\n")
        print(cbind(Df=x$df, `%Dev`=signif(x$dev.ratio, digits), Lambda=signif(x$lambda, digits)))
    }
    cat("\n")
    invisible(x)
}

