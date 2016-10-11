#' @include glmnetUtils.r
NULL

#' @name cvAlpha.glmnet
#' @export
cvAlpha.glmnet <- function(x, ...)
UseMethod("cvAlpha.glmnet")

#' Do elastic net cross-validation for alpha and lambda simultaneously
#'
#' @param x A matrix of predictor variables; or for the plotting methods, an object returned by \code{cvAlpha.glmnet}.
#' @param y A response vector or matrix (for a multinomial response).
#' @param alpha A vector of alpha values for which to do cross-validation. The default is a sequence of 11 values more closely spaced around alpha = 0. For the \code{predict} and \code{coef} methods, the specific value of alpha for which to return predictions/regression coefficients.
#' @param nfolds The number of cross-validation folds to use. Defaults to 10.
#' @param outerParallel Method of parallelising the outer loop over alpha. See 'Details' below. If \code{NULL}, the loop is run sequentially.
#' @param checkInnerParallel If the outer loop is run in parallel, check that the inner loop over lambda will not be in contention for cores.
#'
#' @details
#' The \code{cvAlpha.glmnet} function does simultaneous cross-validation for both the alpha and lambda parameters in an elastic net model. It follows the procedure outlined in the documentation for \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}: it creates a vector \code{foldid} allocating the observations into folds, and then calls \code{cv.glmnet} in a loop over different values of alpha, but the same values of \code{foldid} each time.
#'
#' Optionally this loop over alpha can be parallelised; currently, \code{cvAlpha.glmnet} knows about two methods of doing so:
#' \itemize{
#'   \item Via \code{\link{parLapply}} in the \code{parallel} package. To use this, set \code{outerParallel} to a valid cluster object created by \code{\link{makeCluster}}.
#'   \item Via \code{\link{rxExec}} as supplied by Microsoft R Server's \code{RevoScaleR} package. To use this, set \code{outerParallel} to a valid compute context created by \code{\link{RxComputeContext}}, or a character string specifying such a context.
#' }
#' If the outer loop is run in parallel, \code{cvAlpha.glmnet} can check if the inner loop (over lambda) is also set to run in parallel, and disable this if it would lead to contention for cores. This is done if it is likely that the parallelisation is local on a multicore machine, ie if \code{outerParallel} is a \code{SOCKcluster} object running on \code{"localhost"}, or if the supplied compute context is local parallel.
#'
#' @seealso
#' \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}
#' @rdname cvAlpha.glmnet
#' @method cvAlpha.glmnet default
#' @importFrom parallel parLapply
#' @export
cvAlpha.glmnet.default <- function(x, y, alpha=seq(0, 1, len=11)^3, nfolds=10, ..., outerParallel=NULL,
                                   checkInnerParallel=TRUE)
{
    .cvfunc <- function(a, xmat, y, nfolds, foldid, ...)
    {
        require(glmnet)
        cv.glmnet(x, y, alpha=a, nfolds=nfolds, foldid=foldid, ...)
    }

    .chkPar <- function()
    {
        if(checkInnerParallel && isTRUE(dotargs$parallel))
        {
            warning("Setting parallel to FALSE for inner loop over lambda", call.=FALSE)
            dotargs$parallel <<- FALSE
        }
    }

    foldid <- sample(nfolds, nrow(x), replace=TRUE)
    dotargs <- list(...)

    # do not explicitly import RevoScaleR; this allows use with non-Microsoft R installs
    rxContexts <- c("RxLocalSeq", "local", "RxLocalParallel", "localpar", "RxHadoopMR", "hadoopmr", "RxSpark", "spark",
                    "RxInTeradata", "teradata", "RxForeachDoPar", "dopar", "RxInSqlServer", "sqlserver")
    if(is.character(outerParallel) && (outerParallel %in% rxContexts) && require(RevoScaleR))
        outerParallel <- RevoScaleR::RxComputeContext(outerParallel)

    lst <- if(inherits(outerParallel, "cluster"))
    {
        if(inherits(outerParallel, "SOCKcluster") && identical(outerParallel[[1]]$host, "localhost"))
            .chkPar()
        do.call(parallel::parLapply, c(list(outerParallel, alpha, .cvfunc, xmat=x, y=y, nfolds=nfolds, foldid=foldid),
                dotargs))
    }
    else if(is(outerParallel, "RxComputeContext"))
    {
        oldContext <- RevoScaleR::rxSetComputeContext(outerParallel)
        on.exit(RevoScaleR::rxSetComputeContext(oldContext))
        if(is(outerParallel, "RxLocalParallel"))
            .chkPar()
        do.call(RevoScaleR::rxExec, c(list(.cvfunc, a=RevoScaleR::rxElemArg(alpha), xmat=x, y=y, nfolds=nfolds,
                foldid=foldid), dotargs))
    }
    else if(is.null(outerParallel))
    {
        lapply(alpha, .cvfunc, xmat=x, y=y, nfolds=nfolds, foldid=foldid, ...)
    }
    else stop("unknown value for outerParallel")

    out <- list(alpha=alpha, nfolds=nfolds, modlist=lst)
    class(out) <- "cvAlpha.glmnet"
    out
}

#' @param formula A model formula; interaction terms are allowed and will be expanded per the usual rules for linear models.
#' @param data A data frame or matrix containing the variables in the formula.
#' @param weights An optional vector of case weights to be used in the fitting process. If missing, defaults to an unweighted fit.
#' @param offset An optional vector of offsets, an \emph{a priori} known component to be included in the linear predictor.
#' @param subset An optional vector specifying the subset of observations to be used to fit the model.
#' @param na.action A function which indicates what should happen when the data contains missing values. For the \code{predict} method, \code{na.action = na.pass} will predict missing values with \code{NA}; \code{na.omit} or \code{na.exclude} will drop them.
#' @param drop.unused.levels Should factors have unused levels dropped? Defaults to \code{FALSE}.
#' @param xlev A named list of character vectors giving the full set of levels to be assumed for each factor.
#' @param sparse Should the model matrix be in sparse format? This can save memory when dealing with many factor variables, each with many levels (but see the warning below).
#' @param use.model.frame Should the base \code{\link{model.frame}} function be used when constructing the model matrix? This is the standard method that most R modelling functions use, but has some disadvantages. The default is to avoid \code{model.frame} and construct the model matrix term-by-term; see \link[=glmnet.model.matrix]{discussion}.
#'
#' @details
#' The formula method works in a similar manner to \code{lm}, \code{glm} and other modelling functions. The arguments are used to generate a \emph{model frame}, which is a data frame augmented with information about the roles the columns play in fitting the model. This is then turned into a \emph{model matrix} and a response vector, which are passed to \code{glmnet::glmnet} along with any arguments in \code{...}. If \code{sparse} is TRUE, then \code{Matrix::sparse.model.matrix} is used instead of \code{stats::model.matrix} to create the model matrix.
#'
#' @section Value:
#' For \code{cvAlpha.glmnet.default}, an object of class \code{cvAlpha.glmnet}. This is a list containing the following:
#' \itemize{
#'   \item \code{alpha} The vector of alpha values
#'   \item \code{nfolds} The number of folds
#'   \item \code{modlist} A list of \code{cv.glmnet} objects, containing the cross-validation results for each value of alpha
#' }
#' The function \code{cvAlpha.glmnet.formula} adds a few more components to the above, to facilitate working with formulas.
#'
#' @examples
#' cva <- cvAlpha.glmnet(mpg ~ ., data=mtcars)
#' predict(cva, mtcars, alpha=1)
#'
#' \dontrun{
#'
#' # Leukemia example dataset from Trevor Hastie's website
#' download.file("http://web.stanford.edu/~hastie/glmnet/glmnetData/Leukemia.RData",
#'               "Leukemia.RData")
#' load("Leukemia.Rdata")
#' leuk <- do.call(data.frame, Leukemia)
#' leuk.cva <- cvAlpha.glmnet(y ~ ., leuk, family="binomial")
#' leuk.pred <- predict(leuk.cva, leuk, which=6)
#' }
#' @rdname cvAlpha.glmnet
#' @method cvAlpha.glmnet formula
#' @export
cvAlpha.glmnet.formula <- function(formula, data, ..., weights=NULL, offset=NULL, subset=NULL,
                                   na.action=getOption("na.action"), drop.unused.levels=FALSE, xlev=NULL, 
                                   sparse=FALSE, use.model.frame=FALSE)
{
    # must use NSE to get model.frame emulation to work
    cl <- match.call(expand.dots=FALSE)
    cl[[1]] <- if(use.model.frame)
        makeModelComponentsMF
    else makeModelComponents
    xy <- eval.parent(cl)

    model <- cvAlpha.glmnet.default(xy$x, xy$y, weights=xy$weights, offset=xy$offset, ...)
    model$call <- match.call()
    model$terms <- xy$terms
    model$sparse <- sparse
    model$use.model.frame <- use.model.frame
    model$na.action <- na.action
    class(model) <- c("cvAlpha.glmnet.formula", class(model))
    model
}


#' @param object For the \code{predict} and \code{coef} methods, an object returned by \code{cvAlpha.glmnet}.
#' @param newx For the \code{predict} method, a matrix of predictor variables.
#' @param which An alternative way of specifying alpha; the index number of the desired value within the alpha vector. If both \code{which} and \code{alpha} are supplied, the former takes precedence.
#' @param ... Further arguments to be passed to lower-level functions. In the case of \code{cvAlpha.glmnet}, these arguments are passed to \code{cv.glmnet}; for \code{predict} and \code{coef}, they are passed to \code{predict.cv.glmnet}; and for \code{plot} and \code{plotLambda}, to \code{plot}.
#'
#' @details
#' The \code{predict} method computes predictions for a specific alpha value given a \code{cvAlpha.glmnet} object. It looks up the supplied alpha (possibly supplied indirectly via the \code{which} argument) in the object's stored \code{alpha} vector, and calls \code{glmnet:::predict.cv.glmnet} on the corresponding \code{cv.glmnet} fit. All the arguments to that function are (or should be) supported.
#'
#' @seealso
#' \code{\link[glmnet:predict.cv.glmnet]{glmnet:::predict.cv.glmnet}}, \code{\link[glmnet:coef.cv.glmnet]{glmnet:::coef.cv.glmnet}}
#'
#' @method predict cvAlpha.glmnet
#' @rdname cvAlpha.glmnet
#' @export
predict.cvAlpha.glmnet <- function(object, newx, alpha, which=match(TRUE, abs(object$alpha - alpha) < 1e-8), ...)
{
    if(is.na(which))
        stop("supplied alpha value not found")
    mod <- object$modlist[[which]]
    predict(mod, newx, ...)
}


#' @param newdata For the \code{predict} and \code{coef} methods, a data frame containing the observations for which to calculate predictions.
#'
#' @section Value:
#' For the \code{predict} method, a vector or matrix of predicted values.
#'
#' @method predict cvAlpha.glmnet.formula
#' @rdname cvAlpha.glmnet
#' @export
predict.cvAlpha.glmnet.formula <- function(object, newdata, alpha, which=match(TRUE, abs(object$alpha - alpha) < 1e-8),
                                           na.action=na.pass, ...)
{
    if(object$use.model.frame)
    {
        tt <- delete.response(object$terms)
        newdata <- model.frame(tt, newdata, na.action=na.action)
        x <- if(object$sparse)
            Matrix::sparse.model.matrix(tt, newdata)[, -1, drop=FALSE]
        else model.matrix(tt, newdata)[, -1, drop=FALSE]
    }
    else
    {
        rhs <- object$terms
        x <- makeModelComponents(rhs, newdata, na.action=na.action)$x
    }

    predict.cvAlpha.glmnet(object, x, which=which, ...)
}


#' @details
#' The \code{coef} method is similar, returning the coefficients for the selected alpha value via \code{glmnet:::coef.cv.glmnet}.
#'
#' @section Value:
#' For the \code{coef} method, a vector of regularised regression coefficients.
#'
#' @method coef cvAlpha.glmnet
#' @rdname cvAlpha.glmnet
#' @export
coef.cvAlpha.glmnet <- function(object, alpha, which=match(TRUE, abs(object$alpha - alpha) < 1e-8), ...)
{
    if(is.na(which))
        stop("supplied alpha value not found")
    mod <- object$modlist[[which]]
    coef(mod, ...)
}


#' @details
#' The plot method for \code{cvAlpha.glmnet} objects plots the average cross-validated loss by lambda, for each value of alpha. Each line represents one \code{cv.glmnet} fit, corresponding to one value of alpha. Note that the specific lambda values can vary substantially by alpha, hence no attempt is made to put them on a common scale. Instead, the lines are simply the cross-validated loss results in sequential order, with the smallest lambda values for each fit on the left.
#'
#' @seealso
#' \code{\link{cvAlpha.glmnet}}, \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}, \code{\link{plot}}
#' @method plot cvAlpha.glmnet
#' @rdname cvAlpha.glmnet
#' @export
plot.cvAlpha.glmnet <- function(x, ...)
{
    n <- length(x$modlist)
    cvm <- sapply(x$modlist, "[[", "cvm", simplify=FALSE)
    oldPal <- palette(topo.colors(n))
    on.exit(palette(oldPal))
    rng <- do.call(range, cvm)
    ylab <- x$modlist[[1]]$name
    plot(cvm[[1]], ylim=rng, xlab="Lambda rank", ylab=ylab, type='n', ...)
    for(j in seq_along(cvm)) lines(rev(cvm[[j]]), col=j)
    invisible(x)
}


#' @details
#' The \code{plotLambda} function gives the best (lowest) cross-validated loss for each value of alpha.
#' @rdname cvAlpha.glmnet
#' @export
plotLambda <- function(x, ...)
{
    if(!inherits(x, "cvAlpha.glmnet"))
        stop("not a cvAlpha.glmnet object")
    alpha <- x$alpha
    cvm <- sapply(x$modlist, function(mod) {
        mod$cvm[mod$lambda == mod$lambda.min]
    })
    plot(alpha, cvm, ylab="lambda.min", ...)
    invisible(x)
}

