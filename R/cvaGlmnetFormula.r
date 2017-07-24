#' @include glmnetUtils.r
NULL

#' @name cva.glmnet
#' @export
cva.glmnet <- function(x, ...)
UseMethod("cva.glmnet")

#' Do elastic net cross-validation for alpha and lambda simultaneously
#'
#' @param x A matrix of predictor variables; or for the plotting methods, an object returned by \code{cva.glmnet}.
#' @param y A response vector or matrix (for a multinomial response).
#' @param alpha A vector of alpha values for which to do cross-validation. The default is a sequence of 11 values more closely spaced around alpha = 0. For the \code{predict} and \code{coef} methods, the specific value of alpha for which to return predictions/regression coefficients.
#' @param nfolds The number of cross-validation folds to use. Defaults to 10.
#' @param foldid Vector of fold IDs for cross-validation. See \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}.
#' @param outerParallel Method of parallelising the outer loop over alpha. See 'Details' below. If \code{NULL}, the loop is run sequentially.
#' @param checkInnerParallel If the outer loop is run in parallel, check that the inner loop over lambda will not be in contention for cores.
#'
#' @details
#' The \code{cva.glmnet} function does simultaneous cross-validation for both the alpha and lambda parameters in an elastic net model. It follows the procedure outlined in the documentation for \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}: it creates a vector \code{foldid} allocating the observations into folds, and then calls \code{cv.glmnet} in a loop over different values of alpha, but the same values of \code{foldid} each time.
#'
#' Optionally this loop over alpha can be parallelised; currently, \code{cva.glmnet} knows about two methods of doing so:
#' \itemize{
#'   \item Via \code{\link{parLapply}} in the \code{parallel} package. To use this, set \code{outerParallel} to a valid cluster object created by \code{\link{makeCluster}}.
#'   \item Via \code{rxExec} as supplied by Microsoft R Server's \code{RevoScaleR} package. To use this, set \code{outerParallel} to a valid compute context created by \code{RxComputeContext}, or a character string specifying such a context.
#' }
#' If the outer loop is run in parallel, \code{cva.glmnet} can check if the inner loop (over lambda) is also set to run in parallel, and disable this if it would lead to contention for cores. This is done if it is likely that the parallelisation is local on a multicore machine, ie if \code{outerParallel} is a \code{SOCKcluster} object running on \code{"localhost"}, or if the supplied compute context is local parallel.
#'
#' @seealso
#' \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}
#' @rdname cva.glmnet
#' @method cva.glmnet default
#' @importFrom parallel parLapply
#' @importFrom glmnet cv.glmnet
#' @export
cva.glmnet.default <- function(x, y, alpha=seq(0, 1, len=11)^3, nfolds=10, foldid=sample(nfolds, nrow(x), replace=TRUE),
                               ..., outerParallel=NULL, checkInnerParallel=TRUE)
{
    .cvfunc <- function(a, xmat, y, nfolds, foldid, ...)
    {
        glmnet::cv.glmnet(x, y, alpha=a, nfolds=nfolds, foldid=foldid, ...)
    }

    .chkPar <- function()
    {
        if(checkInnerParallel && isTRUE(dotargs$parallel))
        {
            warning("Setting parallel to FALSE for inner loop over lambda", call.=FALSE)
            dotargs$parallel <<- FALSE
        }
    }

    #foldid <- sample(nfolds, nrow(x), replace=TRUE)
    if(length(foldid) != nrow(x) || !is.numeric(foldid))
        stop("invalid foldid specified")
    dotargs <- list(...)

    # do not explicitly import RevoScaleR; this allows use with non-Microsoft R installs
    # hide Revo from R CMD check, in case we ever want to put this on CRAN
    rxContexts <- c("RxLocalSeq", "local", "RxLocalParallel", "localpar", "RxHadoopMR", "hadoopmr", "RxSpark", "spark",
                    "RxInTeradata", "teradata", "RxForeachDoPar", "dopar", "RxInSqlServer", "sqlserver")
    if(is.character(outerParallel) && (outerParallel %in% rxContexts) && eval(parse(text="require(RevoScaleR)")))
        outerParallel <- eval(parse(text="RevoScaleR::RxComputeContext"))(outerParallel)

    lst <- if(inherits(outerParallel, "cluster"))
    {
        if(inherits(outerParallel, "SOCKcluster") && identical(outerParallel[[1]]$host, "localhost"))
            .chkPar()
        do.call(parallel::parLapply, c(list(outerParallel, alpha, .cvfunc, xmat=x, y=y, nfolds=nfolds, foldid=foldid),
                dotargs))
    }
    else if(inherits(outerParallel, "RxComputeContext"))  # again hide Revo from checks
    {
        eval(parse(text='
        oldContext <- rxSetComputeContext(outerParallel)
        on.exit(rxSetComputeContext(oldContext))
        if(is(outerParallel, "RxLocalParallel"))
            .chkPar()
        do.call(rxExec, c(list(.cvfunc, a=rxElemArg(alpha), xmat=x, y=y, nfolds=nfolds,
                foldid=foldid), dotargs))'))
    }
    else if(is.null(outerParallel))
    {
        lapply(alpha, .cvfunc, xmat=x, y=y, nfolds=nfolds, foldid=foldid, ...)
    }
    else stop("unknown value for outerParallel")

    out <- list(alpha=alpha, nfolds=nfolds, modlist=lst)
    class(out) <- "cva.glmnet"
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
#' For \code{cva.glmnet.default}, an object of class \code{cva.glmnet}. This is a list containing the following:
#' \itemize{
#'   \item \code{alpha} The vector of alpha values
#'   \item \code{nfolds} The number of folds
#'   \item \code{modlist} A list of \code{cv.glmnet} objects, containing the cross-validation results for each value of alpha
#' }
#' The function \code{cva.glmnet.formula} adds a few more components to the above, to facilitate working with formulas.
#'
#' @examples
#' cva <- cva.glmnet(mpg ~ ., data=mtcars)
#' predict(cva, mtcars, alpha=1)
#'
#' \dontrun{
#'
#' # Leukemia example dataset from Trevor Hastie's website
#' download.file("http://web.stanford.edu/~hastie/glmnet/glmnetData/Leukemia.RData",
#'               "Leukemia.RData")
#' load("Leukemia.Rdata")
#' leuk <- do.call(data.frame, Leukemia)
#' leuk.cva <- cva.glmnet(y ~ ., leuk, family="binomial")
#' leuk.pred <- predict(leuk.cva, leuk, which=6)
#' }
#' @rdname cva.glmnet
#' @method cva.glmnet formula
#' @export
cva.glmnet.formula <- function(formula, data, ..., weights=NULL, offset=NULL, subset=NULL,
                                   na.action=getOption("na.action"), drop.unused.levels=FALSE, xlev=NULL, 
                                   sparse=FALSE, use.model.frame=FALSE)
{
    # must use NSE to get model.frame emulation to work
    cl <- match.call(expand.dots=FALSE)
    cl[[1]] <- if(use.model.frame)
        makeModelComponentsMF
    else makeModelComponents
    xy <- eval.parent(cl)

    model <- cva.glmnet.default(xy$x, xy$y, weights=xy$weights, offset=xy$offset, ...)
    model$call <- match.call()
    model$terms <- xy$terms
    model$xlev <- xy$xlev
    model$sparse <- sparse
    model$use.model.frame <- use.model.frame
    model$na.action <- na.action
    class(model) <- c("cva.glmnet.formula", class(model))
    model
}


#' @param object For the \code{predict} and \code{coef} methods, an object returned by \code{cva.glmnet}.
#' @param newx For the \code{predict} method, a matrix of predictor variables.
#' @param which An alternative way of specifying alpha; the index number of the desired value within the alpha vector. If both \code{which} and \code{alpha} are supplied, the former takes precedence.
#' @param ... Further arguments to be passed to lower-level functions. In the case of \code{cva.glmnet}, these arguments are passed to \code{cv.glmnet}; for \code{predict} and \code{coef}, they are passed to \code{predict.cv.glmnet}; and for \code{plot} and \code{minlossplot}, to \code{plot}.
#'
#' @details
#' The \code{predict} method computes predictions for a specific alpha value given a \code{cva.glmnet} object. It looks up the supplied alpha (possibly supplied indirectly via the \code{which} argument) in the object's stored \code{alpha} vector, and calls \code{glmnet:::predict.cv.glmnet} on the corresponding \code{cv.glmnet} fit. All the arguments to that function are (or should be) supported.
#'
#' @seealso
#' \code{\link[glmnet:predict.cv.glmnet]{glmnet:::predict.cv.glmnet}}, \code{\link[glmnet:coef.cv.glmnet]{glmnet:::coef.cv.glmnet}}
#'
#' @method predict cva.glmnet
#' @rdname cva.glmnet
#' @export
predict.cva.glmnet <- function(object, newx, alpha, which=match(TRUE, abs(object$alpha - alpha) < 1e-8), ...)
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
#' @method predict cva.glmnet.formula
#' @rdname cva.glmnet
#' @export
predict.cva.glmnet.formula <- function(object, newdata, alpha, which=match(TRUE, abs(object$alpha - alpha) < 1e-8),
                                           na.action=na.pass, ...)
{
    # must use NSE to get model.frame emulation to work
    cl <- match.call(expand.dots=FALSE)
    cl$formula <- makeFormulaRhs(object$terms)
    cl$data <- cl$newdata
    cl$newdata <- NULL
    cl$xlev <- object$xlev
    cl[[1]] <- if(object$use.model.frame)
        makeModelComponentsMF
    else makeModelComponents

    xy <- eval.parent(cl)
    x <- xy$x
    offset <- xy$offset

    predict.cva.glmnet(object, x, which=which, ...)
}


#' @details
#' The \code{coef} method is similar, returning the coefficients for the selected alpha value via \code{glmnet:::coef.cv.glmnet}.
#'
#' @section Value:
#' For the \code{coef} method, a vector of regularised regression coefficients.
#'
#' @method coef cva.glmnet
#' @rdname cva.glmnet
#' @export
coef.cva.glmnet <- function(object, alpha, which=match(TRUE, abs(object$alpha - alpha) < 1e-8), ...)
{
    if(is.na(which))
        stop("supplied alpha value not found")
    mod <- object$modlist[[which]]
    coef(mod, ...)
}


#' @rdname cva.glmnet
#' @method print cva.glmnet.formula
#' @export
print.cva.glmnet.formula <- function(x, ...)
{
    cat("Call:\n")
    dput(x$call)
    cat("\nModel fitting options:")
    cat("\n    Sparse model matrix:", x$sparse)
    cat("\n    Use model.frame:", x$use.model.frame)
    cat("\n    Alpha values:", x$alpha)
    cat("\n    Number of crossvalidation folds for lambda:", x$nfolds)
    cat("\n")
    invisible(x)
}


#' @details
#' The plot method for \code{cva.glmnet} objects plots the average cross-validated loss by lambda, for each value of alpha. Each line represents one \code{cv.glmnet} fit, corresponding to one value of alpha. Note that the specific lambda values can vary substantially by alpha.
#'
#' @seealso
#' \code{\link{cva.glmnet}}, \code{\link[glmnet:cv.glmnet]{glmnet::cv.glmnet}}, \code{\link{plot}}
#' @method plot cva.glmnet
#' @rdname cva.glmnet
#' @export
plot.cva.glmnet <- function(x, ...)
{
    n <- length(x$modlist)
    cvm <- sapply(x$modlist, "[[", "cvm", simplify=FALSE)
    oldPal <- palette(topo.colors(n))
    on.exit(palette(oldPal))
    ylab <- x$modlist[[1]]$name
    xlst <- lapply(x$modlist, "[[", "lambda")
    ylst <- lapply(x$modlist, "[[", "cvm")
    xlim <- log(range(xlst))
    ylim <- range(ylst)
    plot(NA, xlim=xlim, ylim=ylim, xlab="log Lambda", ylab=x$modlist[[1]]$name, type="n", ...)
    for(i in seq_along(cvm)) lines(log(xlst[[i]]), ylst[[i]], col=i)
    invisible(x)
}


#' @rdname cva.glmnet
#' @export
minlossplot <- function(x, ...)
UseMethod("minlossplot")


#' @param cv.type For \code{minlossplot}, which cross-validated loss value to plot for each value of alpha. This can be either \code{"min"} which is the minimum loss, or \code{"1se"} which is the highest loss within 1 standard error of the minimum. The default is \code{"1se"}.
#'
#' @details
#' The \code{minlossplot} function gives the best (lowest) cross-validated loss for each value of alpha.
#'
#' @rdname cva.glmnet
#' @export
minlossplot.cva.glmnet <- function(x, ..., cv.type=c("1se", "min"))
{
    alpha <- x$alpha
    cv.type <- match.arg(cv.type)
    cv.type <- paste0("lambda.", cv.type)
    cvm <- sapply(x$modlist, function(mod) {
        mod$cvm[mod$lambda == mod[[cv.type]]]
    })
    plot(alpha, cvm, ylab="CV loss", ...)
    invisible(x)
}

