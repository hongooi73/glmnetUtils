#' @name glmnet.model.matrix
#' @aliases glmnet.modelMatrix glmnet.model.frame glmnet.modelFrame
#' @title Model matrix options for glmnet
#'
#' @description
#' This page describes the options available for generating the model matrix.
#'
#' @details
#' There are two ways in which glmnetUtils can generate a model matrix out of a formula and data frame. The first is to use the standard R machinery comprising [model.frame]and [model.matrix]; and the second is to build the matrix one variable at a time. These options are discussed and contrasted below.
#'
#' @section Using `model.frame`:
#' This is the simpler option, and the one that is most compatible with other R modelling functions. The `model.frame` function takes a formula and data frame and returns a _model frame_: a data frame with special information attached that lets R make sense of the terms in the formula. For example, if a formula includes an interaction term, the model frame will specify which columns in the data relate to the interaction, and how they should be treated. Similarly, if the formula includes expressions like `exp(x)` or `I(x^2)` on the RHS, `model.frame` will evaluate these expressions and include them in the output.
#'
#' The major disadvantage of using `model.frame` is that it generates a [terms] object, which encodes how variables and interactions are organised. One of the attributes of this object is a matrix with one row per variable, and one column per main effect and interaction. At minimum, this is (approximately) a \eqn{p \times p}{p x p} square matrix where \eqn{p} is the number of main effects in the model. For wide datasets with \eqn{p > 10000}, this matrix can approach or exceed a gigabyte in size. Even if there is enough memory to store such an object, generating the model matrix can take a significant amount of time.
#'
#' Another issue with the standard R approach is the treatment of factors. Normally, `model.matrix` will turn an \eqn{N}-level factor into an indicator matrix with \eqn{N-1} columns, with one column being dropped. This is necessary for unregularised models as fit with `lm` and `glm`, since the full set of \eqn{N} columns is linearly dependent. With the usual [treatment contrasts][contr.treatment], the interpretation is that the dropped column represents a baseline level, while the coefficients for the other columns represent the difference in the response relative to the baseline.
#'
#' This may not be appropriate for a regularised model as fit with glmnet. The regularisation procedure shrinks the coefficients towards zero, which forces the estimated differences from the baseline to be smaller. But this only makes sense if the baseline level was chosen beforehand, or is otherwise meaningful as a default; otherwise it is effectively making the levels more similar to an arbitrarily chosen level.
#'
#' @section Manually building the model matrix:
#' To deal with the problems above, glmnetUtils by default will avoid using `model.frame`, instead building up the model matrix term-by-term. This avoids the memory cost of creating a `terms` object, and can be noticeably faster than the standard approach. It will also include one column in the model matrix for _all_ levels in a factor; that is, no baseline level is assumed. In this situation, the coefficients represent differences from the overall mean response, and shrinking them to zero _is_ meaningful (usually).
#'
#' This works in an additive fashion, ie the formula `~ a + b:c + d*e` is treated as consisting of three terms, `a`, `b:c` and `d*e` each of which is processed independently of the others. A dot in the formula includes all main effect terms, ie `~ . + a:b + f(x)` expands to `~ a + b + x + a:b + f(x)` (assuming a, b and x are the only columns in the data). Note that a formula like `~ (a + b) + (c + d)` will be treated as two terms, `a + b` and `c + d`.
#'
#' The code can handle fairly complex formulas, but it is not as sophisticated as base `model.frame` and `model.matrix`. In particular, terms that are to be _omitted_ from the model must be at the end of the formula: `~ . - c` works, but not `~ -c + .`.
NULL


#' @importFrom Matrix sparse.model.matrix
# short, simple function that unavoidably creates a pxp square matrix (!)
makeModelComponentsMF <- function(formula, data, weights=NULL, offset=NULL, subset=NULL, na.action=getOption("na.action"),
                                  drop.unused.levels=FALSE, xlev=NULL, sparse=FALSE, ...)
{
    # more NSE hackery
    cl <- match.call(expand.dots=FALSE)
    cl$sparse <- cl$`...` <- NULL
    cl[[1]] <- quote(stats::model.frame)
    mf <- eval.parent(cl)

    x <- if(!is.null(sparse) && sparse)
        Matrix::sparse.model.matrix(attr(mf, "terms"), mf)[, -1, drop=FALSE]
    else model.matrix(attr(mf, "terms"), mf)[, -1, drop=FALSE]
    y <- model.response(mf)
    weights <- model.extract(mf, "weights")
    offset <- model.extract(mf, "offset")
    if(is.null(weights))
        weights <- rep(1, nrow(mf))
    xlev <- .getXlevels(attr(mf, "terms"), mf)

    list(x=x, y=y, weights=weights, offset=offset, terms=terms(mf), xlev=xlev)
}


# emulate model.frame + model.matrix, without constructing a terms object
makeModelComponents <- function(formula, data, weights=NULL, offset=NULL, subset=NULL, na.action=getOption("na.action"),
                                drop.unused.levels=FALSE, xlev=NULL, sparse=FALSE, ...)
{
    if(!is.data.frame(data))
    {
        data <- as.data.frame(data)
        warning("input data was converted to data.frame")
    }

    rhs <- formula[[length(formula)]]
    lhs <- if(length(formula) == 3) formula[[2]] else NULL

    lhsVars <- all.vars(lhs)
    rhsTerms <- additiveTerms(rhs, base::setdiff(names(data), lhsVars))

    # rebuild rhs to allow for . in formula
    rhs <- rebuildRhs(rhsTerms)
    rhsVars <- all.vars(rhs)

    if(!missing(subset))
    {
        subset <- substitute(subset)
        if(!is.null(subset))
            data <- data[eval(subset, data, parent.frame()), , drop=FALSE]
    }

    offset <- substitute(offset)
    offsetVals <- eval(offset, data, parent.frame())

    if(!missing(weights) && !is.null(weights <- substitute(weights)))  # yes, assignment in an if
        weightVals <- eval(weights, data, parent.frame())
    else weightVals <- rep(1, nrow(data))

    if(!is.function(na.action))
        na.action <- get(na.action, mode="function")
    if(!is.null(offset))
    {
        # explicitly call cbind.data.frame to deal with tibbles
        data <- na.action(cbind.data.frame(data[c(lhsVars, rhsVars)], offsetVals, weightVals))
        offsetVals <- data$offsetVals
    }
    else
    {
        data <- na.action(cbind.data.frame(data[c(lhsVars, rhsVars)], weightVals))
        offsetVals <- NULL
    }
    weightVals <- data$weightVals

    # user-supplied xlev is list/NULL, turn into list of lists
    if(length(xlev) == 0)
        xlev <- list(NULL)
    else if(is.list(xlev) && !is.list(xlev[[1]]))
        xlev <- list(xlev)

    matrs <- mapply(function(x, xlev) {
        xvars <- all.vars(x)
        xnames <- all.names(x)
        isExpr <- !identical(xvars, xnames)
        anyFactors <- any(sapply(data[xvars], function(x) is.factor(x) || is.character(x)))
        # only call model.matrix()/model.frame() if necessary
        if(anyFactors || isExpr || sparse)
        {
            xlev <- xlev[names(xlev) %in% unique(c(deparse(x), xvars))]

            f <- eval(call("~", substitute(0 + .x, list(.x=x))))
            mf <- model.frame(f, data, drop.unused.levels=drop.unused.levels, xlev=xlev, na.action=na.action)

            out <- if(sparse)
                Matrix::sparse.model.matrix(terms(mf), mf, xlev=xlev)
            else model.matrix(terms(mf), mf, xlev=xlev)

            # store levels of x
            attr(out, "xlev") <- lapply(mf, levels)
        }
        else if(length(xvars) == 1)
        {
            # manually setting dim() is faster for a vector than calling matrix() or as.matrix()
            out <- na.action(data[[xvars]])
            dim(out) <- c(length(out), 1)
            colnames(out) <- xvars
        }
        else out <- as.matrix(na.action(data[xvars]))

        out
    }, rhsTerms, xlev, SIMPLIFY=FALSE)

    # cut-down terms object: an unevaluated formula
    terms <- call("~", rhs)
    xlev <- lapply(matrs, attr, "xlev")

    list(x=do.call(cbind, matrs), y=eval(lhs, data), weights=weightVals, offset=offsetVals, terms=terms, xlev=xlev)
}


# get list of additive terms in a model: ~ a + b + c*d + e:f -> list(a, b, c*d, e:f)
# without creating terms object
additiveTerms <- function(f, vars)
{
    plus <- quote(`+`)
    minus <- quote(`-`)
    dot <- quote(.)
    tilde <- quote(`~`)
    rhs <- if(!is.symbol(f) && identical(f[[1]], tilde)) f[[length(f)]] else f
    l <- list()

    term <- function(x)
    {
        if(identical(x, dot))
            rev(lapply(vars, as.name))
        else x
    }

    # walk the parse tree
    repeat
    {
        if(is.call(rhs) && (identical(rhs[[1]], plus) || identical(rhs[[1]], minus)))
        {
            if(identical(rhs[[1]], plus))
                l <- c(l, term(rhs[[3]]))
            else if(identical(rhs[[1]], minus))
                vars <- base::setdiff(vars, deparse(rhs[[length(rhs)]]))
            rhs <- rhs[[2]]
        }
        else
        {
            l <- c(l, term(rhs))
            break
        }
    }
    rev(l)
}


# rebuild the rhs of a formula, from a list of terms
# essentially the reverse of additiveTerms() above
rebuildRhs <- function(rhs)
{
    expr <- rhs[[1]]
    if(length(rhs) > 1) for(i in 2:length(rhs))
    {
        expr <- substitute(a + b, list(a=expr, b=rhs[[i]]))
    }
    expr
}


