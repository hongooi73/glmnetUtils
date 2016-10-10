dropIntercept <- function(matr)
{
    if(!is.matrix(matr))
        matr <- as.matrix(matr)
    matr[, -1, drop=FALSE]
}


#' @importFrom Matrix sparse.model.matrix
# short, simple function that unavoidably creates a pxp square matrix (!)
makeModelComponentsMF <- function(formula, data, weights=NULL, offset=NULL, subset=NULL, na.action=getOption("na.action"),
                                  drop.unused.levels=FALSE, xlev=NULL, sparse=FALSE)
{
    mf <- model.frame(formula, data, weights=weights, offset=offset, subset=subset, na.action=na.action,
                      drop.unused.levels=drop.unused.levels, xlev=xlev)
    x <- if(sparse)
        dropIntercept(Matrix::sparse.model.matrix(attr(mf, "terms"), mf))
    else dropIntercept(model.matrix(attr(mf, "terms"), mf))
    y <- model.response(mf)
    weights <- model.extract(mf, "weights")
    offset <- model.extract(mf, "offset")
    if(is.null(weights))
        weights <- rep(1, length(y))

    list(x=x, y=y, weights=weights, offset=offset, terms=terms(mf))
}


# emulate model.frame + model.matrix, without constructing a terms object
makeModelComponents <- function(formula, data, weights=NULL, offset=NULL, subset=NULL, na.action=getOption("na.action"),
                                drop.unused.levels=FALSE, xlev=NULL, sparse=FALSE)
{
    if(length(formula) == 3)
    {
        rhs <- formula[[3]]
        lhs <- formula[[2]]
    }
    else
    {
        rhs <- formula[[2]]
        lhs <- NULL
    }
    lhsVars <- all.vars(lhs)

    if(identical(quote(.), rhs))
    {
        rhsVars <- setdiff(names(data), lhsVars)
        rhs <- formula(paste("~", paste(rhsVars, collapse="+")))[[2]]
    }
    rhsVars <- all.vars(rhs)
    rhsNames <- all.names(rhs)

    # only formulas of the form x1 + x2 + ... allowed, no expressions or interactions
    if(!setequal(c("+", rhsVars), c("+", rhsNames)))
        stop("only simple formulas allowed")

    if(!missing(subset) && !is.null(subset))
    {
        subset <- substitute(subset)
        data <- eval(subset, data, parent.frame())
    }

    if(!is.function(na.action))
        na.action <- getFunction(na.action)
    data <- na.action(data[c(lhsVars, rhsVars)])

    matrs <- sapply(rhsVars, function(x) {
        if(is.numeric(data[[x]]) || is.logical(data[[x]]))
            data[[x]]
        else if(sparse)
            Matrix::sparse.model.matrix(formula(paste("~ 0 +", x)), data,
                drop.unused.levels=drop.unused.levels, xlev=xlev)
        else model.matrix(formula(paste("~ 0 +", x)), data,
                drop.unused.levels=drop.unused.levels, xlev=xlev)
    }, simplify=FALSE)

    # cut-down version of real terms object: just a formula
    terms <- do.call("~", list(rhs))

    offset <- substitute(offset)
    offset <- eval(offset, data, parent.frame())
    if(!missing(weights) && !is.null(weights))
    {
        weights <- substitute(weights)
        weights <- eval(weights, data, parent.frame())
    }
    else weights <- rep(1, nrow(data))

    list(x=do.call(cbind, matrs), y=eval(lhs, data), weights=weights, offset=offset, terms=terms)
}

