context("Simple model frame")

std_mf <- glmnetUtils:::makeModelComponentsMF
simple_mf <- glmnetUtils:::makeModelComponents


test_that("simple model frame works", {
    xy <- simple_mf(mpg ~ ., data=mtcars)

    expect_is(xy, "list")
    expect_is(xy$x, "matrix")
    expect_is(xy$y, "numeric")
    expect_true(all(xy$weights == 1))
    expect_null(xy$offset)
    expect_is(xy$terms, "call")
})


test_that("standard and simple model frame agree", {
    xy0 <- std_mf(mpg ~ ., data=mtcars)
    xy1 <- simple_mf(mpg ~ ., data=mtcars)

    expect_equivalent(xy0$x, xy1$x)
    expect_equivalent(xy0$y, xy1$y)
})


test_that("optional arguments work", {
    xy1.0 <- std_mf(mpg ~ ., data=mtcars, weights=wt, offset=drat, sparse=TRUE)
    xy1.1 <- simple_mf(mpg ~ ., data=mtcars, weights=wt, offset=drat, sparse=TRUE)

    expect_s4_class(xy1.0$x, "Matrix")
    expect_s4_class(xy1.1$x, "Matrix")

    expect_equivalent(xy1.0$x, xy1.1$x)
    expect_equivalent(xy1.0$weights, xy1.1$weights)
    expect_equivalent(xy1.0$offset, xy1.1$offset)
})


test_that("factor handling works", {
    data(Insurance, package="MASS")
    # don't worry about ordered factors for now
    class(Insurance$Group) <- "factor"
    class(Insurance$Age) <- "factor"
    xy <- simple_mf(Claims ~ District + Group + Age, data=Insurance)
    nlevs <- with(Insurance, nlevels(District) + nlevels(Group) + nlevels(Age))

    expect_equal(ncol(xy$x), nlevs)
    expect_true(all(range(xy$x) == c(0, 1)))
})


test_that("NA handling works", {
    mtcarsNA <- mtcars
    mtcarsNA[1, ] <- NA

    expect_error(std_mf(mpg  ~ ., data=mtcarsNA, na.action=na.fail))
    expect_error(simple_mf(mpg  ~ ., data=mtcarsNA, na.action=na.fail))

    xy1.0 <- std_mf(mpg  ~ ., data=mtcarsNA, weights=wt, offset=log(drat), na.action=na.omit)
    xy1.1 <- simple_mf(mpg  ~ ., data=mtcarsNA, weights=wt, offset=log(drat), na.action=na.omit)
    expect_equivalent(xy1.0$x, xy1.1$x)
    expect_equivalent(xy1.0$y, xy1.1$y)
    expect_equivalent(xy1.0$weights, xy1.1$weights)
    expect_equivalent(xy1.0$offset, xy1.1$offset)

    xy2.0 <- std_mf(mpg  ~ ., data=mtcarsNA, weights=wt, offset=log(drat), na.action=na.exclude)
    xy2.1 <- simple_mf(mpg  ~ ., data=mtcarsNA, weights=wt, offset=log(drat), na.action=na.exclude)
    expect_equivalent(xy2.0$x, xy2.1$x)
    expect_equivalent(xy2.0$y, xy2.1$y)
    expect_equivalent(xy2.0$weights, xy2.1$weights)
    expect_equivalent(xy2.0$offset, xy2.1$offset)

    xy3.0 <- std_mf(mpg  ~ ., data=mtcarsNA, weights=wt, offset=log(drat), na.action=na.pass)
    xy3.1 <- simple_mf(mpg  ~ ., data=mtcarsNA, weights=wt, offset=log(drat), na.action=na.pass)
    expect_equivalent(xy3.0$x, xy3.1$x)
    expect_equivalent(xy3.0$y, xy3.1$y)
    expect_equivalent(xy3.0$weights, xy3.1$weights)
    expect_equivalent(xy3.0$offset, xy3.1$offset)
})

