context("Simple model frame")

std_mf <- glmnetUtils:::makeModelComponentsMF
simple_mf <- glmnetUtils:::makeModelComponents


test_that("simple model frame works",
{
    xy <- simple_mf(mpg ~ ., data=mtcars)

    expect_is(xy, "list")
    expect_is(xy$x, "matrix")
    expect_is(xy$y, "numeric")
    expect_true(all(xy$weights == 1))
    expect_null(xy$offset)
    expect_is(xy$terms, "call")
})


test_that("standard and simple model frame agree",
{
    xy0 <- std_mf(mpg ~ ., data=mtcars)
    xy1 <- simple_mf(mpg ~ ., data=mtcars)

    expect_equivalent(xy0$x, xy1$x)
    expect_equivalent(xy0$y, xy1$y)
})


test_that("optional arguments work",
{
    xy1.0 <- std_mf(mpg ~ ., data=mtcars, weights=wt, offset=drat, sparse=TRUE)
    xy1.1 <- simple_mf(mpg ~ ., data=mtcars, weights=wt, offset=drat, sparse=TRUE)

    expect_s4_class(xy1.0$x, "Matrix")
    expect_s4_class(xy1.1$x, "Matrix")

    expect_equivalent(xy1.0$x, xy1.1$x)
    expect_equivalent(xy1.0$weights, xy1.1$weights)
    expect_equivalent(xy1.0$offset, xy1.1$offset)
})


test_that("factor handling works",
{
    skip_if_not_installed("MASS")
    data(Insurance, package="MASS")
    # don't worry about ordered factors for now
    class(Insurance$Group) <- "factor"
    class(Insurance$Age) <- "factor"
    xy <- simple_mf(Claims ~ District + Group + Age, data=Insurance)
    nlevs <- with(Insurance, nlevels(District) + nlevels(Group) + nlevels(Age))

    expect_equal(ncol(xy$x), nlevs)
    expect_true(all(range(xy$x) == c(0, 1)))
})


test_that("NA handling works",
{
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


test_that("nonsyntactic vars work",
{
    mtcarsNS <- cbind(mtcars, `mpg+rand`=mtcars$mpg + rnorm(nrow(mtcars)), `factor(cyl)`=factor(mtcars$cyl))
    xy <- simple_mf(mpg ~ ., data=mtcarsNS)
    expect_is(xy$x, "matrix")
    expect_equal(ncol(xy$x), 14)
})


test_that("interaction/expression terms work",
{
    skip_if_not_installed("MASS")
    data(Insurance, package="MASS")
    xy1 <- simple_mf(Claims / Holders ~ District * Group + Age, data=Insurance)
    expect_is(xy1$x, "matrix")
    expect_equal(ncol(xy1$x), 20)

    xy2 <- simple_mf(Claims / Holders ~ . + District:Group, data=Insurance)
    expect_is(xy2$x, "matrix")
    expect_equal(ncol(xy2$x), 28)

    xy3 <- simple_mf(~Species + log(Sepal.Length) + log(Sepal.Width), data=iris)
    expect_is(xy3$x, "matrix")
    expect_equal(ncol(xy3$x), 5)
})


test_that("xlev handling works",
{
    df <- data.frame(a=sample(letters[1:4], 100, replace=TRUE), b=sample(letters[5:10], 100, replace=TRUE),
                     stringsAsFactors=TRUE)
    xy1 <- simple_mf(~., data=df)
    expect_equal(length(xy1$xlev[[1]]$a), 4)
    expect_equal(length(xy1$xlev[[2]]$b), 6)

    xy2 <- simple_mf(~., data=df, xlev=list(a=letters[1:10], b=letters[1:10]))
    expect_equal(length(xy2$xlev[[1]]$a), 10)
    expect_equal(length(xy2$xlev[[2]]$b), 10)

    xy3 <- simple_mf(~ a + factor(b), data=df, xlev=list(a=letters[1:10], `factor(b)`=letters[1:10]))
    expect_equal(length(xy3$xlev[[1]]$a), 10)
    expect_equal(length(xy3$xlev[[2]]$`factor(b)`), 10)

    expect_warning(xy4 <- simple_mf(~a + factor(b), data=df, xlev=list(a=letters[1:10], b=letters[1:10])))
    expect_equal(length(xy4$xlev[[1]]$a), 10)
    expect_equal(length(xy4$xlev[[2]]$`factor(b)`), 6)
})



test_that("xlev handling works with non-factor categorical x",
{
    df <- data.frame(a=sample(letters[1:4], 100, replace=TRUE), b=sample(letters[5:10], 100, replace=TRUE),
                     stringsAsFactors=FALSE)
    xy1 <- simple_mf(~., data=df)
    expect_equal(length(xy1$xlev[[1]]$a), 4)
    expect_equal(length(xy1$xlev[[2]]$b), 6)

    xy2 <- simple_mf(~., data=df, xlev=list(a=letters[1:10], b=letters[1:10]))
    expect_equal(length(xy2$xlev[[1]]$a), 10)
    expect_equal(length(xy2$xlev[[2]]$b), 10)

    xy3 <- simple_mf(~ a + factor(b), data=df, xlev=list(a=letters[1:10], `factor(b)`=letters[1:10]))
    expect_equal(length(xy3$xlev[[1]]$a), 10)
    expect_equal(length(xy3$xlev[[2]]$`factor(b)`), 10)

    expect_warning(xy4 <- simple_mf(~a + factor(b), data=df, xlev=list(a=letters[1:10], b=letters[1:10])))
    expect_equal(length(xy4$xlev[[1]]$a), 10)
    expect_equal(length(xy4$xlev[[2]]$`factor(b)`), 6)
})

