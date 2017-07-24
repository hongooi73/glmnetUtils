context("predict edge cases")


test_that("mismatched factor levels work, glmnet", {
    df <- data.frame(y=rnorm(100), x=sample(letters[11:14], replace=TRUE, size=100))
    l <- 10^seq(-4, -1, len=101)
    m0 <- glmnet(y ~ x, df, lambda=l, use.model.frame=TRUE)
    m1 <- glmnet(y ~ x, df, lambda=l, use.model.frame=FALSE)

    newdf1a <- data.frame(x=letters[11:14], stringsAsFactors=FALSE)
    newdf1b <- data.frame(x=letters[11:14], stringsAsFactors=TRUE)

    newdf2a <- data.frame(x=letters[10:14], stringsAsFactors=FALSE)
    newdf2b <- data.frame(x=letters[10:14], stringsAsFactors=TRUE)

    newdf3a <- data.frame(x=letters[12:14], stringsAsFactors=FALSE)
    newdf3b <- data.frame(x=letters[12:14], stringsAsFactors=TRUE)

    expect_equal(predict(m0, newdf1a), predict(m0, newdf1b))
    expect_equal(predict(m0, newdf1a), predict(m0, newdf1b))

    expect_error(predict(m0, newdf2a))
    expect_error(predict(m0, newdf2b))
    expect_error(predict(m1, newdf2a))
    expect_error(predict(m1, newdf2b))

    expect_equal(predict(m0, newdf3a), predict(m0, newdf3b))
    expect_equal(predict(m1, newdf3a), predict(m1, newdf3b))
})


test_that("mismatched factor levels work, cv.glmnet", {
    df <- data.frame(y=rnorm(100), x=sample(letters[11:14], replace=TRUE, size=100))
    l <- 10^seq(-4, -1, len=101)
    m0 <- cv.glmnet(y ~ x, df, lambda=l, use.model.frame=TRUE)
    m1 <- cv.glmnet(y ~ x, df, lambda=l, use.model.frame=FALSE)

    newdf1a <- data.frame(x=letters[11:14], stringsAsFactors=FALSE)
    newdf1b <- data.frame(x=letters[11:14], stringsAsFactors=TRUE)

    newdf2a <- data.frame(x=letters[10:14], stringsAsFactors=FALSE)
    newdf2b <- data.frame(x=letters[10:14], stringsAsFactors=TRUE)

    newdf3a <- data.frame(x=letters[12:14], stringsAsFactors=FALSE)
    newdf3b <- data.frame(x=letters[12:14], stringsAsFactors=TRUE)

    expect_equal(predict(m0, newdf1a), predict(m0, newdf1b))
    expect_equal(predict(m0, newdf1a), predict(m0, newdf1b))

    expect_error(predict(m0, newdf2a))
    expect_error(predict(m0, newdf2b))
    expect_error(predict(m1, newdf2a))
    expect_error(predict(m1, newdf2b))

    expect_equal(predict(m0, newdf3a), predict(m0, newdf3b))
    expect_equal(predict(m1, newdf3a), predict(m1, newdf3b))
})


test_that("mismatched factor levels work, cva.glmnet", {
    df <- data.frame(y=rnorm(100), x=sample(letters[11:14], replace=TRUE, size=100))
    l <- 10^seq(-4, -1, len=101)
    m0 <- cva.glmnet(y ~ x, df, lambda=l, use.model.frame=TRUE)
    m1 <- cva.glmnet(y ~ x, df, lambda=l, use.model.frame=FALSE)

    newdf1a <- data.frame(x=letters[11:14], stringsAsFactors=FALSE)
    newdf1b <- data.frame(x=letters[11:14], stringsAsFactors=TRUE)

    newdf2a <- data.frame(x=letters[10:14], stringsAsFactors=FALSE)
    newdf2b <- data.frame(x=letters[10:14], stringsAsFactors=TRUE)

    newdf3a <- data.frame(x=letters[12:14], stringsAsFactors=FALSE)
    newdf3b <- data.frame(x=letters[12:14], stringsAsFactors=TRUE)

    expect_equal(predict(m0, newdf1a, alpha=1), predict(m0, newdf1b, alpha=1))
    expect_equal(predict(m0, newdf1a, alpha=1), predict(m0, newdf1b, alpha=1))

    expect_error(predict(m0, newdf2a, alpha=1))
    expect_error(predict(m0, newdf2b, alpha=1))
    expect_error(predict(m1, newdf2a, alpha=1))
    expect_error(predict(m1, newdf2b, alpha=1))

    expect_equal(predict(m0, newdf3a, alpha=1), predict(m0, newdf3b, alpha=1))
    expect_equal(predict(m1, newdf3a, alpha=1), predict(m1, newdf3b, alpha=1))
})

