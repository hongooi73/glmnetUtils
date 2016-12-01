context("Simple model matrix")

test_that("simple model matrix works", {
    mod1 <- glmnet(mpg ~ ., data=mtcars)
    expect_s3_class(mod1, "glmnet")
})


test_that("simple and base model matrix agree", {
    mod1.1 <- glmnet(mpg ~ ., data=mtcars, use.model.frame=TRUE)
    mod1.2 <- glmnet(mpg ~ ., data=mtcars, use.model.frame=FALSE)
    expect_equal(mod1.1$beta, mod1.2$beta)

    mod2.1 <- glmnet(mpg ~ ., data=mtcars, sparse=TRUE, use.model.frame=TRUE)
    mod2.2 <- glmnet(mpg ~ ., data=mtcars, sparse=TRUE, use.model.frame=FALSE)
    expect_equal(mod2.1$beta, mod2.2$beta)
})


test_that("optional arguments work", {
    x <- as.matrix(iris[-5])
    y <- iris$Species
    mod1.0 <- glmnet(x, y, family="multinomial")
    mod1.1 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=TRUE)
    mod1.2 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=FALSE)
    expect_equal(mod1.0$beta, mod1.1$beta)
    expect_equal(mod1.1$beta, mod1.2$beta)

    x <- as.matrix(mtcars[-1])
    y <- mtcars$mpg
    mod2.0 <- glmnet(x, y, alpha=0.5)
    mod2.1 <- glmnet(mpg ~ ., data=mtcars, alpha=0.5, use.model.frame=TRUE)
    mod2.2 <- glmnet(mpg ~ ., data=mtcars, alpha=0.5, use.model.frame=FALSE)
    expect_equal(mod2.0$beta, mod2.1$beta)
    expect_equal(mod2.1$beta, mod2.2$beta)

    x <- as.matrix(mtcars[c("cyl", "disp", "hp", "drat")])
    y <- mtcars$mpg
    w <- mtcars$wt
    mod3.0 <- glmnet(x, y, weights=w)
    mod3.1 <- glmnet(mpg ~ cyl + disp + hp + drat, data=mtcars, weights=wt, use.model.frame=TRUE)
    mod3.2 <- glmnet(mpg ~ cyl + disp + hp + drat, data=mtcars, weights=wt, use.model.frame=FALSE)
    expect_equal(mod3.0$beta, mod3.1$beta)
    expect_equal(mod3.1$beta, mod3.2$beta)

    data(Boston, package="MASS")
    x <- model.matrix(~ ., Boston[1:3])[, -1]
    y <- Boston$medv
    ofs <- Boston$lstat
    mod4.0 <- glmnet(x, y, offset=log(ofs))
    mod4.1 <- glmnet(medv ~ crim + zn + indus, data=Boston, offset=log(lstat), use.model.frame=TRUE)
    mod4.2 <- glmnet(medv ~ crim + zn + indus, data=Boston, offset=log(lstat), use.model.frame=FALSE)
    expect_equal(mod4.0$beta, mod4.1$beta)
    expect_equal(mod4.1$beta, mod4.2$beta)

    data(Insurance, package="MASS")
    x <- model.matrix(~ 0 + District, Insurance)
    y <- Insurance$Claims
    mod5.0 <- glmnet(x, y, family="poisson")
    mod5.1 <- glmnet(Claims ~ District, data=Insurance, family="poisson")
    expect_equal(mod5.0$beta, mod5.1$beta)
})


test_that("model fitting with NA works", {
    irisNA <- iris
    irisNA[1, 1] <- NA
    x <- as.matrix(irisNA[-5])
    y <- irisNA$Species
    expect_error(glmnet(x, y, family="multinomial"))
    expect_error(glmnet(Species ~ ., data=irisNA, family="multinomial", use.model.frame=FALSE, na.action=na.fail))
    expect_error(glmnet(Species ~ ., data=irisNA, family="multinomial", use.model.frame=TRUE, na.action=na.fail))

    # use alpha=0 so that column with NA doesn't get dropped from model
    mod1.0 <- glmnet(x[-1, ], y[-1], family="multinomial", alpha=0)
    mod1.1 <- glmnet(Species ~ ., data=irisNA, family="multinomial", alpha=0, use.model.frame=FALSE)
    mod1.2 <- glmnet(Species ~ ., data=irisNA, family="multinomial", alpha=0, use.model.frame=TRUE)
    expect_equal(mod1.0$beta, mod1.1$beta)
    expect_equal(mod1.1$beta, mod1.2$beta)
})

