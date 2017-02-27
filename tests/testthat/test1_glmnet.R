context("glmnet")


# TODO: better separation of simpleMM, glmnet
test_that("method dispatch works", {
    x <- as.matrix(mtcars[-1])
    y <- mtcars$mpg
    mod00 <- glmnet::glmnet(x, y)
    mod0 <- glmnet(x, y)
    mod1 <- glmnet(mpg ~ ., data=mtcars)
    expect_s3_class(mod1, "glmnet.formula")
    expect_equal(mod00$beta, mod0$beta)
    expect_equal(mod0$beta, mod1$beta)
})


test_that("simple model matrix works", {
    mod1 <- glmnet(mpg ~ ., data=mtcars, use.model.frame=TRUE)
    mod2 <- glmnet(mpg ~ ., data=mtcars, use.model.frame=FALSE)
    expect_equal(mod1$beta, mod2$beta)
})


test_that("glmnet arguments work", {
    x <- as.matrix(iris[-5])
    y <- as.matrix(iris$Species)
    mod00 <- glmnet::glmnet(x, y, family="multinomial", alpha=0.5)
    mod0 <- glmnet(x, y, family="multinomial", alpha=0.5)
    mod1 <- glmnet(Species ~ ., data=iris, family="multinomial")
    expect_equivalent(mod00$beta, mod0$beta)
    expect_equivalent(mod0$beta, mod1$beta)

    data(Insurance, package="MASS")
    # don't worry about ordered factors for now
    class(Insurance$Group) <- "factor"
    class(Insurance$Age) <- "factor"
    x <- model.matrix(~ District + Group + Age, data=Insurance)[, -1]
    y <- Insurance$Claims
    ofs <- Insurance$Holders
    mod00 <- glmnet::glmnet(x, y, family="poisson", offset=log(ofs))
    mod0 <- glmnet(x, y, family="poisson", offset=log(ofs))
    mod1 <- glmnet(Claims ~ District + Group + Age, data=Insurance, family="poisson", offset=log(Holders),
                   use.model.frame=TRUE)
    expect_equivalent(mod00$beta, mod0$beta)
    expect_equivalent(mod0$beta, mod1$beta)
})


test_that("predict and coef work", {
    x <- as.matrix(iris[-5])
    y <- iris$Species
    mod1.0 <- glmnet(x, y, family="multinomial")
    mod1.1 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=TRUE)
    mod1.2 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=FALSE)
    pred1.0 <- predict(mod1.0, as.matrix(iris[-5]), type="class")
    pred1.1 <- predict(mod1.1, iris, type="class")
    pred1.2 <- predict(mod1.2, iris, type="class")
    expect_equivalent(pred1.0, pred1.1)  # expect_equivalent because dimnames not always preserved
    expect_equivalent(pred1.1, pred1.2)
    coef1.0 <- coef(mod1.0)
    coef1.1 <- coef(mod1.1)
    coef1.2 <- coef(mod1.2)
    expect_equal(coef1.0, coef1.1)
    expect_equal(coef1.1, coef1.2)

    x <- as.matrix(mtcars[-1])
    y <- mtcars$mpg
    mod2.0 <- glmnet(x, y, alpha=0.5)
    mod2.1 <- glmnet(mpg ~ ., data=mtcars, alpha=0.5, use.model.frame=TRUE)
    mod2.2 <- glmnet(mpg ~ ., data=mtcars, alpha=0.5, use.model.frame=FALSE)
    pred2.0 <- predict(mod2.0, x, s=1)
    pred2.1 <- predict(mod2.1, mtcars, s=1)
    pred2.2 <- predict(mod2.2, mtcars, s=1)
    expect_equivalent(pred2.0, pred2.1)
    expect_equivalent(pred2.1, pred2.2)
    coef2.0 <- coef(mod2.0, s=1)
    coef2.1 <- coef(mod2.1, s=1)
    coef2.2 <- coef(mod2.2, s=1)
    expect_equal(coef2.0, coef2.1)
    expect_equal(coef2.1, coef2.2)
})


test_that("prediction with NA works", {
    data(Boston, package="MASS")
    BostonNA <- Boston
    BostonNA[1, ] <- NA

    mod1.1 <- glmnet(medv ~ ., data=Boston, alpha=0, use.model.frame=TRUE)
    mod1.2 <- glmnet(medv ~ ., data=Boston, alpha=0, use.model.frame=FALSE)

    pred1.1 <- predict(mod1.1, BostonNA, na.action=na.exclude)
    pred1.2 <- predict(mod1.2, BostonNA, na.action=na.exclude)
    expect_equivalent(pred1.1, pred1.2)
    expect(all(!is.na(pred1.1)), "NAs found with na.exclude")

    pred1.3 <- predict(mod1.1, BostonNA, na.action=na.pass)
    pred1.4 <- predict(mod1.2, BostonNA, na.action=na.pass)
    expect_equivalent(pred1.3, pred1.4)
    expect(all(is.na(pred1.3[1, ])), "NAs dropped with na.pass")
})


test_that("matrix y works", {
    df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rbinom(100, 10, 0.5))
    df$ny <- 10 - df$y
    mod1.1 <- glmnet(cbind(ny, y) ~ x1 + x2, data = df, family = "binomial", use.model.frame = TRUE)
    mod1.2 <- glmnet(cbind(ny, y) ~ x1 + x2, data = df, family = "binomial", use.model.frame = FALSE)

    pred1.1 <- predict(mod1.1, df)
    pred1.2 <- predict(mod1.2, df)
    expect_equivalent(pred1.1, pred1.2)
})
