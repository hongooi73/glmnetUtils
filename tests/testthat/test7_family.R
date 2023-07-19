context("Enhanced family arg")

# to avoid issues with 32-bit Solaris checks on CRAN
skip_on_os("solaris")


test_that("enhanced family arg works with glmnet", {
    x <- as.matrix(mtcars[-1])
    y <- mtcars$mpg
    mod0.1 <- glmnet::glmnet(x, y)
    mod0.2 <- glmnet::glmnet(x, y, family="gaussian")
    mod0.3 <- glmnet::glmnet(x, y, family=gaussian)
    mod0.4 <- glmnet::glmnet(x, y, family=gaussian())
    mod1.1 <- glmnet(mpg ~ ., data=mtcars)
    mod1.2 <- glmnet(mpg ~ ., data=mtcars, family="gaussian")
    mod1.3 <- glmnet(mpg ~ ., data=mtcars, family=gaussian)
    mod1.4 <- glmnet(mpg ~ ., data=mtcars, family=gaussian())
    expect_s3_class(mod1.1, "glmnet.formula")
    expect_s3_class(mod1.2, "glmnet.formula")
    expect_s3_class(mod1.3, "glmnet.formula")
    expect_s3_class(mod1.4, "glmnet.formula")
    expect_equal(mod0.1$beta, mod1.1$beta)
    expect_equal(mod0.2$beta, mod1.2$beta)
    expect_equal(mod0.3$beta, mod1.3$beta)
    expect_equal(mod0.4$beta, mod1.4$beta)
})


test_that("enhanced family arg works with cv.glmnet", {
    skip_if_not_installed("MASS")
    data(Boston, package="MASS")
    x <- as.matrix(Boston[-1])
    y <- Boston$crim

    set.seed(12345)
    mod0.1 <- glmnet::cv.glmnet(x, y, alpha=0.5)
    set.seed(12345)
    mod0.2 <- glmnet::cv.glmnet(x, y, alpha=0.5, family="gaussian")
    set.seed(12345)
    mod0.3 <- glmnet::cv.glmnet(x, y, alpha=0.5, family=gaussian)
    set.seed(12345)
    mod0.4 <- glmnet::cv.glmnet(x, y, alpha=0.5, family=gaussian())

    set.seed(12345)
    mod1.1 <- cv.glmnet(crim ~ ., data=Boston, alpha=0.5)
    set.seed(12345)
    mod1.2 <- cv.glmnet(crim ~ ., data=Boston, alpha=0.5, family="gaussian")
    set.seed(12345)
    mod1.3 <- cv.glmnet(crim ~ ., data=Boston, alpha=0.5, family=gaussian)
    set.seed(12345)
    mod1.4 <- cv.glmnet(crim ~ ., data=Boston, alpha=0.5, family=gaussian())

    expect_equal(mod0.1$beta, mod1.1$beta)
    expect_equal(mod0.2$beta, mod1.2$beta)
    expect_equal(mod0.3$beta, mod1.3$beta)
    expect_equal(mod0.4$beta, mod1.4$beta)
})


test_that("enhanced family arg works with cva.glmnet", {
    skip_if_not_installed("MASS")
    data(Boston, package="MASS")

    set.seed(12345)
    mod1.1 <- cva.glmnet(medv ~ ., data=Boston)
    set.seed(12345)
    mod1.2 <- cva.glmnet(medv ~ ., data=Boston, family="gaussian")
    set.seed(12345)
    mod1.3 <- cva.glmnet(medv ~ ., data=Boston, family=gaussian)
    set.seed(12345)
    mod1.4 <- cva.glmnet(medv ~ ., data=Boston, family=gaussian())

    w <- 5
    expect_equal(coef(mod1.1, which=w), coef(mod1.2, which=w))
    expect_equal(coef(mod1.3, which=w), coef(mod1.4, which=w))
})


test_that("enhanced family arg works with relaxed glmnet", {
    x <- as.matrix(mtcars[-1])
    y <- mtcars$mpg
    mod0.1 <- glmnet::glmnet(x, y, alpha=0.5, relax=TRUE)
    mod0.2 <- glmnet::glmnet(x, y, family="gaussian", alpha=0.5, relax=TRUE)
    mod0.3 <- glmnet::glmnet(x, y, family=gaussian, alpha=0.5, relax=TRUE)
    mod0.4 <- glmnet::glmnet(x, y, family=gaussian(), alpha=0.5, relax=TRUE)

    mod1.1 <- glmnet(mpg ~ ., data=mtcars, alpha=0.5, relax=TRUE)
    mod1.2 <- glmnet(mpg ~ ., data=mtcars, family="gaussian", alpha=0.5, relax=TRUE)
    mod1.3 <- glmnet(mpg ~ ., data=mtcars, family=gaussian, alpha=0.5, relax=TRUE)
    mod1.4 <- glmnet(mpg ~ ., data=mtcars, family=gaussian(), alpha=0.5, relax=TRUE)
    expect_equivalent(mod0.1$beta, mod1.1$beta)
    expect_equivalent(mod0.2$beta, mod1.2$beta)
    expect_equivalent(mod0.3$beta, mod1.3$beta)
    expect_equivalent(mod0.4$beta, mod1.4$beta)
})


test_that("enhanced family arg works with relaxed cv.glmnet", {
    skip_if_not_installed("MASS")
    data(Boston, package="MASS")
    x <- as.matrix(Boston[-1])
    y <- Boston$crim
    set.seed(12345)
    mod0.1 <- glmnet::cv.glmnet(x, y, alpha=0.5, gamma=(0:3)/3, relax=TRUE)
    set.seed(12345)
    mod0.2 <- glmnet::cv.glmnet(x, y, family="gaussian", alpha=0.5, gamma=(0:3)/3, relax=TRUE)
    set.seed(12345)
    mod0.3 <- glmnet::cv.glmnet(x, y, family=gaussian, alpha=0.5, gamma=(0:3)/3, relax=TRUE)
    set.seed(12345)
    mod0.4 <- glmnet::cv.glmnet(x, y, family=gaussian(), alpha=0.5, gamma=(0:3)/3, relax=TRUE)

    set.seed(12345)
    mod1.1 <- cv.glmnet(crim ~ ., data=Boston, alpha=0.5, gamma=(0:3)/3, relax=TRUE)
    set.seed(12345)
    mod1.2 <- cv.glmnet(crim ~ ., data=Boston, family="gaussian", alpha=0.5, gamma=(0:3)/3, relax=TRUE)
    set.seed(12345)
    mod1.3 <- cv.glmnet(crim ~ ., data=Boston, family=gaussian, alpha=0.5, gamma=(0:3)/3, relax=TRUE)
    set.seed(12345)
    mod1.4 <- cv.glmnet(crim ~ ., data=Boston, family=gaussian(), alpha=0.5, gamma=(0:3)/3, relax=TRUE)

    expect_equal(mod0.1$glmnet.fit$beta, mod1.1$glmnet.fit$beta)
    expect_equal(mod0.2$glmnet.fit$beta, mod1.2$glmnet.fit$beta)
    expect_equal(mod0.3$glmnet.fit$beta, mod1.3$glmnet.fit$beta)
    expect_equal(mod0.4$glmnet.fit$beta, mod1.4$glmnet.fit$beta)
})
