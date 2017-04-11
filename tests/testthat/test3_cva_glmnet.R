context("cva.glmnet")

data(Boston, package="MASS")
set.seed(898989)
Bos_cva0 <- cva.glmnet(medv ~ ., data=Boston)
Bos_cva0_m <- sapply(Bos_cva0$modlist, function(x) x$cvm)


test_that("predict and coef work", {
    w <- 5
    a <- Bos_cva0$alpha[w]
    s <- median(Bos_cva0$modlist[[w]]$lambda)

    Bos_cvac0.1 <- coef(Bos_cva0, alpha=a)
    Bos_cvac0.2 <- coef(Bos_cva0, which=w)
    expect_s4_class(Bos_cvac0.1, "Matrix")
    expect_equal(Bos_cvac0.1, Bos_cvac0.2)

    Bos_cvac0.3 <- coef(Bos_cva0, alpha=a, s=s)
    Bos_cvac0.4 <- coef(Bos_cva0, which=w, s=s)
    expect_equal(Bos_cvac0.3, Bos_cvac0.4)

    Bos_cvap0.1 <- predict(Bos_cva0, Boston, alpha=a)
    Bos_cvap0.2 <- predict(Bos_cva0, Boston, which=w)
    expect_type(Bos_cvap0.1, "double")
    expect_equal(Bos_cvap0.1, Bos_cvap0.2)

    Bos_cvap0.3 <- predict(Bos_cva0, Boston, alpha=a, s=s)
    Bos_cvap0.4 <- predict(Bos_cva0, Boston, which=w, s=s)
    expect_equal(Bos_cvap0.3, Bos_cvap0.4)
})


test_that("prediction with NA works", {
    BostonNA <- Boston
    BostonNA[1, ] <- NA

    pred0.1 <- predict(Bos_cva0, BostonNA, which=5, na.action=na.exclude)
    pred0.2 <- predict(Bos_cva0, BostonNA, which=5, na.action=na.pass)
    expect(all(!is.na(pred0.1)), "NAs found with na.exclude")
    expect(all(is.na(pred0.2[1, ])), "NAs dropped with na.pass")
})


cl <- parallel::makeCluster(2)

test_that("parallel backend works", {
    set.seed(898989)
    Bos_cva1 <- cva.glmnet(medv ~ ., data=Boston, outerParallel=cl)
    expect_s3_class(Bos_cva1, "cva.glmnet.formula")

    Bos_cva1_m <- sapply(Bos_cva1$modlist, function(x) x$cvm)
    expect_equal(Bos_cva0_m, Bos_cva1_m)
})


test_that("Revo backend works", {
    skip_if_not_installed("RevoScaleR")
    skip_if_not_installed("doParallel")
    doParallel::registerDoParallel(cl)

    set.seed(898989)
    Bos_cva2 <- cva.glmnet(medv ~ ., data=Boston, outerParallel="dopar")
    expect_s3_class(Bos_cva2, "cva.glmnet.formula")

    Bos_cva2_m <- sapply(Bos_cva2$modlist, function(x) x$cvm)
    expect_equal(Bos_cva0_m, Bos_cva2_m)
})


test_that("inner and outer parallel triggers warning", {
    skip_if_not_installed("doParallel")
    doParallel::registerDoParallel(cl)
    expect_warning(cva.glmnet(medv ~ ., data=Boston, parallel=TRUE, outerParallel=cl))
})

parallel::stopCluster(cl)
