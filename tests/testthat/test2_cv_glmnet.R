context("cv.glmnet")

data(Boston, package="MASS")
x <- as.matrix(Boston[-1])
y <- Boston$crim
set.seed(12345)
Bos_cv00 <- glmnet::cv.glmnet(x, y, alpha=0.5)
set.seed(12345)
Bos_cv0 <- cv.glmnet(x, y, alpha=0.5)
set.seed(12345)
Bos_cv1 <- cv.glmnet(crim ~ ., data=Boston, alpha=0.5, use.model.frame=TRUE)
set.seed(12345)
Bos_cv2 <- cv.glmnet(crim ~ ., data=Boston, alpha=0.5, use.model.frame=FALSE)


test_that("method dispatch works", {
    expect_s3_class(Bos_cv1, "cv.glmnet.formula")
    expect_equal(Bos_cv00$glmnet.fit$beta, Bos_cv0$glmnet.fit$beta)
    expect_equal(Bos_cv0$glmnet.fit$beta, Bos_cv1$glmnet.fit$beta)
    expect_equal(Bos_cv1$glmnet.fit$beta, Bos_cv2$glmnet.fit$beta)
})


test_that("predict and coef work", {
    Bos_cvc0 <- coef(Bos_cv0, s="lambda.min")
    Bos_cvc1 <- coef(Bos_cv1, s="lambda.min")
    Bos_cvc2 <- coef(Bos_cv2, s="lambda.min")
    expect_equal(Bos_cvc0, Bos_cvc1)
    expect_equal(Bos_cvc1, Bos_cvc2)

    Bos_cvp0 <- predict(Bos_cv0, x)
    Bos_cvp1 <- predict(Bos_cv1, Boston)
    Bos_cvp2 <- predict(Bos_cv2, Boston)
    expect_equivalent(Bos_cvp0, Bos_cvp1)
    expect_equivalent(Bos_cvp1, Bos_cvp2)

    s <- median(Bos_cv0$lambda)

    Bos_cvp0 <- predict(Bos_cv0, x, s=s)
    Bos_cvp1 <- predict(Bos_cv1, Boston, s=s)
    Bos_cvp2 <- predict(Bos_cv2, Boston, s=s)
    expect_equivalent(Bos_cvp0, Bos_cvp1)
    expect_equivalent(Bos_cvp1, Bos_cvp2)
})


test_that("prediction with NA works", {
    BostonNA <- Boston
    BostonNA[1, ] <- NA

    pred1.1 <- predict(Bos_cv1, BostonNA, na.action=na.exclude)
    pred1.2 <- predict(Bos_cv2, BostonNA, na.action=na.exclude)
    expect_equivalent(pred1.1, pred1.2)
    expect(all(!is.na(pred1.1)), "NAs found with na.exclude")

    pred1.3 <- predict(Bos_cv1, BostonNA, na.action=na.pass)
    pred1.4 <- predict(Bos_cv2, BostonNA, na.action=na.pass)
    expect_equivalent(pred1.3, pred1.4)
    expect(all(is.na(pred1.3[1, ])), "NAs dropped with na.pass")
})


test_that("parallel foreach works", {
    skip_if_not_installed("doParallel")
    doParallel::registerDoParallel()
    x <- as.matrix(iris[-5])
    y <- iris$Species
    set.seed(7777)
    mod1.0 <- cv.glmnet(x, y, family="multinomial", nfolds=5, parallel=TRUE)
    set.seed(7777)
    mod1.1 <- cv.glmnet(Species ~ ., data=iris, family="multinomial", nfolds=5, parallel=TRUE)
    doParallel::stopImplicitCluster()
    expect_equal(mod1.0$beta, mod1.1$beta)
})


