context("relaxed cv.glmnet")

data(Boston, package="MASS")
x <- as.matrix(Boston[-1])
y <- Boston$crim
set.seed(12345)
Bos_cv00 <- glmnet::cv.glmnet(x, y, weights=Boston$medv, offset=Boston$rad, alpha=0.5, relax=TRUE)
set.seed(12345)
Bos_cv0 <- cv.glmnet(x, y, weights=Boston$medv, offset=Boston$rad, alpha=0.5, relax=TRUE)
set.seed(12345)
Bos_cv1 <- cv.glmnet(crim ~ ., data=Boston, weights=medv, offset=rad, alpha=0.5, use.model.frame=TRUE, relax=TRUE)
set.seed(12345)
Bos_cv2 <- cv.glmnet(crim ~ ., data=Boston, weights=medv, offset=rad, alpha=0.5, use.model.frame=FALSE, relax=TRUE)


test_that("relaxed cv.glmnet works", {
    expect_s3_class(Bos_cv1, "cv.relaxed.formula")
    expect_equal(Bos_cv00$glmnet.fit$beta, Bos_cv0$glmnet.fit$beta)
    expect_equal(Bos_cv0$glmnet.fit$beta, Bos_cv1$glmnet.fit$beta)
    expect_equal(Bos_cv1$glmnet.fit$beta, Bos_cv2$glmnet.fit$beta)

    expect_equal(Bos_cv00$relaxed$statlist, Bos_cv0$relaxed$statlist)
    expect_equal(Bos_cv0$relaxed$statlist, Bos_cv1$relaxed$statlist)
    expect_equal(Bos_cv1$relaxed$statlist, Bos_cv2$relaxed$statlist)
})


test_that("predict and coef work", {
    Bos_cvc0 <- coef(Bos_cv0, s="lambda.min")
    Bos_cvc1 <- coef(Bos_cv1, s="lambda.min")
    Bos_cvc2 <- coef(Bos_cv2, s="lambda.min")
    expect_equal(Bos_cvc0, Bos_cvc1)
    expect_equal(Bos_cvc1, Bos_cvc2)

    Bos_cvp0 <- predict(Bos_cv0, x, newoffset=Boston$rad)
    Bos_cvp1 <- predict(Bos_cv1, Boston, newoffset=Boston$rad)
    Bos_cvp2 <- predict(Bos_cv2, Boston, newoffset=Boston$rad)
    expect_equivalent(Bos_cvp0, Bos_cvp1)
    expect_equivalent(Bos_cvp1, Bos_cvp2)

    s <- median(Bos_cv0$lambda)

    Bos_cvp0 <- predict(Bos_cv0, x, s=s, newoffset=Boston$rad)
    Bos_cvp1 <- predict(Bos_cv1, Boston, s=s, newoffset=Boston$rad)
    Bos_cvp2 <- predict(Bos_cv2, Boston, s=s, newoffset=Boston$rad)
    expect_equivalent(Bos_cvp0, Bos_cvp1)
    expect_equivalent(Bos_cvp1, Bos_cvp2)
})

