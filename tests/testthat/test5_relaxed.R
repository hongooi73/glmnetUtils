context("relaxed glmnet")


test_that("relaxed glmnet works", {
    x <- as.matrix(iris[-5])
    y <- as.matrix(iris$Species)
    mod00 <- glmnet::glmnet(x, y, family="multinomial", alpha=0.5, relax=TRUE)
    mod0 <- glmnet(x, y, family="multinomial", alpha=0.5, relax=TRUE)
    mod1 <- glmnet(Species ~ ., data=iris, family="multinomial", relax=TRUE)
    expect_equivalent(mod00$beta, mod0$beta)
    expect_equivalent(mod0$beta, mod1$beta)

    expect_equivalent(mod00$relaxed$beta, mod0$relaxed$beta)
    expect_equivalent(mod0$relaxed$beta, mod1$relaxed$beta)
})


test_that("relaxed predict and coef work", {
    x <- as.matrix(iris[-5])
    y <- iris$Species
    mod1.0 <- glmnet(x, y, family="multinomial", relax=TRUE)
    mod1.1 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=TRUE, relax=TRUE)
    mod1.2 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=FALSE, relax=TRUE)
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
})


test_that("relaxed predict and coef work with nondefault gamma", {
    x <- as.matrix(iris[-5])
    y <- iris$Species
    mod1.0 <- glmnet(x, y, family="multinomial", relax=TRUE)
    mod1.1 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=TRUE, relax=TRUE)
    mod1.2 <- glmnet(Species ~ ., data=iris, family="multinomial", use.model.frame=FALSE, relax=TRUE)

    pred1.0 <- predict(mod1.0, as.matrix(iris[-5]), type="class", gamma=0.5)
    pred1.1 <- predict(mod1.1, iris, type="class", gamma=0.5)
    pred1.2 <- predict(mod1.2, iris, type="class", gamma=0.5)
    expect_equivalent(pred1.0, pred1.1)  # expect_equivalent because dimnames not always preserved
    expect_equivalent(pred1.1, pred1.2)
    coef1.0 <- coef(mod1.0)
    coef1.1 <- coef(mod1.1)
    coef1.2 <- coef(mod1.2)
    expect_equal(coef1.0, coef1.1)
    expect_equal(coef1.1, coef1.2)
})
