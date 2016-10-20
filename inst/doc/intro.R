## ---- echo=FALSE---------------------------------------------------------
library(glmnetUtils)

## ------------------------------------------------------------------------
# least squares regression
(mtcarsMod <- glmnet(mpg ~ cyl + disp + hp, data=mtcars))

# multinomial logistic regression with specified elastic net alpha parameter
(irisMod <- glmnet(Species ~ ., data=iris, family="multinomial", alpha=0.5))

# Poisson regression with an offset
(InsMod <- glmnet(Claims ~ District + Group + Age, data=MASS::Insurance,
                  family="poisson", offset=log(Holders)))

## ---- eval=FALSE---------------------------------------------------------
#  # least squares regression: get predictions for lambda=1
#  predict(mtcarsMod, newdata=mtcars, s=1)
#  
#  # multinomial logistic regression: get predicted class
#  predict(irisMod, newdata=iris, type="class")
#  
#  # Poisson regression: need to specify offset
#  predict(InsMod, newdata=MASS::Insurance, offset=log(Holders))

## ------------------------------------------------------------------------
mtcarsX <- as.matrix(mtcars[c("cyl", "disp", "hp")])
mtcarsY <- mtcars$mpg
mtcarsMod2 <- glmnet(mtcarsX, mtcarsY)

summary(as.numeric(predict(mtcarsMod, mtcars) - 
                   predict(mtcarsMod2, mtcarsX)))

## ---- eval=FALSE---------------------------------------------------------
#  # generate sample (uncorrelated) data of a given size
#  makeSampleData <- function(N, P)
#  {
#      X <- matrix(rnorm(N*P), nrow=N)
#      data.frame(y=rnorm(N), X)
#  }
#  
#  # test for three sizes: 100/1000/10000 predictors
#  df1 <- makeSampleData(N=1000, P=100)
#  df2 <- makeSampleData(N=1000, P=1000)
#  df3 <- makeSampleData(N=1000, P=10000)
#  
#  library(microbenchmark)
#  res <- microbenchmark(
#      glmnet(y ~ ., df1, use.model.frame=TRUE),
#      glmnet(y ~ ., df1, use.model.frame=FALSE),
#      glmnet(y ~ ., df2, use.model.frame=TRUE),
#      glmnet(y ~ ., df2, use.model.frame=FALSE),
#      glmnet(y ~ ., df3, use.model.frame=TRUE),
#      glmnet(y ~ ., df3, use.model.frame=FALSE),
#      times=10
#  )
#  print(res, unit="s", digits=2)

## ---- eval=FALSE---------------------------------------------------------
#  df4 <- makeSampleData(N=1000, P=100000)
#  
#  glmnet(y ~ ., df4, use.model.frame=TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  glmnet(y ~ ., df4, use.model.frame=FALSE)

## ---- echo=FALSE---------------------------------------------------------
if(!file.exists("~/Leukemia.rdata"))
    download.file("http://web.stanford.edu/~hastie/glmnet/glmnetData/Leukemia.RData", "~/Leukemia.rdata", mode="wb")

## ------------------------------------------------------------------------
# Leukemia dataset from Trevor Hastie's website:
# http://web.stanford.edu/~hastie/glmnet/glmnetData/Leukemia.RData
load("~/Leukemia.rdata")
leuk <- do.call(data.frame, Leukemia)

leukMod <- cvAlpha.glmnet(y ~ ., data=leuk, family="binomial")
leukMod

## ---- fig.height=5, fig.width=7------------------------------------------
plot(leukMod)

## ---- fig.height=5, fig.width=7------------------------------------------
plot(leukMod$modlist[[10]])  # alpha = 0.729

