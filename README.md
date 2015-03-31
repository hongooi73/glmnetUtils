# glmnetUtils

Some quality-of-life functions to streamline the process of fitting elastic net models with `glmnet`, specifically:

* `glmnet`'s interface is fairly bare-bones compared to many other R modelling functions. It expects an X matrix and a y vector, while others take a formula and data frame. So one of the first things you typically do is manually construct the matrix using `model.matrix()`.
* Similarly, getting predictions requires an X matrix, not a data frame as input. Again, this means having to call `model.matrix` rather than letting the package handle it automatically.
* `cv.glmnet` does crossvalidation for the lambda parameter, but not alpha. Its help page describes how to do crossvalidation for alpha, but no actual function is supplied.

