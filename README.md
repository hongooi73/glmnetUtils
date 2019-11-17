# glmnetUtils

[![Build Status](https://dev.azure.com/hongooi/glmnetUtils/_apis/build/status/Hong-Revo.glmnetUtils?branchName=master)](https://dev.azure.com/hongooi/glmnetUtils/_build/latest?definitionId=1&branchName=master)

Some quality-of-life functions to streamline the process of fitting elastic net models with `glmnet`, specifically:

* `glmnet.formula` provides a formula/data frame interface to `glmnet`.
* `cv.glmnet.formula` does a similar thing for `cv.glmnet`.
* Methods for `predict` and `coef` for both the above.
* A function `cva.glmnet` to choose both the alpha and lambda parameters via cross-validation, following the approach described in the help page for `cv.glmnet`. Optionally does the cross-validation in parallel.
* Methods for `plot`, `predict` and `coef` for the above.

You can install the development version from Github using `devtools::install_github`.

    install.packages("devtools")
    library(devtools)
    install_github("hong-revo/glmnetUtils")
    library(glmnetUtils)
