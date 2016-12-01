Sys.setenv(R_TESTS="")  # needed for cluster creation to work

library(testthat)
library(glmnetUtils)

test_check("glmnetUtils")
