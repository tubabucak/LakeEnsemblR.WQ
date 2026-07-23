# This file is part of the standard testthat setup: `devtools::test()` and
# `R CMD check` invoke it, which in turn runs every tests/testthat/test-*.R
# file. See https://testthat.r-lib.org/reference/test_package.html.

library(testthat)
library(LakeEnsemblR.WQ)

test_check("LakeEnsemblR.WQ")
