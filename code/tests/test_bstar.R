# Test b.star.fun
# 
source(here::here("code","1_operating_model.R"))
library(testthat)
Bmsy

b.equilib <- sapply(F.vec, FUN=b.star.fun, K=K, A=A, r=r)
b.equilib <- replace(b.star.vec,which(b.star.vec=="NaN"),0)

test_that("No fishing",{
  Ktest = 100
  expect_equal(b.star.fun(F = 0,K = Ktest,A = 0,r = 1.28),Ktest)
})

testthat::test_dir("tests")
