# Test b.star.fun

source(here::here("code","1_operating_model.R"))

# with no fishing, pop goes to K
test_that("No fishing",{
  Ktest <- 100
  expect_equal(b.star.fun(F = 0,K = Ktest,A = 0,r = 1.28),Ktest)
})

# test_that("FMSY check"){
#   Fmsy <- 0.7
#   F.vec <- seq(0, 2*Fmsy, length.out = 100)
#   b.star.vec <- sapply(F.vec, FUN=b.star.fun, K=K, A=A, r=r)
#   b.star.vec <- replace(b.star.vec,which(b.star.vec=="NaN"),0)
#   which.max(b.star.vec)
# }

# To run tests:
#testthat::test_dir(here::here("code","tests"))
