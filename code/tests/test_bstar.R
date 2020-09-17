# Test b.star.fun

source(here::here("code","1_operating_model.R"))

# with no fishing, pop goes to K
test_that("No fishing",{
  Ktest <- 100
  expect_equal(b.star.fun(F = 0,K = Ktest,A = 0,r = 1.28),Ktest)
})

test_that("MSY check",{
  A  <- 0
  Bmsy <- 70
  MSY <- 20
  K <- -(3*Bmsy^2 - 2*A*Bmsy)/(A - 2*Bmsy)
  r <- MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
  
  F.vec <- seq(0, 2, length.out = 100)
  
  b.star.vec <- sapply(F.vec, FUN=b.star.fun, K=K, A=A, r=r)
  b.star.vec <- replace(b.star.vec,which(b.star.vec=="NaN"),0)
  catch <- F.vec*b.star.vec
  plot(F.vec,catch)
  MSY.out <- max(catch)
  diff <- abs(MSY - MSY.out)
  
  expect_true(diff<0.01)
})