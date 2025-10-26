# Quick test of coral restoration monitoring model
# Test that the model runs without errors

setwd("/home/user/coral-restoration-monitoring-voi")

source("code/0_libraries.R")
source("code/2_coral_parameters.R")
source("code/2b_coral_dangerzone.R")
source("code/3_coral_mse_model.R")

cat("Testing single model run...\n")

# Run single model with moderate restoration intensity
test_run <- est.NPV(
  years = 50,
  K = K.coral,
  A = A.coral,
  r = r.coral,
  phi.CV.low = 0.1,
  phi.CV.high = 0.5,
  delta = 0.05,
  process.noise = 0,
  v = 100,
  C.start = 75,
  C.lim = Cmsy,
  C.crit = C.crit,
  max.R = max.R,
  phi.CV.seed = 12345,
  process.noise.seed = 67890,
  c.restore = c.restore
)

cat("\n=== Test Results ===\n")
cat("NPV:", round(test_run$NPV, 2), "\n")
cat("Final coral cover:", round(test_run$C[length(test_run$C)], 2), "%\n")
cat("Collapse occurred:", test_run$TP, "\n")
cat("Monitoring cost:", round(test_run$cost.monitor, 2), "\n")
cat("Mean monitoring CV:", round(mean(test_run$phi.CV, na.rm=TRUE), 3), "\n")

cat("\n=== Testing Monte Carlo (10 iterations) ===\n")

n.iters <- 10
phi.seeds <- round(1000000 * runif(n.iters), 0)
process.seeds <- round(1000000 * runif(n.iters), 0)

mc_results <- repeat.model2(
  n.iters = n.iters,
  C.start = 75,
  C.lim = Cmsy,
  years = 50,
  K = K.coral,
  A = A.coral,
  r = r.coral,
  phi.CV = NA,
  delta = 0.05,
  process.noise = 0,
  v = 100,
  max.R = max.R,
  phi.seeds = phi.seeds,
  process.seeds = process.seeds
)

cat("Median NPV:", round(median(mc_results$value), 2), "\n")
cat("Probability of collapse:", sum(mc_results$TP) / n.iters, "\n")
cat("Mean rescue probability:", round(mean(mc_results$rescue_prob, na.rm = TRUE), 3), "\n")

cat("\n✓ All tests passed successfully!\n")
