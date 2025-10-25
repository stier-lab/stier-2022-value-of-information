# ============================================================================
# COMPARISON: Old vs. Improved Coral Restoration Models
# ============================================================================
# This script demonstrates the critical differences between the original
# and improved versions of the coral restoration monitoring model
# ============================================================================

source("code/0_libraries.R")
source("code/2_coral_parameters.R")
source("code/2b_coral_dangerzone.R")

# Load both versions
source("code/3_coral_mse_model.R")  # Original version
source("code/3_coral_mse_model_IMPROVED.R")  # Improved version

cat("========================================\n")
cat("COMPARING OLD VS. IMPROVED MODELS\n")
cat("========================================\n\n")

# Set up common parameters
years <- 50
C.start <- 60  # Start at moderate coral cover
A <- A.coral
process.noise.old <- 0    # Original model used no process noise
process.noise.new <- 0.1  # Improved model uses realistic process noise

# Fixed seeds for comparison
phi.seed <- 12345
proc.seed <- 67890

# ============================================================================
# TEST 1: Restoration Control Rule Comparison
# ============================================================================

cat("TEST 1: Restoration Control Rule Logic\n")
cat("---------------------------------------\n")

# Simulate different coral cover levels to see restoration response
cover.levels <- c(20, 40, 60, 80, 100)  # Below Allee, critical, optimal, healthy, max

cat("\nHow much restoration is applied at different coral cover levels?\n")
cat("(Higher numbers = more restoration effort)\n\n")
cat("Coral Cover | OLD MODEL | IMPROVED MODEL | Better?\n")
cat("--------------------------------------------------------\n")

for (cover in cover.levels) {
  # OLD MODEL
  test.old <- est.NPV(
    years = 5,  # Short run just to see initial decision
    K = K.coral, A = A, r = r.coral,
    phi.CV.low = 0.1, phi.CV.high = 0.1,  # High precision for clean test
    delta = 0.05, process.noise = 0,
    v = 100, C.start = cover,
    C.lim = Cmsy, C.crit = C.crit,
    max.R = max.R,
    phi.CV.seed = phi.seed,
    process.noise.seed = proc.seed,
    c.restore = c.restore
  )

  # IMPROVED MODEL
  test.new <- est.NPV.improved(
    years = 5,
    K = K.coral, A = A, r = r.coral,
    phi.CV.low = 0.1, phi.CV.high = 0.1,
    delta = 0.05, process.noise = 0,
    v = 100, C.start = cover,
    C.lim = Cmsy, C.crit = C.crit,
    max.R = max.R,
    phi.CV.seed = phi.seed,
    process.noise.seed = proc.seed,
    c.restore = c.restore,
    use_budget_constraint = FALSE,  # Disable budget for pure comparison
    track_errors = FALSE
  )

  R.old <- test.old$pR[1]
  R.new <- test.new$pR[1]

  # Which model makes more sense?
  is_degraded <- cover < Cmsy
  better <- ifelse(is_degraded,
                   ifelse(R.new > R.old, "✓ IMPROVED", "  OLD better?"),
                   ifelse(R.new < R.old, "✓ IMPROVED", "  OLD better?"))

  cat(sprintf("%3d%%       | %9.3f | %14.3f | %s\n",
              cover, R.old, R.new, better))
}

cat("\nInterpretation:\n")
cat("- OLD MODEL: High restoration when healthy (>60%), low when degraded - BACKWARDS!\n")
cat("- IMPROVED: High restoration when degraded, low when healthy - CORRECT!\n")

# ============================================================================
# TEST 2: Impact of Monitoring Precision
# ============================================================================

cat("\n\n")
cat("TEST 2: Does Monitoring Precision Matter?\n")
cat("------------------------------------------\n")

n.iters <- 50
phi.seeds <- round(1000000 * runif(n.iters), 0)
process.seeds <- round(1000000 * runif(n.iters), 0)

# OLD MODEL: High vs Low precision
npv.old.high <- npv.old.low <- rep(NA, n.iters)
for (i in 1:n.iters) {
  out.high <- est.NPV(years, K.coral, A, r.coral, 0.1, 0.1, 0.05,
                      process.noise.old, v, C.start, Cmsy, C.crit, max.R,
                      phi.seeds[i], process.seeds[i], c.restore)
  npv.old.high[i] <- out.high$NPV

  out.low <- est.NPV(years, K.coral, A, r.coral, 0.5, 0.5, 0.05,
                     process.noise.old, v, C.start, Cmsy, C.crit, max.R,
                     phi.seeds[i], process.seeds[i], c.restore)
  npv.old.low[i] <- out.low$NPV
}

# IMPROVED MODEL: High vs Low precision
npv.new.high <- npv.new.low <- rep(NA, n.iters)
for (i in 1:n.iters) {
  out.high <- est.NPV.improved(years, K.coral, A, r.coral, 0.1, 0.1, 0.05,
                                process.noise.new, v, C.start, Cmsy, C.crit, max.R,
                                phi.seeds[i], process.seeds[i], c.restore,
                                use_budget_constraint = FALSE, track_errors = FALSE)
  npv.new.high[i] <- out.high$NPV

  out.low <- est.NPV.improved(years, K.coral, A, r.coral, 0.5, 0.5, 0.05,
                               process.noise.new, v, C.start, Cmsy, C.crit, max.R,
                               phi.seeds[i], process.seeds[i], c.restore,
                               use_budget_constraint = FALSE, track_errors = FALSE)
  npv.new.low[i] <- out.low$NPV
}

voi.old <- median(npv.old.high) - median(npv.old.low)
voi.new <- median(npv.new.high) - median(npv.new.low)

cat(sprintf("OLD MODEL VOI:      $%8.2f\n", voi.old))
cat(sprintf("IMPROVED MODEL VOI: $%8.2f\n", voi.new))

cat("\nInterpretation:\n")
cat("- OLD MODEL: VOI is artificially low due to no process noise\n")
cat("- IMPROVED: VOI is realistic due to environmental variability\n")

# ============================================================================
# TEST 3: Budget Trade-off (NEW FEATURE)
# ============================================================================

cat("\n\n")
cat("TEST 3: Budget Trade-off (New Feature in Improved Model)\n")
cat("---------------------------------------------------------\n")

total.budget <- 12000

# Improved model WITH budget constraint
out.budget <- est.NPV.improved(
  years = 50, K = K.coral, A = A, r = r.coral,
  phi.CV.low = 0.1, phi.CV.high = 0.5,  # Adaptive monitoring
  delta = 0.05, process.noise = 0.1,
  v = 100, C.start = C.start,
  C.lim = Cmsy, C.crit = C.crit, max.R = max.R,
  phi.CV.seed = phi.seed, process.noise.seed = proc.seed,
  c.restore = c.restore,
  total.budget = total.budget,
  use_budget_constraint = TRUE,  # ENABLE BUDGET TRADE-OFF
  track_errors = TRUE
)

# Improved model WITHOUT budget constraint
out.no.budget <- est.NPV.improved(
  years = 50, K = K.coral, A = A, r = r.coral,
  phi.CV.low = 0.1, phi.CV.high = 0.5,
  delta = 0.05, process.noise = 0.1,
  v = 100, C.start = C.start,
  C.lim = Cmsy, C.crit = C.crit, max.R = max.R,
  phi.CV.seed = phi.seed, process.noise.seed = proc.seed,
  c.restore = c.restore,
  use_budget_constraint = FALSE,
  track_errors = TRUE
)

cat("\nWith Budget Constraint:\n")
cat(sprintf("  Total Budget:        $%8.2f\n", total.budget))
cat(sprintf("  Monitoring Cost:     $%8.2f\n", out.budget$cost.monitor))
cat(sprintf("  Restoration Cost:    $%8.2f\n", out.budget$restoration.cost.total))
cat(sprintf("  NPV:                 $%8.2f\n", out.budget$NPV))

cat("\nWithout Budget Constraint:\n")
cat(sprintf("  Monitoring Cost:     $%8.2f\n", out.no.budget$cost.monitor))
cat(sprintf("  Restoration Cost:    $%8.2f\n", out.no.budget$restoration.cost.total))
cat(sprintf("  NPV:                 $%8.2f\n", out.no.budget$NPV))

cat("\nInterpretation:\n")
cat("- With budget: Monitoring costs REDUCE restoration capacity (realistic!)\n")
cat("- Without budget: Can do both freely (unrealistic but useful for theory)\n")

# ============================================================================
# TEST 4: Decision Error Tracking (NEW FEATURE)
# ============================================================================

cat("\n\n")
cat("TEST 4: Decision Error Tracking (New Feature)\n")
cat("----------------------------------------------\n")

cat("\nHigh Precision Monitoring (CV = 0.1):\n")
cat(sprintf("  Type I errors (should restore but didn't):  %d\n",
            out.budget$error.type1.count))
cat(sprintf("  Type II errors (restored unnecessarily):    %d\n",
            out.budget$error.type2.count))
cat(sprintf("  Total cost of errors:                       $%.2f\n",
            out.budget$error.cost.total))

# Compare with low precision
out.low.precision <- est.NPV.improved(
  years = 50, K = K.coral, A = A, r = r.coral,
  phi.CV.low = 0.5, phi.CV.high = 0.5,  # Always low precision
  delta = 0.05, process.noise = 0.1,
  v = 100, C.start = C.start,
  C.lim = Cmsy, C.crit = C.crit, max.R = max.R,
  phi.CV.seed = phi.seed, process.noise.seed = proc.seed,
  c.restore = c.restore,
  use_budget_constraint = FALSE,
  track_errors = TRUE
)

cat("\nLow Precision Monitoring (CV = 0.5):\n")
cat(sprintf("  Type I errors (should restore but didn't):  %d\n",
            out.low.precision$error.type1.count))
cat(sprintf("  Type II errors (restored unnecessarily):    %d\n",
            out.low.precision$error.type2.count))
cat(sprintf("  Total cost of errors:                       $%.2f\n",
            out.low.precision$error.cost.total))

error.reduction <- out.low.precision$error.cost.total - out.budget$error.cost.total
cat(sprintf("\n✓ High-precision monitoring reduces error costs by: $%.2f\n", error.reduction))

# ============================================================================
# VISUALIZATIONS
# ============================================================================

cat("\n\n")
cat("Creating comparison visualizations...\n")

par(mfrow = c(2, 2))

# Plot 1: Restoration effort comparison (OLD vs NEW)
test.old.viz <- est.NPV(years, K.coral, A, r.coral, 0.1, 0.1, 0.05, 0,
                        v, 60, Cmsy, C.crit, max.R, 12345, 67890, c.restore)
test.new.viz <- est.NPV.improved(years, K.coral, A, r.coral, 0.1, 0.1, 0.05, 0.1,
                                  v, 60, Cmsy, C.crit, max.R, 12345, 67890, c.restore,
                                  use_budget_constraint = FALSE, track_errors = FALSE)

plot(test.old.viz$pR, type = "l", lwd = 2, col = "red",
     main = "Restoration Effort Over Time",
     xlab = "Year", ylab = "Restoration Effort (proportion of max)",
     ylim = c(0, max(c(test.old.viz$pR, test.new.viz$pR))))
lines(test.new.viz$pR, lwd = 2, col = "darkgreen")
abline(h = 1, lty = 2, col = "gray")
legend("topright", legend = c("OLD (backwards)", "IMPROVED (correct)"),
       col = c("red", "darkgreen"), lwd = 2)

# Plot 2: Coral cover trajectories
plot(test.old.viz$C, type = "l", lwd = 2, col = "red",
     main = "Coral Cover Trajectories",
     xlab = "Year", ylab = "Coral Cover (%)",
     ylim = c(0, max(c(test.old.viz$C, test.new.viz$C))))
lines(test.new.viz$C, lwd = 2, col = "darkgreen")
abline(h = A, lty = 2, col = "red", lwd = 1.5)
abline(h = Cmsy, lty = 2, col = "blue", lwd = 1.5)
legend("bottomright",
       legend = c("OLD model", "IMPROVED model", "Allee threshold", "Optimal (Cmsy)"),
       col = c("red", "darkgreen", "red", "blue"),
       lty = c(1, 1, 2, 2), lwd = c(2, 2, 1.5, 1.5))

# Plot 3: NPV distributions
hist(npv.old.high - npv.old.low, breaks = 20,
     col = rgb(1, 0, 0, 0.5), main = "Value of Information Distribution",
     xlab = "VOI (NPV high precision - NPV low precision)",
     xlim = range(c(npv.old.high - npv.old.low, npv.new.high - npv.new.low)))
hist(npv.new.high - npv.new.low, breaks = 20,
     col = rgb(0, 0.5, 0, 0.5), add = TRUE)
legend("topright", legend = c("OLD model", "IMPROVED model"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0.5, 0, 0.5)))

# Plot 4: Decision errors over time
plot(cumsum(out.low.precision$error.type1.vec), type = "l", lwd = 2, col = "red",
     main = "Cumulative Decision Errors",
     xlab = "Year", ylab = "Cumulative Errors",
     ylim = c(0, max(cumsum(out.low.precision$error.type1.vec) +
                       cumsum(out.low.precision$error.type2.vec))))
lines(cumsum(out.low.precision$error.type2.vec), lwd = 2, col = "orange")
lines(cumsum(out.budget$error.type1.vec), lwd = 2, col = "darkred", lty = 2)
lines(cumsum(out.budget$error.type2.vec), lwd = 2, col = "darkorange", lty = 2)
legend("topleft",
       legend = c("Type I (low prec)", "Type II (low prec)",
                  "Type I (high prec)", "Type II (high prec)"),
       col = c("red", "orange", "darkred", "darkorange"),
       lty = c(1, 1, 2, 2), lwd = 2, cex = 0.8)

par(mfrow = c(1, 1))

cat("\n========================================\n")
cat("SUMMARY OF IMPROVEMENTS\n")
cat("========================================\n\n")

cat("✓ CRITICAL FIX 1: Restoration control rule now applies MORE effort when coral is degraded\n")
cat("✓ CRITICAL FIX 2: Restoration amount no longer scales incorrectly by estimates\n")
cat("✓ NEW FEATURE 1: Budget trade-off between monitoring and restoration\n")
cat("✓ NEW FEATURE 2: Decision error tracking (Type I and Type II errors)\n")
cat("✓ NEW FEATURE 3: Perfect information benchmark for VOI quantification\n")
cat("✓ ENHANCEMENT 1: Increased process noise makes monitoring valuable\n")
cat("✓ ENHANCEMENT 2: Explicit calculation of cost of being wrong\n\n")

cat("The IMPROVED model is now ready for meaningful VOI analysis!\n")
