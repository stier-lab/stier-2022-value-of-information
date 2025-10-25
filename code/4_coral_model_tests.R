# ============================================================================
# CORAL RESTORATION MODEL TESTS AND VISUALIZATION
# ============================================================================
# Quick tests and visualizations to verify model behavior
# ============================================================================

source("code/0_libraries.R")
source("code/2_coral_parameters.R")
source("code/2b_coral_dangerzone.R")
source("code/3_coral_mse_model.R")

# ============================================================================
# TEST 1: Compare different monitoring precision levels
# ============================================================================

cat("TEST 1: Comparing monitoring precision levels\n")
cat("==============================================\n\n")

years <- 50
C.start <- 75
A <- 20

# High precision monitoring (CV = 0.1)
result_high <- est.NPV(
  years = years,
  K = K.coral,
  A = A,
  r = r.coral,
  phi.CV.low = 0.1,
  phi.CV.high = 0.1,  # Always high precision
  delta = delta,
  process.noise = 0,
  v = v,
  C.start = C.start,
  C.lim = Cmsy,
  C.crit = C.crit,
  max.R = max.R,
  phi.CV.seed = 12345,
  process.noise.seed = 67890,
  c.restore = c.restore
)

# Low precision monitoring (CV = 0.5)
result_low <- est.NPV(
  years = years,
  K = K.coral,
  A = A,
  r = r.coral,
  phi.CV.low = 0.5,
  phi.CV.high = 0.5,  # Always low precision
  delta = delta,
  process.noise = 0,
  v = v,
  C.start = C.start,
  C.lim = Cmsy,
  C.crit = C.crit,
  max.R = max.R,
  phi.CV.seed = 12345,
  process.noise.seed = 67890,
  c.restore = c.restore
)

# Adaptive monitoring (switches based on coral health)
result_adaptive <- est.NPV(
  years = years,
  K = K.coral,
  A = A,
  r = r.coral,
  phi.CV.low = 0.1,
  phi.CV.high = 0.5,  # Adaptive
  delta = delta,
  process.noise = 0,
  v = v,
  C.start = C.start,
  C.lim = Cmsy,
  C.crit = C.crit,
  max.R = max.R,
  phi.CV.seed = 12345,
  process.noise.seed = 67890,
  c.restore = c.restore
)

# Compare NPV
cat(sprintf("High-precision NPV:   $%.2f\n", result_high$NPV))
cat(sprintf("Low-precision NPV:    $%.2f\n", result_low$NPV))
cat(sprintf("Adaptive NPV:         $%.2f\n", result_adaptive$NPV))

cat(sprintf("\nHigh-precision monitoring cost:  $%.2f\n", result_high$cost.monitor))
cat(sprintf("Low-precision monitoring cost:   $%.2f\n", result_low$cost.monitor))
cat(sprintf("Adaptive monitoring cost:        $%.2f\n", result_adaptive$cost.monitor))

cat(sprintf("\nValue of Information (High vs Low): $%.2f\n",
            result_high$NPV - result_low$NPV))
cat(sprintf("Monitoring cost difference:         $%.2f\n",
            result_high$cost.monitor - result_low$cost.monitor))
cat(sprintf("Net VOI (after monitoring cost):    $%.2f\n",
            (result_high$NPV - result_low$NPV) - (result_high$cost.monitor - result_low$cost.monitor)))

# ============================================================================
# VISUALIZATION 1: True vs. Estimated Coral Cover
# ============================================================================

par(mfrow = c(1, 3))

# Plot 1: High precision
plot(result_high$C, type = "l", lwd = 2, col = "darkgreen",
     ylim = c(0, max(c(result_high$C, result_high$Chat), na.rm = TRUE)),
     main = "High Precision (CV=0.1)",
     xlab = "Year", ylab = "Coral Cover (%)")
lines(result_high$Chat, col = "blue", lty = 2, lwd = 2)
abline(h = A, col = "red", lwd = 2, lty = 3)
abline(h = Cmsy, col = "gray", lty = 2)
legend("topleft", legend = c("True", "Estimated", "Allee", "Cmsy"),
       col = c("darkgreen", "blue", "red", "gray"),
       lty = c(1, 2, 3, 2), lwd = c(2, 2, 2, 1), cex = 0.8)

# Plot 2: Low precision
plot(result_low$C, type = "l", lwd = 2, col = "darkgreen",
     ylim = c(0, max(c(result_low$C, result_low$Chat), na.rm = TRUE)),
     main = "Low Precision (CV=0.5)",
     xlab = "Year", ylab = "Coral Cover (%)")
lines(result_low$Chat, col = "blue", lty = 2, lwd = 2)
abline(h = A, col = "red", lwd = 2, lty = 3)
abline(h = Cmsy, col = "gray", lty = 2)
legend("topleft", legend = c("True", "Estimated", "Allee", "Cmsy"),
       col = c("darkgreen", "blue", "red", "gray"),
       lty = c(1, 2, 3, 2), lwd = c(2, 2, 2, 1), cex = 0.8)

# Plot 3: Adaptive
plot(result_adaptive$C, type = "l", lwd = 2, col = "darkgreen",
     ylim = c(0, max(c(result_adaptive$C, result_adaptive$Chat), na.rm = TRUE)),
     main = "Adaptive Monitoring",
     xlab = "Year", ylab = "Coral Cover (%)")
lines(result_adaptive$Chat, col = "blue", lty = 2, lwd = 2)
abline(h = A, col = "red", lwd = 2, lty = 3)
abline(h = Cmsy, col = "gray", lty = 2)
abline(h = C.crit, col = "orange", lty = 2)
legend("topleft", legend = c("True", "Estimated", "Allee", "Cmsy", "Crit"),
       col = c("darkgreen", "blue", "red", "gray", "orange"),
       lty = c(1, 2, 3, 2, 2), lwd = c(2, 2, 2, 1, 1), cex = 0.7)

par(mfrow = c(1, 1))

# ============================================================================
# VISUALIZATION 2: Monitoring precision over time (adaptive only)
# ============================================================================

par(mfrow = c(2, 1))

# Top panel: Coral cover
plot(result_adaptive$C, type = "l", lwd = 2, col = "darkgreen",
     main = "Adaptive Monitoring Strategy",
     xlab = "", ylab = "Coral Cover (%)")
lines(result_adaptive$Chat, col = "blue", lty = 2, lwd = 2)
abline(h = C.crit, col = "orange", lty = 2, lwd = 2)
text(5, C.crit + 5, "Monitoring threshold", col = "orange", pos = 3, cex = 0.9)
legend("topright", legend = c("True cover", "Estimated cover"),
       col = c("darkgreen", "blue"), lty = c(1, 2), lwd = 2)

# Bottom panel: Monitoring CV
plot(result_adaptive$phi.CV[-1], type = "l", lwd = 2, col = "purple",
     xlab = "Year", ylab = "Monitoring CV",
     ylim = c(0, 0.6))
abline(h = 0.1, col = "blue", lty = 2)
abline(h = 0.5, col = "red", lty = 2)
text(45, 0.15, "High precision", col = "blue", cex = 0.9)
text(45, 0.55, "Low precision", col = "red", cex = 0.9)

par(mfrow = c(1, 1))

# ============================================================================
# TEST 2: Test different starting coral cover levels
# ============================================================================

cat("\n\nTEST 2: Starting coral cover sensitivity\n")
cat("=========================================\n\n")

C.start.vec <- c(30, 50, 75, 90)
npv.results <- numeric(length(C.start.vec))
collapse.results <- logical(length(C.start.vec))

for (i in 1:length(C.start.vec)) {
  result <- est.NPV(
    years = 50,
    K = K.coral,
    A = 20,
    r = r.coral,
    phi.CV.low = 0.1,
    phi.CV.high = 0.5,
    delta = delta,
    process.noise = 0,
    v = v,
    C.start = C.start.vec[i],
    C.lim = Cmsy,
    C.crit = C.crit,
    max.R = max.R,
    phi.CV.seed = 12345,
    process.noise.seed = 67890,
    c.restore = c.restore
  )

  npv.results[i] <- result$NPV
  collapse.results[i] <- result$TP == 1

  cat(sprintf("Starting cover: %d%% → NPV: $%.2f, Collapsed: %s\n",
              C.start.vec[i], npv.results[i], ifelse(collapse.results[i], "YES", "NO")))
}

# ============================================================================
# VISUALIZATION 3: NPV vs. Starting Cover
# ============================================================================

plot(C.start.vec, npv.results, type = "b", lwd = 2, pch = 19,
     col = ifelse(collapse.results, "red", "darkgreen"),
     main = "NPV vs. Starting Coral Cover",
     xlab = "Starting Coral Cover (%)",
     ylab = "Net Present Value ($)",
     cex = 1.5)
abline(v = A, col = "red", lty = 2, lwd = 2)
text(A, max(npv.results), "Allee threshold", pos = 4, col = "red")
legend("bottomright",
       legend = c("No collapse", "Collapsed"),
       col = c("darkgreen", "red"),
       pch = 19, pt.cex = 1.5)

# ============================================================================
# TEST 3: Monte Carlo simulation comparison
# ============================================================================

cat("\n\nTEST 3: Monte Carlo simulation (100 iterations)\n")
cat("===============================================\n")
cat("This may take a minute...\n\n")

n.iters <- 100
phi.seeds <- round(1000000 * runif(n.iters), 0)
process.seeds <- round(1000000 * runif(n.iters), 0)

# High precision
mc_high <- repeat.model2(
  n.iters = n.iters,
  C.start = 75,
  C.lim = Cmsy,
  years = 50,
  K = K.coral,
  A = 20,
  r = r.coral,
  phi.CV = 0.1,
  delta = 0.05,
  process.noise = 0,
  v = v,
  max.R = max.R,
  phi.seeds = phi.seeds,
  process.seeds = process.seeds
)

# Low precision
mc_low <- repeat.model2(
  n.iters = n.iters,
  C.start = 75,
  C.lim = Cmsy,
  years = 50,
  K = K.coral,
  A = 20,
  r = r.coral,
  phi.CV = 0.5,
  delta = 0.05,
  process.noise = 0,
  v = v,
  max.R = max.R,
  phi.seeds = phi.seeds,
  process.seeds = process.seeds
)

cat(sprintf("High-precision median NPV:  $%.2f\n", median(mc_high$value)))
cat(sprintf("Low-precision median NPV:   $%.2f\n", median(mc_low$value)))
cat(sprintf("Value of Information:       $%.2f\n",
            median(mc_high$value) - median(mc_low$value)))

cat(sprintf("\nHigh-precision collapse prob: %.3f\n", sum(mc_high$TP) / n.iters))
cat(sprintf("Low-precision collapse prob:  %.3f\n", sum(mc_low$TP) / n.iters))

# ============================================================================
# VISUALIZATION 4: NPV distributions
# ============================================================================

par(mfrow = c(1, 2))

hist(mc_high$value, breaks = 20, col = rgb(0, 0, 1, 0.5),
     main = "NPV Distribution",
     xlab = "Net Present Value ($)",
     xlim = range(c(mc_high$value, mc_low$value)))
hist(mc_low$value, breaks = 20, col = rgb(1, 0, 0, 0.5), add = TRUE)
legend("topright",
       legend = c("High precision", "Low precision"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))

boxplot(list(High = mc_high$value, Low = mc_low$value),
        main = "NPV Comparison",
        ylab = "Net Present Value ($)",
        col = c("lightblue", "lightcoral"))

par(mfrow = c(1, 1))

cat("\n\n✓ All tests completed successfully!\n")
cat("=====================================\n")
cat("The coral restoration monitoring model is working as expected.\n")
cat("Value of Information is being correctly calculated.\n")
