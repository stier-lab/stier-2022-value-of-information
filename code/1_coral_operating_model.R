# ============================================================================
# CORAL POPULATION DYNAMICS - OPERATING MODEL
# ============================================================================
# Reparameterized logistic model with Allee effect and restoration effort
#
# dC/dt = r*C*(1 - C/K)*(C/K - A/K) + R
#
# Where:
#   C = Coral cover (%)
#   K = Carrying capacity (maximum sustainable coral cover)
#   A = Allee threshold (critical minimum for self-sustaining population)
#   r = Population growth rate
#   R = Restoration effort (transplanting, coral gardening, etc.)
# ============================================================================

# CORAL GROWTH FUNCTION
# Returns the rate of change in coral cover
dcdt.fun <- function(C, K, A, r, R){
  # Natural dynamics: logistic growth with Allee effect
  natural_growth <- r * C * (1 - C/K) * (C/K - A/K)

  # Total change: natural growth + restoration effort
  natural_growth + R
}

# ============================================================================
# VISUALIZATION: Coral dynamics under different Allee thresholds
# ============================================================================

C.vec <- seq(0, 100, by = 1)  # Coral cover from 0-100%

# Test different Allee effect thresholds
A.vec <- c(-10, 0, 10, 15, 20, 30)  # Negative A = no Allee effect

par(mfrow = c(3, 2))

for (i in 1:length(A.vec)){
  A <- A.vec[i]
  Cmsy <- 60

  # Calculate carrying capacity given Cmsy and A
  K <- -(3*Cmsy^2 - 2*A*Cmsy)/(A - 2*Cmsy)

  MGR <- 15  # Maximum growth rate
  r <- MGR/(Cmsy*(1 - Cmsy/K)*(Cmsy/K - A/K))
  Rmsy <- MGR/Cmsy

  # Calculate growth rate at each coral cover level (no restoration)
  dcdt <- sapply(C.vec, FUN = dcdt.fun, K = K, A = A, r = r, R = 0)

  # Plot natural dynamics
  plot(C.vec, dcdt, type = "l", lwd = 2,
       xlab = "Coral Cover (%)",
       ylab = "Rate of Change (% per year)",
       main = paste("Allee Threshold A =", A),
       ylim = c(-10, 20))

  # Add zero line (equilibrium points where dcdt = 0)
  abline(h = 0, col = "gray", lty = 2)

  # Add restoration mortality line (effort needed to offset decline)
  lines(C.vec, Rmsy * C.vec, col = "red", lwd = 2)

  # Mark critical thresholds
  abline(v = A, col = "red", lty = 3)      # Allee threshold
  abline(v = Cmsy, col = "blue", lty = 3)  # Optimal cover

  legend("topleft",
         legend = c("Natural growth", "Restoration", "Allee threshold", "Optimal cover"),
         col = c("black", "red", "red", "blue"),
         lty = c(1, 1, 3, 3),
         lwd = c(2, 2, 1, 1),
         cex = 0.7)
}

par(mfrow = c(1, 1))

# ============================================================================
# EQUILIBRIUM ANALYSIS: Analytical solution for equilibrium coral cover
# ============================================================================

# Calculate equilibrium coral cover given restoration effort
c.star.fun <- function(R, K, A, r){
  # Solve quadratic equation for equilibrium
  discriminant <- r*(4*R*K^2 - A^2*r - K^2*r + 2*A*K*r)

  if(discriminant < 0) return(0)  # No equilibrium (extinction)

  max(0, (A*r + K*r + sqrt(discriminant))/(2*r))
}

# Test equilibrium under different restoration efforts
A <- 20
Cmsy <- 60
MGR <- 15
K <- -(3*Cmsy^2 - 2*A*Cmsy)/(A - 2*Cmsy)
r <- MGR/(Cmsy*(1 - Cmsy/K)*(Cmsy/K - A/K))
Rmsy <- MGR/Cmsy

R.vec <- seq(0, 2*Rmsy, length.out = 100)
c.star.vec <- sapply(R.vec, FUN = c.star.fun, K = K, A = A, r = r)
c.star.vec <- replace(c.star.vec, which(is.nan(c.star.vec)), 0)

# ============================================================================
# ECONOMIC ANALYSIS: Value vs. Cost of restoration
# ============================================================================

v <- 100           # Value per unit coral cover (ecosystem services)
c.restore <- 500   # Cost per unit restoration effort

par(mfrow = c(1, 2))

# Plot 1: Equilibrium coral cover vs. restoration effort
plot(R.vec, c.star.vec,
     type = "l", lwd = 2, col = "darkgreen",
     xlab = "Restoration Effort (R)",
     ylab = "Equilibrium Coral Cover (%)",
     main = "Coral Cover vs. Restoration Effort")
abline(h = A, col = "red", lty = 2)
abline(h = Cmsy, col = "blue", lty = 2)
legend("bottomright",
       legend = c("Equilibrium cover", "Allee threshold", "Optimal cover"),
       col = c("darkgreen", "red", "blue"),
       lty = c(1, 2, 2),
       lwd = c(2, 1, 1))

# Plot 2: Economic value vs. cost
plot(R.vec, v * c.star.vec,
     type = "l", lwd = 2, col = "blue",
     xlab = "Restoration Effort (R)",
     ylab = "Annual Value or Cost",
     main = "Restoration Economics",
     ylim = c(0, max(v * c.star.vec, c.restore * R.vec)))

lines(R.vec, c.restore * R.vec, type = "l", lwd = 2, col = "red")

# Find optimal restoration effort (where marginal benefit = marginal cost)
net_benefit <- v * c.star.vec - c.restore * R.vec
optimal_idx <- which.max(net_benefit)
abline(v = R.vec[optimal_idx], col = "darkgreen", lty = 2)

legend("topleft",
       legend = c("Ecosystem value", "Restoration cost", "Optimal effort"),
       col = c("blue", "red", "darkgreen"),
       lty = c(1, 1, 2),
       lwd = c(2, 2, 1))

par(mfrow = c(1, 1))

# ============================================================================
# KEY INSIGHTS:
# ============================================================================
# 1. Below Allee threshold A, coral cannot self-sustain (negative growth)
# 2. At Cmsy, natural growth is maximized (optimal restoration target)
# 3. Above K, coral cover cannot increase further (carrying capacity)
# 4. Restoration effort can prevent collapse but has diminishing returns
# 5. Optimal restoration balances ecosystem value against restoration cost
# ============================================================================
