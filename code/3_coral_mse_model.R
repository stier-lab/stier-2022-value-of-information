# ============================================================================
# CORAL RESTORATION MSE MODEL
# ============================================================================
# Management Strategy Evaluation for coral restoration monitoring
#
# Three-component MSE framework:
# 1. OPERATING MODEL: True coral population dynamics
# 2. SAMPLING MODEL: Monitoring with observation uncertainty
# 3. MANAGEMENT MODEL: Restoration decisions based on estimated coral cover
#
# Key question: Is precise monitoring worth the cost, or should we
# invest those resources in restoration action instead?
# ============================================================================

source("code/0_libraries.R")
source("code/2b_coral_dangerzone.R")
source("code/2_coral_parameters.R")

# ============================================================================
# FUNCTION: est.NPV
# ============================================================================
# Simulates coral restoration over time and calculates net present value
#
# Returns:
#   - NPV: Net present value of ecosystem services minus restoration costs
#   - C: True coral cover trajectory
#   - Chat: Estimated coral cover trajectory
#   - R: Restoration effort each year
#   - phi.CV: Monitoring precision over time
#   - cost.monitor: Total monitoring cost
#   - TP: Did coral dip below Allee threshold?
#   - rescue: Number of recovery events
# ============================================================================

est.NPV <- function(years, K, A, r, phi.CV.low, phi.CV.high, delta,
                    process.noise, v, C.start, C.lim, C.crit, max.R,
                    phi.CV.seed, process.noise.seed, c.restore){

  # Calculate reference points given parameters
  Cmsy <- A/3 + K/3 + (A^2 - A*K + K^2)^(1/2)/3
  MGR <- r * Cmsy * (1 - Cmsy/K) * (Cmsy/K - A/K)  # Maximum growth rate
  Rmsy <- MGR / Cmsy
  C.lim <- Cmsy

  # Initialize vectors
  C.vec <- rep(NA, years)        # True coral cover
  C.vec[1] <- C.start
  Chat.vec <- rep(NA, years)     # Estimated coral cover
  Chat.vec[1] <- C.start
  ES.vec <- rep(NA, years)       # Ecosystem service value
  R.vec <- rep(NA, years)        # Restoration effort
  phi.CV <- rep(NA, years)       # Monitoring precision

  # Generate observation errors (lognormal)
  set.seed(phi.CV.seed)
  C.errors.low <- exp(rnorm(years, mean = (0 - phi.CV.low^2/2), sd = phi.CV.low))
  C.errors.high <- exp(rnorm(years, mean = (0 - phi.CV.high^2/2), sd = phi.CV.high))

  # Generate process errors
  set.seed(process.noise.seed)
  process.errors <- rnorm(years, mean = 0, sd = process.noise)

  # Solve for restoration control rule parameters
  # Linear interpolation between C.lim and Cmsy
  Ro <- -(max.R * C.lim) / (Cmsy - C.lim)
  b <- max.R / (Cmsy - C.lim)

  # ============================================================================
  # SIMULATION LOOP
  # ============================================================================

  for (i in 1:years){

    # ------------------------------------------------------------------------
    # MANAGEMENT MODEL: Determine restoration effort based on estimated cover
    # ------------------------------------------------------------------------
    if (Chat.vec[i] > Cmsy) R <- max.R          # High effort when healthy
    if (Chat.vec[i] <= Cmsy) R <- Ro + b * Chat.vec[i]  # Scale with cover
    if (Chat.vec[i] <= C.lim) R <- 0            # Stop if too degraded

    R.vec[i] <- R / max.R  # Store as proportion of max

    # Ecosystem service value (proportional to coral cover)
    ES.vec[i] <- v * C.vec[i]

    # ------------------------------------------------------------------------
    # OPERATING MODEL: True coral population dynamics
    # ------------------------------------------------------------------------
    # Natural dynamics: logistic growth with Allee effect
    production <- r * (1 - C.vec[i]/K) * (C.vec[i]/K - A/K) + process.errors[i]

    # Update coral cover: natural growth + restoration effort
    C.vec[i+1] <- max(0.1, C.vec[i] + C.vec[i] * production + R * Chat.vec[i])

    # ------------------------------------------------------------------------
    # SAMPLING MODEL: Monitoring with adaptive precision
    # ------------------------------------------------------------------------
    # High precision monitoring when near danger zone, low precision otherwise
    Chat.vec[i+1] <- ifelse(Chat.vec[i] < C.crit,
                            C.vec[i+1] * C.errors.low[i],
                            C.vec[i+1] * C.errors.high[i])

    # Record monitoring precision
    phi.CV[i+1] <- ifelse(Chat.vec[i] < C.crit,
                          phi.CV.low,
                          phi.CV.high)
  }

  # ============================================================================
  # CALCULATE PERFORMANCE METRICS
  # ============================================================================

  # RESCUE PROBABILITY: Recovery from near-collapse events
  temp_mat <- matrix(NA, nrow = length(C.vec) + 1, ncol = 2)
  threshold <- 0.8 * Cmsy

  for(j in 1:length(C.vec)){
    # Number of times recovered above threshold
    temp_mat[j+1, 1] <- ifelse(C.vec[j+1] > threshold & C.vec[j] < threshold, 1, 0)
    # Number of times dipped below threshold
    temp_mat[j+1, 2] <- ifelse(C.vec[j+1] < threshold & C.vec[j] > threshold, 1, 0)
  }

  # ECONOMIC METRICS
  Value <- ES.vec - (c.restore * R.vec * C.vec[1:years] / C.vec[1])  # Net value each year
  discount.vec <- 1 / ((1 + delta)^seq(0, (years - 1)))
  NPV <- sum(Value * discount.vec)

  # STABILITY METRICS
  BB <- sum(length(which(C.vec <= (0.25 * median(C.vec)))),
            length(which(C.vec >= (2.25 * median(C.vec))))) / length(C.vec)

  # COLLAPSE METRICS
  TP <- ifelse(C.vec[years] > A, 0, 1)  # Did coral collapse below Allee threshold?
  TPCMSY <- ifelse(C.vec[years] > (0.25 * Cmsy), 0, 1)  # Below 25% of optimal?

  # RECOVERY METRICS
  rescue <- colSums(temp_mat, na.rm = T)[1]  # Number of recoveries
  rescue_prob <- colSums(temp_mat, na.rm = T)[1] / colSums(temp_mat, na.rm = T)[2]
  dangers <- colSums(temp_mat, na.rm = T)[2]  # Number of danger events

  # MONITORING COST
  # Cost decreases exponentially as precision decreases (higher CV = lower cost)
  moncost <- sum(ci * exp(-cs * phi.CV), na.rm = T)

  return(list(
    NPV = NPV,
    ES = ES.vec,
    C = C.vec,
    Chat = Chat.vec,
    BB = BB,
    TP = TP,
    TPCMSY = TPCMSY,
    phi.CV = phi.CV,
    cost.monitor = moncost,
    pR = R.vec,
    rescue = rescue,
    rescue_prob = rescue_prob,
    dangers = dangers
  ))
}

# ============================================================================
# FUNCTION: repeat.model2
# ============================================================================
# Monte Carlo wrapper: Runs est.NPV multiple times with different random seeds
# ============================================================================

repeat.model2 <- function(n.iters, C.start, C.lim, years, K, A, r, phi.CV,
                          delta, process.noise, v, max.R, phi.seeds, process.seeds){

  # Initialize result vectors
  value <- rep(NA, n.iters)
  BB <- rep(NA, n.iters)
  TP <- rep(NA, n.iters)
  TPCMSY <- rep(NA, n.iters)
  dC <- rep(NA, n.iters)
  C <- rep(NA, n.iters)
  ES <- rep(NA, n.iters)
  phi.CV <- rep(NA, n.iters)
  cost.monitor <- rep(NA, n.iters)
  NPV_minusCM <- rep(NA, n.iters)
  pRmax <- rep(NA, n.iters)

  phi.CV.seed.save <- rep(NA, n.iters)
  thresh2 <- thresh1 <- rep(NA, n.iters)
  rescue <- rep(NA, n.iters)
  rescue_prob <- rep(NA, n.iters)
  dangers <- rep(NA, n.iters)

  # Run simulation n.iters times
  for (i in 1:n.iters){

    phi.CV.seed <- phi.seeds[i]
    process.noise.seed <- process.seeds[i]
    phi.CV.seed.save[i] <- phi.CV.seed

    # Run single simulation
    model.output <- est.NPV(years, K, A, r, phi.CV.low, phi.CV.high, delta,
                            process.noise, v, C.start, C.lim, C.crit, max.R,
                            phi.CV.seed, process.noise.seed, c.restore)

    # Extract results
    value[i] <- model.output$NPV
    BB[i] <- model.output$BB
    TP[i] <- model.output$TP
    TPCMSY[i] <- model.output$TPCMSY
    dC[i] <- median(abs(model.output$C / model.output$Chat))
    C[i] <- mean(model.output$C)

    thresh1[i] <- coral_dangerzone(C.vec = model.output$C, A = A, thresh = 0.8 * Cmsy)
    thresh2[i] <- coral_dangerzone(C.vec = model.output$C, A = A, thresh = 0.8 * Cmsy)
    rescue[i] <- length(which(model.output$C < 0.8 * Cmsy & model.output$C > A))
    rescue_prob[i] <- model.output$rescue_prob
    dangers[i] <- length(which(model.output$C > 0.8 * Cmsy))

    ES[i] <- median(model.output$ES)
    phi.CV[i] <- mean(model.output$phi.CV, na.rm = T)
    cost.monitor[i] <- model.output$cost.monitor
    NPV_minusCM[i] <- model.output$NPV - model.output$cost.monitor
    pRmax[i] <- max(model.output$pR)
  }

  return(list(
    value = value,
    BB = BB,
    TP = TP,
    TPCMSY = TPCMSY,
    dC = dC,
    C = C,
    ES = ES,
    phi.CV = phi.CV,
    cost.monitor = cost.monitor,
    NPV_minusCM = NPV_minusCM,
    pRmax = pRmax,
    thresh1 = thresh1,
    thresh2 = thresh2,
    rescue = rescue,
    rescue_prob = rescue_prob,
    dangers = dangers
  ))
}

# ============================================================================
# TEST: Single run of est.NPV
# ============================================================================

source("code/2_coral_parameters.R")

# Test with different restoration intensities
mR0.5 <- max.R * 0.5
mR1.3 <- max.R * 1.3
mR1.9 <- max.R * 1.9
phi.CV.low <- phi.CV.high <- 0.5
A <- 15
C.start <- 75
years <- 50

test_run <- est.NPV(years, K.coral, A, r.coral, phi.CV.low, phi.CV.high,
                    delta, process.noise, v, C.start, C.lim, C.crit,
                    max.R = mR1.9, phi.CV.seed, process.noise.seed, c.restore)

print(test_run)

# Plot monitoring precision over time
plot(test_run$phi.CV[-1], main = "Monitoring Precision Over Time",
     ylab = "CV", xlab = "Year")

# Plot true vs. estimated coral cover
plot(test_run$C, type = "l", xlim = c(0, 51), ylim = c(0, 150),
     main = "Coral Cover: True vs. Estimated",
     xlab = "Year", ylab = "Coral Cover (%)", lwd = 2)
lines(test_run$Chat, col = 3, lty = 2, lwd = 2)
abline(h = A, col = 'red', lwd = 2)  # Allee threshold
threshold <- 0.8 * Cmsy
abline(h = threshold, col = 'red', lty = 2, lwd = 1)  # Danger threshold
legend("topright",
       legend = c("True cover", "Estimated cover", "Allee threshold", "Danger zone"),
       col = c(1, 3, "red", "red"),
       lty = c(1, 2, 1, 2),
       lwd = c(2, 2, 2, 1))

print(paste("Rescue probability:", round(test_run$rescue_prob, 3)))

# ============================================================================
# TEST: Monte Carlo simulation with repeat.model2
# ============================================================================

n.iters <- 100
phi.seeds <- round(1000000 * runif(n.iters), 0)
process.seeds <- round(1000000 * runif(n.iters), 0)

A <- 15
Cmsy <- A/3 + K.coral/3 + (A^2 - A*K.coral + K.coral^2)^(1/2)/3
C.lim <- max(A, 0.25 * Cmsy)
MGR <- r.coral * Cmsy * (1 - Cmsy/K.coral) * (Cmsy/K.coral - A/K.coral)
Rmsy <- MGR / Cmsy

mR0.5 <- Rmsy * 0.5
mR1.3 <- Rmsy * 1.3
mR1.5 <- Rmsy * 1.5
mR2.0 <- Rmsy * 2.0
phi.CV.low <- phi.CV.high <- 0.5

# Run Monte Carlo simulation
mc_results <- repeat.model2(n.iters, C.start = 70, C.lim, years, K.coral, A,
                             r.coral, phi.CV, delta = 0.05, process.noise, v,
                             max.R = mR0.5, phi.seeds, process.seeds)

# Summarize results
return.value <- median(mc_results$value)
return.BB <- median(mc_results$BB)
return.TP <- sum(mc_results$TP) / n.iters
return.TPMGMT <- sum(mc_results$TPCMSY) / n.iters
return.dC <- mc_results$dC
return.C <- mc_results$C
return.cm <- mc_results$cost.monitor

print(paste("Median NPV:", round(return.value, 2)))
print(paste("Probability of collapse:", round(return.TP, 3)))
print(paste("Mean rescue probability:", round(mean(mc_results$rescue_prob, na.rm = T), 3)))

# ============================================================================
# KEY INSIGHTS FOR VALUE OF INFORMATION:
# ============================================================================
# Compare scenarios:
#   1. High-precision monitoring (CV = 0.1) → High cost, accurate decisions
#   2. Low-precision monitoring (CV = 0.5) → Low cost, less accurate decisions
#
# Value of Information = NPV(high precision) - NPV(low precision) - Monitoring cost
#
# When is precise monitoring worth it?
#   - When near Allee threshold (high risk of collapse)
#   - When restoration resources are limited
#   - When coral dynamics are uncertain
#
# When should we skip monitoring and just restore?
#   - When coral survival is well-known and predictable
#   - When far from danger zone
#   - When restoration is cheap relative to monitoring
# ============================================================================
