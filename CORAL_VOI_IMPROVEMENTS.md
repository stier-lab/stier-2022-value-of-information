# ANALYSIS: Improving the Coral Restoration Monitoring VOI Model

## Executive Summary

After reviewing the coral restoration monitoring model, I've identified **10 critical issues** that reduce its effectiveness at quantifying the value of monitoring. Below are prioritized improvements to make the model more realistic and powerful for VOI analysis.

---

## CRITICAL ISSUES IDENTIFIED

### 🔴 **ISSUE 1: Backwards Restoration Control Rule** (HIGH PRIORITY)

**Current Code (lines 77-79 in 3_coral_mse_model.R):**
```r
if (Chat.vec[i] > Cmsy) R <- max.R          # High effort when healthy
if (Chat.vec[i] <= Cmsy) R <- Ro + b * Chat.vec[i]  # Scale with cover
if (Chat.vec[i] <= C.lim) R <- 0            # Stop if too degraded
```

**Problem:** This is backwards! It applies MAXIMUM restoration when coral is healthy (>Cmsy) and REDUCES restoration when coral is struggling (<Cmsy). This defeats the purpose of restoration.

**Fix:** Invert the logic:
```r
# Restore MORE when coral is degraded, LESS when healthy
if (Chat.vec[i] > Cmsy) R <- 0                      # No restoration needed when healthy
if (Chat.vec[i] <= Cmsy && Chat.vec[i] > C.lim) {
  R <- max.R * (Cmsy - Chat.vec[i]) / (Cmsy - C.lim)  # Scale inversely with cover
}
if (Chat.vec[i] <= C.lim) R <- max.R * 2            # Emergency restoration when critical
```

**Impact:** This single fix will dramatically change model behavior and make VOI meaningful.

---

### 🔴 **ISSUE 2: Restoration Effort Scales with ESTIMATED Cover** (HIGH PRIORITY)

**Current Code (line 93):**
```r
C.vec[i+1] <- max(0.1, C.vec[i] + C.vec[i] * production + R * Chat.vec[i])
```

**Problem:** The restoration amount is `R * Chat.vec[i]`, meaning if you OVERESTIMATE coral cover, you add MORE restoration effort. This creates perverse incentives and doesn't reflect reality.

**Fix:** Restoration should be a fixed amount based on the decision, not scaled by estimates:
```r
# Option 1: Fixed restoration amount
C.vec[i+1] <- max(0.1, C.vec[i] + C.vec[i] * production + R)

# Option 2: Scale by TRUE cover (restoration success depends on actual substrate)
C.vec[i+1] <- max(0.1, C.vec[i] + C.vec[i] * production + R * C.vec[i])

# Option 3: Restoration effectiveness depends on whether you're right
restoration_effectiveness <- ifelse(abs(Chat.vec[i] - C.vec[i]) < 10, R, R * 0.5)
C.vec[i+1] <- max(0.1, C.vec[i] + C.vec[i] * production + restoration_effectiveness)
```

**Impact:** Makes monitoring uncertainty affect restoration effectiveness, which is central to VOI.

---

### 🟡 **ISSUE 3: No Budget Trade-off Between Monitoring and Restoration** (MEDIUM PRIORITY)

**Problem:** The model calculates monitoring cost but doesn't allow managers to REALLOCATE that budget to restoration. The core VOI question is "should we spend on monitoring OR restoration?" but there's no mechanism for this trade-off.

**Fix:** Add a budget constraint:
```r
# Add to parameters:
total.budget <- 10000  # Total annual budget for monitoring + restoration

# In est.NPV function:
# Calculate monitoring cost for this year
annual_monitoring_cost <- ci * exp(-cs * phi.CV[i])

# Budget available for restoration after monitoring
budget_for_restoration <- total.budget - annual_monitoring_cost

# Restoration limited by budget
R_desired <- [calculate from control rule]
R_actual <- min(R_desired, budget_for_restoration / c.restore)

# Apply actual restoration
C.vec[i+1] <- C.vec[i] + C.vec[i] * production + R_actual
```

**Impact:** This creates a DIRECT trade-off where choosing high-precision monitoring reduces restoration capacity. This is the heart of VOI!

---

### 🟡 **ISSUE 4: Process Noise Set to Zero** (MEDIUM PRIORITY)

**Current Code:** `process.noise <- 0`

**Problem:** With no environmental stochasticity, the system is deterministic except for measurement error. This reduces the value of monitoring because the true state is predictable.

**Fix:** Add realistic process noise:
```r
process.noise <- 0.1  # 10% SD in growth rate
# Or make it a parameter to explore
```

**Impact:** Environmental variability makes monitoring more valuable because the true state is uncertain even with perfect models.

---

### 🟡 **ISSUE 5: Missing "Perfect Information" Benchmark** (MEDIUM PRIORITY)

**Problem:** To quantify VOI, you need to know the value of PERFECT information as an upper bound.

**Fix:** Add a perfect information scenario:
```r
# Run three scenarios for each parameter set:
# 1. High precision monitoring (CV = 0.1)
# 2. Low precision monitoring (CV = 0.5)
# 3. Perfect information (CV = 0.0)

VOI_current <- NPV(CV=0.1) - NPV(CV=0.5) - (Cost(CV=0.1) - Cost(CV=0.5))
VOI_potential <- NPV(CV=0.0) - NPV(CV=0.5) - (Cost(CV=0.0) - Cost(CV=0.5))

# What fraction of potential VOI are we capturing?
VOI_efficiency <- VOI_current / VOI_potential
```

**Impact:** Allows you to say "high-precision monitoring captures 73% of the value of perfect information"

---

### 🟢 **ISSUE 6: Monitoring Frequency vs. Precision Not Separated** (LOW PRIORITY)

**Problem:** The model only varies monitoring QUALITY (CV), not FREQUENCY. In reality, managers might monitor less often with low budgets, not just with lower quality.

**Fix:** Add monitoring frequency:
```r
# Parameters
monitoring.freq.high <- 1    # Monitor every year
monitoring.freq.low <- 0.25  # Monitor every 4 years

# In simulation loop
if (i %% (1/monitoring.freq) == 0) {
  # Take new measurement
  Chat.vec[i] <- C.vec[i] * C.errors[i]
} else {
  # Use previous estimate (with increasing uncertainty)
  time_since_survey <- i %% (1/monitoring.freq)
  uncertainty_inflation <- 1 + 0.1 * time_since_survey
  Chat.vec[i] <- Chat.vec[i-1] * exp(rnorm(1, 0, phi.CV * uncertainty_inflation))
}
```

**Impact:** More realistic representation of monitoring decisions.

---

### 🟢 **ISSUE 7: No Cost of Restoration Failure** (LOW PRIORITY)

**Problem:** If you apply restoration when coral is below the Allee threshold, it might fail entirely (corals die). There's no penalty for wasting restoration effort.

**Fix:** Add restoration survival that depends on coral health:
```r
# Restoration success probability
success_prob <- ifelse(C.vec[i] < A.coral, 0.2,  # 20% survival below Allee
                       ifelse(C.vec[i] < Cmsy, 0.7,  # 70% survival below optimal
                              0.95))  # 95% survival when healthy

# Apply restoration with success rate
R_effective <- R * success_prob
C.vec[i+1] <- C.vec[i] + C.vec[i] * production + R_effective
```

**Impact:** Makes monitoring MORE valuable near critical thresholds (where mistakes are costly).

---

### 🟢 **ISSUE 8: Adaptive Monitoring Threshold is Fixed** (LOW PRIORITY)

**Current:** `C.crit <- 40` (fixed)

**Fix:** Make it relative to Allee threshold:
```r
C.crit <- A.coral + 0.5 * (Cmsy - A.coral)  # Midpoint between Allee and optimal
# Or add a buffer above Allee
C.crit <- A.coral * 1.5  # Switch to high precision at 150% of Allee threshold
```

**Impact:** Adaptive monitoring strategy adjusts to different Allee threshold values.

---

### 🟢 **ISSUE 9: Economic Value is Linear** (LOW PRIORITY)

**Current:** `ES.vec[i] <- v * C.vec[i]` (linear)

**Fix:** Add thresholds or nonlinearities:
```r
# Option 1: Threshold effect (ecosystem services collapse below critical coral cover)
ES.vec[i] <- ifelse(C.vec[i] < 30, 0.1 * v * C.vec[i],  # Reduced value below threshold
                     v * C.vec[i])

# Option 2: Saturating function (diminishing returns to coral cover)
ES.vec[i] <- v * C.vec[i] / (C.vec[i] + 20)  # Michaelis-Menten

# Option 3: Tipping point in services
ES.vec[i] <- ifelse(C.vec[i] < A.coral, 0,  # No services if collapsed
                     v * C.vec[i])
```

**Impact:** Makes avoiding collapse more valuable, increasing VOI near thresholds.

---

### 🟢 **ISSUE 10: No Explicit "Cost of Being Wrong" Metric** (LOW PRIORITY)

**Problem:** VOI should explicitly quantify the cost of making wrong decisions due to poor monitoring.

**Fix:** Track decision errors:
```r
# In simulation loop, record:
# 1. When you SHOULD restore but don't (Type I error)
error_type1 <- ifelse(C.vec[i] < Cmsy & R == 0, 1, 0)

# 2. When you SHOULDN'T restore but do (Type II error)
error_type2 <- ifelse(C.vec[i] > Cmsy & R > 0, 1, 0)

# 3. Economic cost of errors
cost_of_error_type1 <- (Cmsy - C.vec[i]) * v  # Lost ecosystem services
cost_of_error_type2 <- R * c.restore  # Wasted restoration funds

# Sum up over time
total_cost_of_errors <- sum(cost_of_error_type1 + cost_of_error_type2)

# VOI = reduction in error costs
VOI <- total_cost_of_errors(low_CV) - total_cost_of_errors(high_CV)
```

**Impact:** Direct interpretation: "High-precision monitoring saves $X by avoiding costly mistakes"

---

## RECOMMENDED IMPLEMENTATION PRIORITY

### Phase 1: Critical Fixes (Do These First!)
1. Fix backwards restoration control rule (Issue 1)
2. Fix restoration scaling by estimate (Issue 2)
3. Add budget trade-off (Issue 3)

### Phase 2: VOI Enhancements
4. Add perfect information benchmark (Issue 5)
5. Increase process noise (Issue 4)
6. Add cost of being wrong metric (Issue 10)

### Phase 3: Realism & Extensions
7. Add restoration failure (Issue 7)
8. Add monitoring frequency (Issue 6)
9. Make adaptive threshold relative (Issue 8)
10. Add nonlinear ecosystem values (Issue 9)

---

## EXPECTED IMPACT

After these fixes, the model will:

✓ **Have realistic restoration decisions** (more effort when coral is degraded)
✓ **Create direct monitoring-restoration trade-offs** (budget constraint)
✓ **Quantify VOI bounds** (perfect information benchmark)
✓ **Make monitoring valuable** (through process noise and decision errors)
✓ **Measure cost of mistakes** (explicit error tracking)

This will transform the model from a theoretical exercise into a **practical decision-support tool** for coral restoration managers.

---

## QUESTIONS FOR DISCUSSION

1. **What is the realistic total budget for monitoring + restoration?** (Needed for Issue 3)
2. **How variable are coral growth rates year-to-year?** (Sets process noise level)
3. **What is the survival rate of restored corals at different health levels?** (For Issue 7)
4. **Are there threshold effects in ecosystem services?** (For Issue 9)
5. **How often would managers realistically monitor?** (For Issue 6)

---

**Next Steps:** Shall I implement Phase 1 fixes (Issues 1-3) to get the model working correctly?
