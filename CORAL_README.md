# Coral Restoration Monitoring: Value of Information Model

A Management Strategy Evaluation (MSE) framework for quantifying the value of monitoring precision in coral reef restoration.

## Overview

**Central Question:** Should coral restoration managers invest in precise monitoring, or would those resources be better spent on restoration action?

This model explores the trade-off between monitoring precision and restoration effort. If managers have reasonable knowledge of coral survival rates and growth dynamics, intensive monitoring may not provide sufficient value to justify its cost. Instead, those resources could fund more restoration (coral transplanting, reef restoration, etc.).

The model quantifies **when monitoring Return on Investment (ROI) is positive versus negative** under different ecological and economic conditions.

## Key Concept: Value of Information

**Value of Information (VOI)** = NPV(high-precision monitoring) - NPV(low-precision monitoring) - Monitoring cost difference

### When is precise monitoring worth it?
- Near critical Allee threshold (high collapse risk)
- High uncertainty in coral dynamics
- Restoration resources are limited
- Strong penalties for management failure

### When should we skip monitoring and just restore?
- Coral survival rates are well-known and predictable
- System is far from danger zone (healthy reefs)
- Restoration is cheap relative to monitoring costs
- Monitoring provides little decision-relevant information

## Model Structure

### Three-Component MSE Framework

```
┌─────────────────────────────────────────────────────────────┐
│                    OPERATING MODEL                          │
│  True coral population dynamics (unobserved by manager)     │
│                                                             │
│  dC/dt = r·C·(1 - C/K)·(C/K - A/K) + R                    │
│                                                             │
│  - Logistic growth with Allee effect                        │
│  - Below threshold A: coral cannot self-sustain            │
│  - Restoration effort R adds coral cover                    │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│                    SAMPLING MODEL                           │
│  Monitoring with observation uncertainty                    │
│                                                             │
│  Chat = C · exp(ε), where ε ~ N(0, CV²)                   │
│                                                             │
│  - High precision: CV = 10% (expensive)                     │
│  - Low precision: CV = 50% (cheap)                          │
│  - Adaptive: switch precision based on coral health        │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│                   MANAGEMENT MODEL                          │
│  Restoration decisions based on estimated coral cover       │
│                                                             │
│  If Chat > Cmsy:  R = max.R     (intensive restoration)    │
│  If Chat ≤ Cmsy:  R = f(Chat)   (scaled restoration)       │
│  If Chat ≤ C.lim: R = 0          (stop restoration)        │
└─────────────────────────────────────────────────────────────┘
```

### Allee Effect in Coral Reefs

Coral populations exhibit **strong Allee effects**: below a critical threshold, coral cannot self-sustain due to:
- Reduced fertilization success (fewer coral colonies = less reproductive output)
- Increased predation and algal competition
- Loss of positive feedbacks (reef structure, larval settlement cues)

This creates a **critical management threshold**: monitoring becomes most valuable when coral cover approaches the Allee threshold.

## Model Parameters

### Population Dynamics
- `A.coral = 20` : Allee threshold (% coral cover below which reefs collapse)
- `Cmsy = 60` : Coral cover at maximum net growth (optimal restoration target)
- `MGR = 15` : Maximum growth rate (% per year)
- `K.coral` : Carrying capacity (calculated)
- `r.coral` : Population growth rate (calculated)

### Economics
- `v = 100` : Value per unit coral cover (ecosystem services: tourism, fisheries, coastal protection)
- `c.restore = 500` : Cost per unit restoration effort (transplanting, coral gardening)
- `delta = 0.05` : Discount rate (5% annually)

### Monitoring
- `phi.CV.low = 0.1` : High-precision monitoring (10% coefficient of variation)
- `phi.CV.high = 0.5` : Low-precision monitoring (50% CV)
- `ci = 100` : Monitoring cost intercept
- `cs = 5` : Monitoring cost slope
- **Cost function:** `Cost(CV) = ci · exp(-cs · CV)`
  - High precision (CV=0.1): expensive
  - Low precision (CV=0.5): cheap

## Repository Structure

```
coral-restoration-monitoring-voi/
├── code/
│   ├── 0_libraries.R               # Load packages and themes
│   ├── 1_coral_operating_model.R   # Population dynamics
│   ├── 2_coral_parameters.R        # Parameter definitions
│   ├── 2b_coral_dangerzone.R       # Helper: risk zone calculations
│   ├── 3_coral_mse_model.R         # CORE: MSE functions
│   ├── 4_coral_model_tests.R       # Visualization tests
│   └── ...                         # Additional simulation/visualization scripts
├── output/
│   ├── simulation/                 # Saved .Rdata from simulations
│   └── figures/                    # Generated plots
└── README.md                       # This file
```

## Quick Start

### Run a single simulation

```r
source("code/0_libraries.R")
source("code/2_coral_parameters.R")
source("code/3_coral_mse_model.R")

# Run single model with high-precision monitoring
result <- est.NPV(
  years = 50,
  K = K.coral,
  A = 20,
  r = r.coral,
  phi.CV.low = 0.1,
  phi.CV.high = 0.5,
  delta = 0.05,
  process.noise = 0,
  v = 100,
  C.start = 75,
  C.lim = Cmsy,
  C.crit = 40,
  max.R = Rmsy,
  phi.CV.seed = 12345,
  process.noise.seed = 67890,
  c.restore = 500
)

# Plot results
plot(result$C, type = "l", main = "Coral Cover Over Time")
lines(result$Chat, col = "blue", lty = 2)
```

### Run Monte Carlo simulation

```r
n.iters <- 100
phi.seeds <- round(1000000 * runif(n.iters), 0)
process.seeds <- round(1000000 * runif(n.iters), 0)

results <- repeat.model2(
  n.iters = 100,
  C.start = 75,
  C.lim = Cmsy,
  years = 50,
  K = K.coral,
  A = 20,
  r = r.coral,
  phi.CV = NA,
  delta = 0.05,
  process.noise = 0,
  v = 100,
  max.R = Rmsy,
  phi.seeds = phi.seeds,
  process.seeds = process.seeds
)

# Summarize results
median(results$value)           # Median NPV
sum(results$TP) / n.iters       # Probability of collapse
mean(results$rescue_prob, na.rm = TRUE)  # Recovery probability
```

## Key Outputs

| Metric | Definition |
|--------|-----------|
| **NPV** | Net Present Value of ecosystem services minus restoration costs |
| **Prob.Collapse** | Probability coral drops below Allee threshold |
| **Rescue Probability** | Of times dipped below 80% Cmsy, what % recovered? |
| **Danger Zone Time** | % years spent in overharvested zone (0.8·Cmsy to A) |
| **Monitoring Cost** | Total cost of monitoring over 50 years |
| **ROI** | Return on Investment = NPV(CV=0.1) / NPV(CV=0.5) |

## Research Questions to Explore

1. **When does monitoring precision matter?**
   - Compare NPV under high vs. low precision monitoring
   - How does VOI change with Allee threshold proximity?

2. **What is the optimal monitoring strategy?**
   - Adaptive monitoring (switch precision based on coral health)
   - Fixed high-precision vs. fixed low-precision
   - Cost-benefit of monitoring investment

3. **How do economic parameters affect VOI?**
   - Restoration cost vs. monitoring cost trade-offs
   - Ecosystem service value sensitivity
   - Discount rate effects

4. **What are the collapse risks?**
   - Probability of dipping below Allee threshold
   - Recovery potential after near-collapse
   - Safe operating space for coral restoration

## Connections to Fisheries Model

This coral restoration model is inspired by and structurally similar to the fisheries management Value of Information model in [stier-2022-value-of-information](https://github.com/stier-lab/stier-2022-value-of-information).

**Key parallels:**
- Fisheries biomass (B) ↔ Coral cover (C)
- Fishing mortality (F) ↔ Restoration effort (R)
- Harvest yield ↔ Ecosystem service value
- Fishing cost ↔ Restoration cost
- Monitoring precision trade-offs in both systems

**Key differences:**
- Coral restoration **adds** to population (R > 0 increases cover)
- Fishing **removes** from population (F > 0 decreases biomass)
- Coral has stronger Allee effects (critical thresholds)
- Restoration economics are investment-focused, not extraction-focused

## Citation

If you use this model, please cite:

> Stier Lab. (2025). Coral Restoration Monitoring: Value of Information Model. GitHub repository: https://github.com/[your-org]/coral-restoration-monitoring-voi

## Contact

For questions or collaborations, please open an issue on GitHub or contact the Stier Lab.

## License

[Specify license - e.g., MIT, GPL, etc.]
