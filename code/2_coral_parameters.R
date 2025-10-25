# ============================================================================
# CORAL RESTORATION MONITORING MODEL - PARAMETERS
# ============================================================================
# This model explores the value of information trade-off in coral restoration:
# Should we invest in precise monitoring, or put those resources into restoration action?
#
# Key question: If we know coral survival rates reasonably well, is intensive
# monitoring worth the cost, or should we just do more restoration?
# ============================================================================

# MODEL DURATION AND ECONOMICS
years <- 50              # Time horizon for simulation (years)
C.start <- 75            # Starting coral cover (%)
delta <- 0.05            # Discount rate (5% annually)
process.noise <- 0       # Temporally uncorrelated variance in growth rate

# ECONOMIC PARAMETERS
v <- 100                 # Value per unit coral cover (ecosystem services per year)
                         # (e.g., tourism, fisheries productivity, coastal protection)
c.restore <- 500         # Cost per unit restoration effort
                         # (transplanting, coral gardening, reef restoration)

# MONITORING COST PARAMETERS
# Cost follows exponential decay: higher precision = higher cost
# Cost(CV) = ci * exp(-cs * CV)
ci <- 100                # Intercept of monitoring cost function
cs <- 5                  # Slope of monitoring cost decay

# CORAL POPULATION PARAMETERS
# Using Allee effect model where coral cover below threshold leads to collapse
# (due to reduced fertilization success, increased predation, etc.)

A.coral <- 20            # Allee effect threshold (% coral cover)
                         # Below this, coral cannot self-sustain

Cmsy <- 60               # Coral cover at maximum net growth (%)
                         # Optimal target for restoration

MGR <- 15                # Maximum net growth rate (% per year)
                         # Maximum increase in coral cover

K.coral <- (3*Cmsy^2 - 2*A.coral*Cmsy)/(2*Cmsy - A.coral)  # Carrying capacity (%)
r.coral <- MGR/(Cmsy*(1-Cmsy/K.coral)*(Cmsy/K.coral - A.coral/K.coral))  # Population growth rate

# RESTORATION PARAMETERS
Rmsy <- MGR/Cmsy         # Restoration effort that produces maximum net growth
max.R <- Rmsy            # Maximum restoration effort
C.lim <- Cmsy            # Lower coral cover limit for restoration control rule
C.crit <- 40             # Critical threshold for adaptive monitoring
                         # Below this, switch to high-precision monitoring

# MONITORING PRECISION PARAMETERS
phi.CV.low <- 0.1        # High precision monitoring (10% CV) - expensive
phi.CV.high <- 0.5       # Low precision monitoring (50% CV) - cheap

# The VALUE OF INFORMATION question:
# How much is the difference between 10% and 50% monitoring precision worth
# in terms of restoration outcomes and ecosystem service value?

# RANDOM SEEDS FOR REPRODUCIBILITY
rm(.Random.seed)
phi.CV.seed <- round(100000*runif(1), 0)
process.noise.seed <- round(100000*runif(1), 0)

# ============================================================================
# INTERPRETATION NOTES:
# ============================================================================
# - If you know coral survival is ~Rmsy with reasonable certainty, you might
#   be better off spending money on more restoration rather than precise monitoring
# - But if uncertainty is high and you're near the Allee threshold, monitoring
#   precision could be critical to avoid collapse
# - This model quantifies when monitoring ROI is positive vs. negative
# ============================================================================
