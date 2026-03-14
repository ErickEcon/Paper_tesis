x <- 1:10
# rm(list=ls())

library(readxl)
library(quantmod)
library(rugarch)
library(rmgarch)
library(tseries)
library(ggplot2)
library(tidyr)
library(MTS)
library(zoo)
library(tidyquant)
library(MSGARCH)

library(help = MSGARCH)


Ret <- read_excel("Retornos.xlsx")


Retornos <- Ret[, -1] * 100


multi_data <- Retornos[c("BVL_r", "TC_Peru_r", "SP500_r", "VIX_r")]


spec_alt <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 12), include.mean = FALSE),
    distribution.model = "sged",
    fixed.pars = list(
        ma1 = 0,
        ma2 = 0,
        ma3 = 0,
        ma4 = 0,
        ma5 = 0,
        ma6 = 0,
        ma7 = 0,
        ma8 = 0,
        ma9 = 0,
        ma10 = 0,
        ma11 = 0
    )
)
spec_ot1 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "sstd"
)
spec_ot2 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
    distribution.model = "nig",
    fixed.pars = list(
        ma2 = 0,
        ma3 = 0,
        ma4 = 0,
        ar2 = 0,
        ar3 = 0,
        ar4 = 0
    )
)
spec_ot3 <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 5), include.mean = FALSE),
    distribution.model = "ghyp",
    fixed.pars = list(
        ma2 = 0,
        ma3 = 0,
        ma4 = 0
    )
)

uspec.n <- multispec(c(spec_alt, spec_ot1, spec_ot2, spec_ot3))


multf <- multifit(uspec.n, multi_data)
multf

spec1 <- dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = "mvt")
fit1 <- dccfit(spec1, data = multi_data, fit.control = list(eval.se = TRUE), fit = multf)
fit1

help(CreateSpec)

# --- MSGARCH Specifications (2-regime MS-GARCH) ---

# Define 2-state Markov Switching GARCH models for each series
# Corrected: arguments must be variance.spec and distribution.spec (lists)
msgarch_spec_BVL <- CreateSpec(
    variance.spec = list(model = c("sGARCH", "sGARCH")),
    distribution.spec = list(distribution = c("sged", "sged"))
)

msgarch_spec_TC <- CreateSpec(
    variance.spec = list(model = c("sGARCH", "sGARCH")),
    distribution.spec = list(distribution = c("sstd", "sstd"))
)

msgarch_spec_SP500 <- CreateSpec(
    variance.spec = list(model = c("sGARCH", "sGARCH")),
    distribution.spec = list(distribution = c("sstd", "sstd"))
)

msgarch_spec_VIX <- CreateSpec(
    variance.spec = list(model = c("sGARCH", "sGARCH")),
    distribution.spec = list(distribution = c("sstd", "sstd"))
)

# --- Fitting MSGARCH Models ---
fit_msgarch_BVL <- FitML(spec = msgarch_spec_BVL, data = multi_data$BVL_r)
fit_msgarch_TC <- FitML(spec = msgarch_spec_TC, data = multi_data$TC_Peru_r)
fit_msgarch_SP500 <- FitML(spec = msgarch_spec_SP500, data = multi_data$SP500_r)
fit_msgarch_VIX <- FitML(spec = msgarch_spec_VIX, data = multi_data$VIX_r)

# --- MS-DCC-GARCH Incorporation ---
# Step 1: Extract Conditional Volatility from MS-GARCH fits
vol_BVL <- Volatility(fit_msgarch_BVL)
vol_TC <- Volatility(fit_msgarch_TC)
vol_SP500 <- Volatility(fit_msgarch_SP500)
vol_VIX <- Volatility(fit_msgarch_VIX)

# Step 2: Calculate Standardized Residuals
# These are the returns divided by the MS-regime-aware volatility
std_res_BVL <- multi_data$BVL_r / vol_BVL
std_res_TC <- multi_data$TC_Peru_r / vol_TC
std_res_SP500 <- multi_data$SP500_r / vol_SP500
std_res_VIX <- multi_data$VIX_r / vol_VIX

# Combine into a matrix for multivariate correlation analysis
std_res_matrix <- cbind(std_res_BVL, std_res_TC, std_res_SP500, std_res_VIX)
colnames(std_res_matrix) <- colnames(multi_data)

# Step 3: Estimate DCC using the MTS package
# This fits the dynamic correlation structure on the MS-standardized residuals
ms_dcc_fit <- dccFit(std_res_matrix, type = "Engle")

# Step 4: Extract and Visualize Dynamic Correlations
# Step 4: Extract and Visualize Dynamic Correlations
# In MTS::dccFit, rho is a matrix [T, N^2] where N is the number of assets.
# For 4 assets, the indices for (i, j) are (j-1)*4 + i.
# 1:BVL, 2:TC, 3:SP500, 4:VIX
cor_BVL_SP500 <- ms_dcc_fit$rho[, 9] # (1,3) -> (3-1)*4 + 1 = 9
cor_TC_VIX <- ms_dcc_fit$rho[, 14] # (2,4) -> (4-1)*4 + 2 = 14

# Visualization
time_index <- index(Ret$Fecha)
plot_data <- data.frame(
    Date = time_index[seq_along(cor_BVL_SP500)],
    BVL_SP500 = cor_BVL_SP500,
    TC_VIX = cor_TC_VIX
)

# --- Visualization of Regime Changes and Correlations ---
# Extract Smoothed Probabilities of being in State 2 (typically high volatility)
prob_BVL <- as.vector(State(fit_msgarch_BVL)$SmoothProb[, 1, 2, drop = TRUE])
prob_SP500 <- as.vector(State(fit_msgarch_SP500)$SmoothProb[, 1, 2, drop = TRUE])

# Ensure all series have the same length for plotting
# Sometimes DCC or MSGARCH might have slight differences depending on initialization
n_obs <- min(length(prob_BVL), length(cor_BVL_SP500))
dates_plot <- tail(time_index, n_obs)
p_BVL_plot <- tail(prob_BVL, n_obs)
p_SP500_plot <- tail(prob_SP500, n_obs)
cor_BVL_plot <- tail(cor_BVL_SP500, n_obs)
cor_VIX_plot <- tail(cor_TC_VIX, n_obs)

# Configure multi-panel plot (2 rows, 1 column)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2))

# Panel 1: Regime Probabilities (State 2: High Volatility)
plot(dates_plot, p_BVL_plot,
    type = "l", col = "blue",
    main = "Regime Probabilities (State 2: High Volatility)",
    xlab = "", ylab = "Probability", ylim = c(0, 1)
)
lines(dates_plot, p_SP500_plot, col = "darkgreen")
legend("topright",
    legend = c("BVL", "SP500"),
    col = c("blue", "darkgreen"), lty = 1, bty = "n", cex = 0.8
)

# Panel 2: Dynamic Correlations
plot(dates_plot, cor_BVL_plot,
    type = "l", col = "blue",
    main = "Dynamic Correlations (from MS-DCC-GARCH)",
    xlab = "Year", ylab = "Correlation",
    ylim = c(
        min(c(cor_BVL_plot, cor_VIX_plot)),
        max(c(cor_BVL_plot, cor_VIX_plot))
    )
)
lines(dates_plot, cor_VIX_plot, col = "red")
legend("bottomleft",
    legend = c("BVL - SP500", "TC Peru - VIX"),
    col = c("blue", "red"), lty = 1, bty = "n", cex = 0.8
)

# Reset plot configuration
par(mfrow = c(1, 1))

# Summary of MS-GARCH Marginals
summary(fit_msgarch_BVL)
summary(fit_msgarch_TC)
summary(fit_msgarch_SP500)
summary(fit_msgarch_VIX)
