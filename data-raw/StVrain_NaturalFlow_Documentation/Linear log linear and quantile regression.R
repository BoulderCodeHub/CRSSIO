# Script to test linear, log linear, and quantile regression models


library(quantreg)   # quantile regression
library(ggplot2)
library(dplyr)

# ---- 0. prepare data -----------------------------

rm(list=ls()); gc()

SV_histNF=read.csv(file.path('outdata', 'Gaged and Natural Flow - St Vrain Lyons.csv'))[,-1]


CRB_NF=CoRiverNF::cyAnnTot

# convert to dataframe with Year column
CRB_NF=data.frame(
  
  Year=floor(as.numeric(zoo::index(CRB_NF))),
  
  zoo::coredata(CRB_NF)
  
)

df=CRB_NF %>% left_join(SV_histNF %>% select(Year, Natural)%>%rename(StVrain=Natural), by='Year')

df=df %>% filter(!is.na(StVrain))

# ---------------------------------------------------------------
# Assumes you have a data.frame `df` with columns:
#   Year, GlenwoodSprings, StVrain   (both flow columns in natural flow units)
# ---------------------------------------------------------------

# --- 1. Simple linear regression (real space) -------------------
fit_linear <- lm(StVrain ~ GlenwoodSprings, data = df)
summary(fit_linear)

# --- 2. Log-log linear regression (power law) --------------------
# StVrain = a * GlenwoodSprings^b  <=>  log(StVrain) = log(a) + b*log(GlenwoodSprings)
df_log <- df %>% mutate(log_glenwood = log(GlenwoodSprings),
                        log_stvrain = log(StVrain))

fit_loglog <- lm(log_stvrain ~ log_glenwood, data = df_log)
summary(fit_loglog)

# Slope (elasticity) with CI - key number to interpret
b_hat <- coef(fit_loglog)["log_glenwood"]
b_ci  <- confint(fit_loglog)["log_glenwood", ]
cat(sprintf("\nElasticity (log-log slope): %.3f  [95%% CI: %.3f, %.3f]\n",
            b_hat, b_ci[1], b_ci[2]))
cat("If slope > 1: St Vrain responds MORE than proportionally to Glenwood.\n")
cat("If slope < 1: St Vrain responds LESS than proportionally.\n")
cat("If slope = 1 is inside CI: simple proportionality can't be ruled out.\n\n")

# --- 3. Quantile regression (real space) --------------------------
# Fit at multiple quantiles to see if slope itself changes with flow level
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
fit_qr <- rq(StVrain ~ GlenwoodSprings, tau = taus, data = df)
summary(fit_qr)

# Extract slopes across quantiles for easy comparison
qr_slopes <- sapply(taus, function(t) coef(rq(StVrain ~ GlenwoodSprings, tau = t, data = df))["GlenwoodSprings"])
qr_summary <- data.frame(tau = taus, slope = qr_slopes)
print(qr_summary)
cat("\nIf slope increases with tau, St Vrain's response to Glenwood steepens\n")
cat("at higher flows -- consistent with the divergence you observed visually.\n\n")

# ---------------------------------------------------------------
# MODEL COMPARISON: fit statistics
# ---------------------------------------------------------------

# For fair comparison, need predictions all back in real (untransformed) space
df$pred_linear <- predict(fit_linear)

# Back-transform log-log predictions (naive back-transform; see note below)
df$pred_loglog <- exp(predict(fit_loglog))

# Duan's smearing correction for log-back-transformation bias
# (naive exp(predicted log) underestimates the mean in real space)
smear_factor <- mean(exp(residuals(fit_loglog)))
df$pred_loglog_smear <- df$pred_loglog * smear_factor

# RMSE comparison in real space
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

comparison <- data.frame(
  model = c("Linear (real space)", "Log-log (naive backtransform)", "Log-log (smearing correction)"),
  RMSE  = c(rmse(df$StVrain, df$pred_linear),
            rmse(df$StVrain, df$pred_loglog),
            rmse(df$StVrain, df$pred_loglog_smear))
)
print(comparison)

cat("\nNote: quantile regression isn't directly comparable via RMSE since it\n")
cat("targets conditional quantiles, not the conditional mean. Better to compare\n")
cat("visually (see plot) or via pinball loss if you want a formal metric.\n\n")

# ---------------------------------------------------------------
# R-SQUARED FOR EACH MODEL
# ---------------------------------------------------------------

# Standard R2 for linear (real space)
r2_linear <- summary(fit_linear)$r.squared

# R2 for log-log model -- reported in LOG space (this is what summary() gives)
r2_loglog_logspace <- summary(fit_loglog)$r.squared

# R2 for log-log model back-transformed to REAL space
# (correlation between actual StVrain and smearing-corrected predictions, squared)
r2_loglog_realspace <- cor(df$StVrain, df$pred_loglog_smear)^2

cat("=== R-squared comparison ===\n")
cat(sprintf("Linear (real space):              R2 = %.4f\n", r2_linear))
cat(sprintf("Log-log (log space, as fitted):   R2 = %.4f\n", r2_loglog_logspace))
cat(sprintf("Log-log (back-transformed, real): R2 = %.4f\n", r2_loglog_realspace))
cat("\nNote: log-log R2 in log space and real space can differ. The real-space\n")
cat("version is the fairer comparison to the linear model, since it's measured\n")
cat("on the same scale (actual StVrain flow units).\n\n")

# Pseudo-R2 for quantile regression (Koenker & Machado, 1999)
# Compares pinball loss of fitted model vs. intercept-only model at same tau
pseudo_r2_qr <- function(tau, data) {
  fit_full <- rq(StVrain ~ GlenwoodSprings, tau = tau, data = data)
  fit_null <- rq(StVrain ~ 1, tau = tau, data = data)
  
  pinball_loss <- function(resid, tau) sum(resid * (tau - (resid < 0)))
  
  V1 <- pinball_loss(residuals(fit_full), tau)
  V0 <- pinball_loss(residuals(fit_null), tau)
  
  1 - V1 / V0
}

qr_r2 <- sapply(taus, function(t) pseudo_r2_qr(t, df))
qr_summary$pseudo_R2 <- qr_r2

cat("=== Quantile regression pseudo-R2 (Koenker & Machado) ===\n")
print(qr_summary)
cat("\nInterpretation: same idea as R2 -- proportion of loss (pinball loss,\n")
cat("not squared error) explained relative to an intercept-only model, at\n")
cat("each specific quantile tau.\n\n")

# ---------------------------------------------------------------
# VISUAL COMPARISON
# ---------------------------------------------------------------

glenwood_seq <- seq(min(df$GlenwoodSprings), max(df$GlenwoodSprings), length.out = 200)
pred_df <- data.frame(GlenwoodSprings = glenwood_seq)

# Linear
pred_df$linear <- predict(fit_linear, newdata = pred_df)

# Log-log (with smearing correction)
pred_df$loglog <- exp(predict(fit_loglog, newdata = data.frame(log_glenwood = log(glenwood_seq)))) * smear_factor

# Quantile regression lines (10th, 50th, 90th for clarity)
for (t in c(0.1, 0.5, 0.9)) {
  fit_t <- rq(StVrain ~ GlenwoodSprings, tau = t, data = df)
  pred_df[[paste0("q", t*100)]] <- predict(fit_t, newdata = pred_df)
}

ggplot(df, aes(x = GlenwoodSprings, y = StVrain)) +
  geom_point(alpha = 0.5, color = "gray30") +
  geom_line(data = pred_df, aes(y = linear, color = "Linear"), linewidth = 1) +
  geom_line(data = pred_df, aes(y = loglog, color = "Log-log"), linewidth = 1) +
  geom_line(data = pred_df, aes(y = q10, color = "Quantile (10th/50th/90th)"), linetype = "dashed") +
  geom_line(data = pred_df, aes(y = q50, color = "Quantile (10th/50th/90th)")) +
  geom_line(data = pred_df, aes(y = q90, color = "Quantile (10th/50th/90th)"), linetype = "dashed") +
  scale_color_manual(name = "Model",
                     values = c("Linear" = "blue", "Log-log" = "red",
                                "Quantile (10th/50th/90th)" = "darkgreen")) +
  labs(title = "St Vrain vs Glenwood Springs: Model Comparison",
       x = "Glenwood Springs annual natural flow",
       y = "St Vrain annual natural flow") +
  theme_minimal()

# ---------------------------------------------------------------
# RESIDUAL DIAGNOSTICS (linear vs log-log)
# ---------------------------------------------------------------

par(mfrow = c(1, 2))
plot(df$GlenwoodSprings, residuals(fit_linear),
     main = "Linear model residuals", xlab = "Glenwood flow", ylab = "Residual")
abline(h = 0, col = "red", lty = 2)

plot(df_log$log_glenwood, residuals(fit_loglog),
     main = "Log-log model residuals", xlab = "log(Glenwood flow)", ylab = "Residual")
abline(h = 0, col = "red", lty = 2)
par(mfrow = c(1, 1))

cat("\nLook for: does the linear model's residual spread increase with flow\n")
cat("(fanning pattern)? That would confirm heteroscedasticity that log-log\n")
cat("or quantile regression handles better than simple linear regression.\n")

