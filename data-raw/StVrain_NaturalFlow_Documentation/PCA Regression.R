library(dplyr)
library(ggplot2)
library(tidyr)

rm(list=ls()); gc()

# ---------------------------------------------------------------
# Assumes:
#   df_crb   : data.frame with Year + 29 gage columns (from your xts export)
#   df_sv    : data.frame with Year, StVrain
# Merge and check overlap first.
# ---------------------------------------------------------------

df_sv=read.csv(file.path('outdata', 'Gaged and Natural Flow - St Vrain Lyons.csv'))[,-1]

df_crb=CoRiverNF::cyAnnTot

# convert to dataframe with Year column
df_crb=data.frame(
  
  Year=floor(as.numeric(zoo::index(df_crb))),
  
  zoo::coredata(df_crb)
  
)

df_complete=df_crb %>% left_join(df_sv %>% select(Year, Natural)%>%rename(StVrain=Natural), by='Year')

df_complete=df_complete %>% filter(!is.na(StVrain))


# ---------------------------------------------------------------
# PCA (standardized)
# ---------------------------------------------------------------

gage_cols=CRSSIO::nf_gage_abbrv()

X <- df_complete[gage_cols]
pca <- prcomp(X, center = TRUE, scale. = TRUE)

# Variance explained
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
cum_var <- cumsum(var_explained)

scree_df <- data.frame(
  PC = seq_along(var_explained),
  var_explained = var_explained,
  cum_var = cum_var
)
print(scree_df, row.names = FALSE)

# Scree plot
ggplot(scree_df, aes(x = PC)) +
  geom_col(aes(y = var_explained), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = cum_var), color = "darkred", linewidth = 1) +
  geom_point(aes(y = cum_var), color = "darkred") +
  scale_y_continuous(labels = scales::percent,
                     sec.axis = sec_axis(~., labels = scales::percent, name = "Cumulative variance")) +
  labs(title = "Scree plot: variance explained by each PC",
       x = "Principal Component", y = "Variance explained") +
  theme_minimal()

# Loadings for first few PCs -- which gages drive each component
loadings <- as.data.frame(pca$rotation[, 1:5])
loadings$gage <- rownames(loadings)
loadings <- loadings %>% select(gage, everything())

cat("\n=== Top loadings on PC1 (likely 'basin-wide wet/dry' signal) ===\n")
print(loadings %>% arrange(desc(abs(PC1))) %>% select(gage, PC1) %>% head(10))

cat("\n=== Top loadings on PC2 ===\n")
print(loadings %>% arrange(desc(abs(PC2))) %>% select(gage, PC2) %>% head(10))

cat("\n=== Top loadings on PC3 ===\n")
print(loadings %>% arrange(desc(abs(PC3))) %>% select(gage, PC3) %>% head(10))

# ---------------------------------------------------------------
# PRINCIPAL COMPONENT REGRESSION (PCR)
# ---------------------------------------------------------------

pc_scores <- as.data.frame(pca$x)
pcr_data <- cbind(StVrain = df_complete$StVrain, pc_scores)

# LOOCV to choose number of components
max_pc <- min(15, ncol(pc_scores))  # cap search at 15 PCs for practicality
loocv_rmse <- numeric(max_pc)

for (k in 1:max_pc) {
  preds <- numeric(nrow(pcr_data))
  for (i in 1:nrow(pcr_data)) {
    train <- pcr_data[-i, ]
    test  <- pcr_data[i, , drop = FALSE]
    form  <- as.formula(paste("StVrain ~", paste0("PC", 1:k, collapse = " + ")))
    fit_i <- lm(form, data = train)
    preds[i] <- predict(fit_i, newdata = test)
  }
  loocv_rmse[k] <- sqrt(mean((pcr_data$StVrain - preds)^2))
}

loocv_df <- data.frame(num_PCs = 1:max_pc, LOOCV_RMSE = loocv_rmse)
print(loocv_df)

best_k_raw <- which.min(loocv_rmse)
cat(sprintf("\nBest number of PCs by raw LOOCV minimum: %d (RMSE = %.1f)\n",
            best_k_raw, loocv_rmse[best_k_raw]))

# ---------------------------------------------------------------
# 1-SE RULE (corrected): store per-fold squared errors for EVERY k,
# then compute SE of RMSE via bootstrap resampling of those errors
# (avoids the delta-method approximation, which is unreliable when
# squared errors are skewed -- as streamflow errors typically are).
# ---------------------------------------------------------------

all_sq_errors <- matrix(NA, nrow = nrow(pcr_data), ncol = max_pc)

for (k in 1:max_pc) {
  preds <- numeric(nrow(pcr_data))
  for (i in 1:nrow(pcr_data)) {
    train <- pcr_data[-i, ]
    test  <- pcr_data[i, , drop = FALSE]
    form  <- as.formula(paste("StVrain ~", paste0("PC", 1:k, collapse = " + ")))
    fit_i <- lm(form, data = train)
    preds[i] <- predict(fit_i, newdata = test)
  }
  all_sq_errors[, k] <- (pcr_data$StVrain - preds)^2
}

# Bootstrap SE of RMSE at each k: resample the 115 per-fold squared
# errors with replacement, recompute RMSE, repeat many times, take SD
set.seed(42)
n_boot <- 2000
boot_se_rmse <- numeric(max_pc)

for (k in 1:max_pc) {
  boot_rmses <- replicate(n_boot, {
    resampled <- sample(all_sq_errors[, k], replace = TRUE)
    sqrt(mean(resampled))
  })
  boot_se_rmse[k] <- sd(boot_rmses)
}

loocv_df$SE_RMSE <- boot_se_rmse
print(loocv_df)

threshold <- loocv_rmse[best_k_raw] + boot_se_rmse[best_k_raw]
best_k_1se <- min(which(loocv_rmse <= threshold))

cat(sprintf("\n1-SE threshold RMSE: %.2f (min RMSE %.2f + bootstrap SE %.2f)\n",
            threshold, loocv_rmse[best_k_raw], boot_se_rmse[best_k_raw]))
cat(sprintf("Simplest k within 1 SE of minimum: %d (RMSE = %.2f)\n",
            best_k_1se, loocv_rmse[best_k_1se]))
cat("\nRecommendation: use the 1-SE k unless there's a strong substantive\n")
cat("reason to trust the higher-dimensional model.\n\n")

best_k <- best_k_1se  # use the more conservative choice downstream

ggplot(loocv_df, aes(x = num_PCs, y = LOOCV_RMSE)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = best_k, linetype = "dashed", color = "red") +
  labs(title = "LOOCV RMSE vs number of principal components",
       x = "Number of PCs included", y = "LOOCV RMSE") +
  theme_minimal()

# ---------------------------------------------------------------
# COMPARE: PCR (best k) vs Glenwood-only linear model
# ---------------------------------------------------------------

# best_k=2 # force some other number of PCs

form_best <- as.formula(paste("StVrain ~", paste0("PC", 1:best_k, collapse = " + ")))
fit_pcr <- lm(form_best, data = pcr_data)

r2_pcr <- summary(fit_pcr)$r.squared

# Refit Glenwood-only on the SAME complete-case subset for fair comparison
fit_glenwood_samesample <- lm(StVrain ~ GlenwoodSprings, data = df_complete)
r2_glenwood_samesample <- summary(fit_glenwood_samesample)$r.squared

cat("\n=== Final comparison (same sample, complete cases only) ===\n")
cat(sprintf("Glenwood-only linear model:      R2 = %.4f\n", r2_glenwood_samesample))
cat(sprintf("PCR with %d components:          R2 = %.4f\n", best_k, r2_pcr))
cat(sprintf("LOOCV RMSE, Glenwood-only:       %.1f\n",
            sqrt(mean((df_complete$StVrain - predict(fit_glenwood_samesample))^2))))
cat(sprintf("LOOCV RMSE, PCR (%d PCs):         %.1f\n", best_k, loocv_rmse[best_k]))

cat("\nNote: compare LOOCV RMSE, not in-sample R2, for an honest comparison --\n")
cat("in-sample R2 will mechanically increase as you add PCs even if they're\n")
cat("not truly predictive out-of-sample.\n")

