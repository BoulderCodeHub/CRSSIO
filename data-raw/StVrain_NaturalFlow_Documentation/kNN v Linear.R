library(FNN)
library(dplyr)
library(ggplot2)

rm(list=ls()); gc()

df_sv=read.csv(file.path('outdata', 'Gaged and Natural Flow - St Vrain Lyons.csv'))[,-1]

df_crb=CoRiverNF::cyAnnTot

# convert to dataframe with Year column
df_crb=data.frame(
  
  Year=floor(as.numeric(zoo::index(df_crb))),
  
  zoo::coredata(df_crb)
  
)

df_complete=df_crb %>% left_join(df_sv %>% select(Year, Natural)%>%rename(StVrain=Natural), by='Year')

df_complete=df_complete %>% filter(!is.na(StVrain))

data <- df_complete %>% select(GlenwoodSprings, StVrain) %>% na.omit()
n <- nrow(data)

# ---------------------------------------------------------------
# model parameters
# ---------------------------------------------------------------

set.seed(123)  # fixes the random fold assignments so results are reproducible

n_repeats <- 100  # how many times we redo the entire 10-fold CV process,
# each time with a NEW random assignment of points to folds
n_folds <- 10      # standard 10-fold CV within each repeat

# Range of k values to search over when tuning kNN.
# We don't just pick one k in advance -- we let each training fold decide
# its own best k, so the model selection process is fair and doesn't leak
# information from the test data.
k_range <- 1:20

# ---------------------------------------------------------------
# Helper function: tune_knn_k()
#
# Given ONLY a training set (never touches the test set), this finds the
# best k for kNN using its own INNER 5-fold cross-validation.
#
# Why do this instead of just picking k=5 or k=10 by hand?
# Because the "right" k can matter a lot for kNN, and choosing it using
# the same data you'll test on later would bias your results to look
# better than they really are (a form of data leakage).
# ---------------------------------------------------------------

tune_knn_k <- function(train_data, k_range, inner_folds = 5) {
  n_train <- nrow(train_data)
  
  # Randomly assign each training row to one of 5 "inner" folds
  inner_fold_id <- sample(rep(1:inner_folds, length.out = n_train))
  
  # For each candidate k, estimate its performance by averaging RMSE
  # across the 5 inner folds
  avg_rmse_by_k <- sapply(k_range, function(k) {
    
    fold_rmses <- sapply(1:inner_folds, function(f) {
      inner_train <- train_data[inner_fold_id != f, ]
      inner_test  <- train_data[inner_fold_id == f, ]
      
      # Standardize Glenwood flow using ONLY inner_train's mean/sd,
      # then apply that same scaling to inner_test.
      # (You always scale based on training data only, never test data,
      # to avoid leaking test set information into the scaling itself.)
      X_train <- scale(inner_train$GlenwoodSprings)
      center_ <- attr(X_train, "scaled:center")
      scale_  <- attr(X_train, "scaled:scale")
      X_test  <- (inner_test$GlenwoodSprings - center_) / scale_
      
      # Find the k nearest training points for each inner test point,
      # then predict StVrain as a DISTANCE-WEIGHTED average of those
      # neighbors' StVrain values (see weighted_knn_predict() below)
      nn <- get.knnx(matrix(X_train, ncol = 1), matrix(X_test, ncol = 1), k = k)
      preds <- weighted_knn_predict(nn, inner_train$StVrain)
      
      sqrt(mean((inner_test$StVrain - preds)^2))  # RMSE for this inner fold
    })
    
    mean(fold_rmses)  # average RMSE for this k, across the 5 inner folds
  })
  
  # Return whichever k had the lowest average inner-CV RMSE
  k_range[which.min(avg_rmse_by_k)]
}

# ---------------------------------------------------------------
# Helper function: weighted_knn_predict()
#
# Standard kNN predicts using a SIMPLE mean of the k nearest neighbors'
# y-values -- every neighbor counts equally, whether it's very close to
# the test point or barely inside the k-th spot.
#
# Inverse-distance weighting instead gives closer neighbors more say:
# weight_i = 1 / (distance_i + epsilon)
# prediction = sum(weight_i * y_i) / sum(weight_i)
#
# The small epsilon (1e-6) prevents a divide-by-zero error in the rare
# case where a test point's Glenwood flow exactly matches a training
# point's flow (distance = 0), which would otherwise give that neighbor
# infinite weight.
#
# This tends to help especially at the edges of the data range, where
# the single nearest neighbor is often much closer than the k-1 other,
# more distant neighbors -- a simple mean would dilute that close
# neighbor's information unnecessarily.
# ---------------------------------------------------------------

weighted_knn_predict <- function(nn_result, train_y, epsilon = 1e-6) {
  n_test <- nrow(nn_result$nn.index)
  
  sapply(1:n_test, function(i) {
    idx <- nn_result$nn.index[i, ]       # indices of the k nearest neighbors
    dist <- nn_result$nn.dist[i, ]       # their distances to this test point
    weights <- 1 / (dist + epsilon)      # closer neighbors get higher weight
    sum(weights * train_y[idx]) / sum(weights)  # weighted average prediction
  })
}

# ---------------------------------------------------------------
# MAIN LOOP: repeated 10-fold cross-validation
#
# Structure:
#   for each of 100 repeats:
#     randomly split the 115 years into 10 folds
#     for each of the 10 folds:
#       train all 4 models on the other 9 folds
#       test all 4 models on the held-out fold
#       record RMSE for each model on that fold
#
# This produces 100 x 10 = 1,000 total RMSE values per model, which we'll
# aggregate afterward.
# ---------------------------------------------------------------

results <- data.frame(
  repeat_num = integer(),
  fold = integer(),
  model = character(),
  rmse = numeric()
)

# Separate storage for raw actual/predicted values (needed to compute
# R2 correctly -- see note below on why we can't just average per-fold R2)
raw_predictions <- data.frame()

for (r in 1:n_repeats) {
  
  # Randomly assign each of the 115 years to one of 10 folds.
  # This assignment is DIFFERENT every repeat (no fixed seed inside the loop),
  # which is exactly what lets repeated CV average out "lucky/unlucky" splits.
  fold_id <- sample(rep(1:n_folds, length.out = n))
  
  for (f in 1:n_folds) {
    
    # Training data = every fold EXCEPT f. Test data = fold f only.
    train <- data[fold_id != f, ]
    test  <- data[fold_id == f, ]
    
    # --- MODEL 1: Linear regression (real space) ---
    # Refit from scratch on this fold's training data only
    fit_lm <- lm(StVrain ~ GlenwoodSprings, data = train)
    pred_lm <- predict(fit_lm, newdata = test)
    rmse_lm <- sqrt(mean((test$StVrain - pred_lm)^2))
    
    # --- MODEL 2: kNN regression (inverse-distance weighted) ---
    # First, pick the best k using ONLY this fold's training data
    # (via the inner-CV helper function above, which also uses
    # distance-weighted predictions internally now)
    best_k <- tune_knn_k(train, k_range)
    
    # Standardize using training data's mean/sd, apply same scaling to test
    X_train <- scale(train$GlenwoodSprings)
    center_ <- attr(X_train, "scaled:center")
    scale_  <- attr(X_train, "scaled:scale")
    X_test  <- (test$GlenwoodSprings - center_) / scale_
    
    # For each test point, find its k nearest neighbors in the training set,
    # then predict StVrain as a DISTANCE-WEIGHTED average -- closer
    # neighbors count more, which should help especially at the edges of
    # the flow range where the nearest neighbor is often much closer than
    # the others
    nn <- get.knnx(matrix(X_train, ncol = 1), matrix(X_test, ncol = 1), k = best_k)
    pred_knn <- weighted_knn_predict(nn, train$StVrain)
    rmse_knn <- sqrt(mean((test$StVrain - pred_knn)^2))
    
    # --- MODEL 3: LOESS (local regression) ---
    # span = 0.75 is R's default -- controls how much of the data each local
    # fit uses (larger span = smoother, more global; smaller = wigglier, more local)
    fit_loess <- tryCatch(
      loess(StVrain ~ GlenwoodSprings, data = train, span = 0.75),
      error = function(e) NULL
    )
    if (!is.null(fit_loess)) {
      # LOESS can fail to predict (returns NA) for test points that fall
      # outside the range where it has enough local training data nearby --
      # this is itself a symptom of LOESS's own difficulty extrapolating
      pred_loess <- tryCatch(
        predict(fit_loess, newdata = test),
        error = function(e) rep(NA, nrow(test))
      )
    } else {
      pred_loess <- rep(NA, nrow(test))
    }
    valid <- !is.na(pred_loess)  # only score points where LOESS actually predicted something
    rmse_loess <- if (sum(valid) > 0) {
      sqrt(mean((test$StVrain[valid] - pred_loess[valid])^2))
    } else {
      NA
    }
    
    # --- MODEL 4: Smoothing spline ---
    # cv = FALSE tells smooth.spline to choose its smoothness parameter via
    # GCV (generalized cross-validation) automatically, using only the
    # training data passed in -- no manual tuning or leakage
    fit_spline <- tryCatch(
      smooth.spline(train$GlenwoodSprings, train$StVrain, cv = FALSE),
      error = function(e) NULL
    )
    if (!is.null(fit_spline)) {
      pred_spline <- predict(fit_spline, x = test$GlenwoodSprings)$y
      rmse_spline <- sqrt(mean((test$StVrain - pred_spline)^2))
    } else {
      rmse_spline <- NA
    }
    
    # Record this fold's RMSE for all 4 models
    results <- bind_rows(results, data.frame(
      repeat_num = r, fold = f,
      model = c("Linear", "kNN", "LOESS", "Smoothing Spline"),
      rmse = c(rmse_lm, rmse_knn, rmse_loess, rmse_spline)
    ))
    
    # Also store raw actual/predicted pairs (not just fold-level RMSE),
    # so we can compute R2 correctly across the FULL set of repeat-level
    # out-of-sample predictions rather than trying to average R2 values
    # directly (averaging R2 across folds is not mathematically valid,
    # since R2 is a ratio of sums, not a simple additive quantity)
    raw_predictions <- bind_rows(raw_predictions, data.frame(
      repeat_num = r, fold = f,
      actual = test$StVrain,
      pred_linear = pred_lm,
      pred_knn = pred_knn,
      pred_loess = pred_loess,
      pred_spline = pred_spline
    ))
  }
}

# ---------------------------------------------------------------
# AGGREGATE STEP 1: combine the 10 per-fold RMSEs within each repeat
# into a single RMSE per repeat, per model.
#
# We do this by treating all 10 folds' squared errors as one combined
# set (equivalent to computing RMSE across the whole dataset for that
# repeat), rather than just averaging the 10 fold-level RMSEs directly --
# this is the more mathematically correct way to combine RMSEs from
# folds of slightly different sizes.
# ---------------------------------------------------------------

per_repeat <- results %>%
  group_by(repeat_num, model) %>%
  summarise(rmse = sqrt(mean(rmse^2, na.rm = TRUE)), .groups = "drop")

# ---------------------------------------------------------------
# AGGREGATE STEP 2: summarize across the 100 repeats, giving one
# final mean/median/sd RMSE per model -- this is your headline result
# ---------------------------------------------------------------

summary_table <- per_repeat %>%
  group_by(model) %>%
  summarise(
    mean_RMSE = mean(rmse, na.rm = TRUE),
    sd_RMSE   = sd(rmse, na.rm = TRUE),
    median_RMSE = median(rmse, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_RMSE)

cat("=== Summary across 100 repeats of 10-fold CV ===\n")
print(summary_table)

# ---------------------------------------------------------------
# R-SQUARED DIAGNOSTICS (out-of-sample / predictive R2)
#
# IMPORTANT NOTE ON METHODOLOGY:
# R2 is NOT something you can validly average across folds the way you
# can with RMSE. R2 = 1 - (SS_residual / SS_total) is a RATIO of two
# sums of squares -- averaging ratios computed on small subsets (each
# fold has only ~11-12 points) produces a biased, noisy estimate.
#
# The correct approach: for each of the 100 repeats, pool ALL 115
# out-of-sample predictions from that repeat's 10 folds together, and
# compute ONE R2 value using the full set. This gives you 100 (not 1000)
# R2 values -- one per repeat -- which can then be averaged validly.
#
# R2 formula used:
#   R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
#
# Note this can be NEGATIVE if a model's predictions are worse than
# simply guessing the mean of the test data every time -- this is a
# real, valid outcome for out-of-sample R2 (unlike in-sample R2, which
# is bounded at 0 by construction), and it happening would itself be an
# important, informative result.
# ---------------------------------------------------------------

compute_r2 <- function(actual, predicted) {
  valid <- !is.na(predicted) & !is.na(actual)
  ss_res <- sum((actual[valid] - predicted[valid])^2)
  ss_tot <- sum((actual[valid] - mean(actual[valid]))^2)
  1 - ss_res / ss_tot
}

# Compute one pooled R2 per repeat, per model
r2_per_repeat <- raw_predictions %>%
  group_by(repeat_num) %>%
  summarise(
    Linear = compute_r2(actual, pred_linear),
    kNN = compute_r2(actual, pred_knn),
    LOESS = compute_r2(actual, pred_loess),
    `Smoothing Spline` = compute_r2(actual, pred_spline),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = -repeat_num, names_to = "model", values_to = "r2")

r2_summary <- r2_per_repeat %>%
  group_by(model) %>%
  summarise(
    mean_R2 = mean(r2, na.rm = TRUE),
    sd_R2   = sd(r2, na.rm = TRUE),
    median_R2 = median(r2, na.rm = TRUE),
    pct_negative = mean(r2 < 0, na.rm = TRUE) * 100,  # % of repeats where model did WORSE than guessing the mean
    .groups = "drop"
  ) %>%
  arrange(desc(mean_R2))

cat("\n=== Out-of-sample R2 across 100 repeats of 10-fold CV ===\n")
print(r2_summary)
cat("\nNote: 'pct_negative' shows what fraction of the 100 repeats produced\n")
cat("a NEGATIVE R2 for that model -- meaning it did worse than simply\n")
cat("predicting the mean of the test set. This is a meaningful red flag\n")
cat("if it happens often, and is a real possibility for out-of-sample R2\n")
cat("(unlike in-sample R2, which can never go below 0).\n\n")

# Boxplot of R2 across repeats, for visual comparison alongside the RMSE boxplot
ggplot(r2_per_repeat, aes(x = model, y = r2, fill = model)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of out-of-sample R2 across 100 repeats of 10-fold CV",
       subtitle = "Dashed line = R2 of 0 (performance equal to guessing the mean)",
       x = NULL, y = "R2") +
  theme_minimal() +
  theme(legend.position = "none")

# ---------------------------------------------------------------
# VISUALIZE: one boxplot per model, showing the spread of RMSE
# across the 100 repeats (each repeat = one dot)
# ---------------------------------------------------------------

ggplot(per_repeat, aes(x = model, y = rmse, fill = model)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.8) +
  labs(title = "Distribution of CV RMSE across 100 repeats of 10-fold CV",
       subtitle = "Each point = RMSE from one full 10-fold CV repeat",
       x = NULL, y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none")

# ---------------------------------------------------------------
# PAIRED COMPARISON
#
# Because every repeat used the SAME random fold assignment across all
# 4 models (we didn't re-randomize folds separately per model), we can
# treat each repeat as a "paired" observation -- like comparing the same
# 100 exam scores under two different study methods, rather than two
# unrelated groups of 100 people. Paired tests are more powerful (better
# at detecting real differences) than unpaired tests when this pairing
# structure is valid, because they cancel out repeat-to-repeat variability
# that affects all models equally within a given repeat.
# ---------------------------------------------------------------

wide_results <- per_repeat %>%
  tidyr::pivot_wider(names_from = model, values_from = rmse)

cat("\n=== Paired t-tests (each repeat is a paired observation) ===\n")
cat("Linear vs kNN:\n")
print(t.test(wide_results$Linear, wide_results$kNN, paired = TRUE))

cat("\nLinear vs LOESS:\n")
print(t.test(wide_results$Linear, wide_results$LOESS, paired = TRUE))

cat("\nLinear vs Smoothing Spline:\n")
print(t.test(wide_results$Linear, wide_results$`Smoothing Spline`, paired = TRUE))

# ---------------------------------------------------------------
# DIAGNOSTIC: does kNN's error concentrate at the extremes of Glenwood flow?
#
# Why this matters: kNN predicts by averaging the StVrain values of the
# k nearest TRAINING points. If a test point has a very high or very low
# Glenwood flow (more extreme than most training points), kNN has no
# neighbors beyond it to properly represent that extremity -- it can only
# average toward whatever training points ARE nearby, which pulls the
# prediction back toward the middle of the distribution. This is a
# structural limitation: kNN cannot extrapolate beyond its training data,
# whereas linear regression can (it just extends the line).
#
# This section reruns a SINGLE pass of 10-fold CV (not repeated 100 times)
# so we can look at individual test points and their errors directly,
# rather than losing that detail in an aggregated summary.
# ---------------------------------------------------------------

set.seed(999)
fold_id_diag <- sample(rep(1:n_folds, length.out = n))

diag_results <- data.frame()

for (f in 1:n_folds) {
  train <- data[fold_id_diag != f, ]
  test  <- data[fold_id_diag == f, ]
  
  # Linear regression, refit on this fold's training data
  fit_lm <- lm(StVrain ~ GlenwoodSprings, data = train)
  pred_lm <- predict(fit_lm, newdata = test)
  
  # kNN, same tuning approach as the main loop above (inverse-distance weighted)
  best_k <- tune_knn_k(train, k_range)
  X_train <- scale(train$GlenwoodSprings)
  center_ <- attr(X_train, "scaled:center")
  scale_  <- attr(X_train, "scaled:scale")
  X_test  <- (test$GlenwoodSprings - center_) / scale_
  nn <- get.knnx(matrix(X_train, ncol = 1), matrix(X_test, ncol = 1), k = best_k)
  pred_knn <- weighted_knn_predict(nn, train$StVrain)
  
  # Store each individual test point's actual value, both models'
  # predictions, and both models' errors -- along with the Glenwood flow
  # value itself, so we can later check whether errors grow at the extremes
  diag_results <- bind_rows(diag_results, data.frame(
    GlenwoodSprings = test$GlenwoodSprings,
    StVrain_actual = test$StVrain,
    pred_linear = pred_lm,
    pred_knn = pred_knn,
    error_linear = test$StVrain - pred_lm,
    error_knn = test$StVrain - pred_knn,
    knn_k_used = best_k
  ))
}

# Reshape to "long" format so both models' errors can be plotted together,
# colored by model, on the same axes
diag_long <- diag_results %>%
  select(GlenwoodSprings, error_linear, error_knn) %>%
  tidyr::pivot_longer(cols = c(error_linear, error_knn),
                      names_to = "model", values_to = "error") %>%
  mutate(model = recode(model, error_linear = "Linear", error_knn = "kNN"),
         abs_error = abs(error))

# Plot absolute error against Glenwood flow value.
# What to look for: if kNN's smoothed error curve (blue/red line) turns
# upward at the low and high ends of the x-axis while Linear's stays flatter,
# that confirms kNN struggles specifically at the extremes (boundary effect),
# rather than being uniformly worse across the whole range.
ggplot(diag_long, aes(x = GlenwoodSprings, y = abs_error, color = model)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 1) +
  labs(title = "Absolute prediction error vs Glenwood flow (single 10-fold CV pass)",
       subtitle = "Look for error concentrating at high/low extremes, especially for kNN",
       x = "Glenwood Springs flow (test set value)", y = "Absolute error") +
  theme_minimal()

# Pull out the 5 lowest-flow and 5 highest-flow test years specifically,
# so we can directly compare both models' errors at the extremes rather
# than relying on the smoothed curve alone
extreme_years <- diag_results %>%
  arrange(GlenwoodSprings) %>%
  slice(c(1:5, (n()-4):n()))  # first 5 rows (lowest flow) + last 5 rows (highest flow)

cat("\n=== Model error at the 5 lowest and 5 highest Glenwood flow years (this CV split) ===\n")
print(extreme_years %>%
        select(GlenwoodSprings, StVrain_actual, pred_linear, pred_knn, error_linear, error_knn))

cat("\nCompare abs(error_linear) vs abs(error_knn) in these extreme rows --\n")
cat("if kNN's errors are consistently larger here, that confirms the boundary/\n")
cat("extrapolation problem as the main driver of its worse overall RMSE.\n")
