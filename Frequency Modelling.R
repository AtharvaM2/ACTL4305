library(data.table)
library(glmnet)

# Execute data cleaning script
setwd("C:/Users/Ray/Documents/ACTL4305")
source("Data Cleaning and EDA.R")

# (1) Splitting Out Data ---------------------------------------------------

test_set <- copy(data[Year == 2026])
dev_pool <- copy(data[Year %in% 2023:2025])

set.seed(9)
dev_policies   <- unique(dev_pool$PolicyID)
val_policy_ids <- sample(dev_policies, size = 0.20 * length(dev_policies))

tuning_train_set    <- dev_pool[!PolicyID %in% val_policy_ids]
model_selection_set <- dev_pool[PolicyID %in% val_policy_ids]

# Rating formula
rating_formula <- ~ DriverAgeBand + VehAgeBand + VehValueBand + VehicleType + 
  RiskZone + DensityBand + AnnualKms + GaragingLocation + 
  VehicleUse + PaymentFrequency + factor(ExcessChosen) + factor(NCDLevel)

train_mat <- model.matrix(rating_formula, data = tuning_train_set)[, -1]
train_y   <- tuning_train_set$ClaimNb

val_mat   <- model.matrix(rating_formula, data = model_selection_set)[, -1]
val_y     <- model_selection_set$ClaimNb

# Grouped 10-fold mapping inside tuning_train_set for hyperparameter tuning
inner_policies <- unique(tuning_train_set$PolicyID)
inner_map      <- data.table(
  PolicyID   = inner_policies,
  inner_fold = sample(rep(1:10, length.out = length(inner_policies)))
)
inner_merged   <- merge(tuning_train_set[, .(PolicyID)], inner_map, by = "PolicyID", sort = FALSE)
inner_fold_ids <- inner_merged$inner_fold

calc_poisson_dev <- function(actual, predicted) {
  2 * sum(ifelse(actual == 0, 0, actual * log(actual / predicted)) - (actual - predicted))
}


# (2) Fit and Tune Models --------------------------------------------------

# 1. Base GLM
freq_base_model <- glm(
  ClaimNb ~ DriverAgeBand + VehAgeBand + VehValueBand + VehicleType + 
    RiskZone + DensityBand + AnnualKms + GaragingLocation + 
    VehicleUse + PaymentFrequency + factor(ExcessChosen) + factor(NCDLevel),
  family = poisson(link = "log"),
  data   = tuning_train_set
)

# 2. Stepwise AIC & BIC
freq_step_aic <- step(freq_base_model, direction = "both", trace = 0, k = 2)
freq_step_bic <- step(freq_base_model, direction = "both", trace = 0, k = log(nrow(tuning_train_set)))

# 3. Ridge GLM (alpha = 0)
freq_ridge_cv    <- cv.glmnet(train_mat, train_y, family = "poisson", alpha = 0, foldid = inner_fold_ids)
freq_ridge_model <- glmnet(train_mat, train_y, family = "poisson", alpha = 0, lambda = freq_ridge_cv$lambda.1se)

# 4. Lasso GLM (alpha = 1)
freq_lasso_cv    <- cv.glmnet(train_mat, train_y, family = "poisson", alpha = 1, foldid = inner_fold_ids)
freq_lasso_model <- glmnet(train_mat, train_y, family = "poisson", alpha = 1, lambda = freq_lasso_cv$lambda.1se)

# 5. Elastic Net GLM (alpha grid search: 0.1 to 0.9)
alpha_grid   <- seq(0.1, 0.9, by = 0.1)
cv_enet_fits <- lapply(alpha_grid, function(a) {
  cv.glmnet(train_mat, train_y, family = "poisson", alpha = a, foldid = inner_fold_ids)
})
best_alpha_idx  <- which.min(sapply(cv_enet_fits, function(fit) min(fit$cvm)))
best_alpha      <- alpha_grid[best_alpha_idx]
freq_enet_cv    <- cv_enet_fits[[best_alpha_idx]]

freq_enet_model <- glmnet(train_mat, train_y, family = "poisson", alpha = best_alpha, lambda = freq_enet_cv$lambda.1se)


# (3) Validation Set Predictions ------------------------------------------

pred_base     <- predict(freq_base_model, newdata = model_selection_set, type = "response")
pred_step_aic <- predict(freq_step_aic,   newdata = model_selection_set, type = "response")
pred_step_bic <- predict(freq_step_bic,   newdata = model_selection_set, type = "response")

pred_ridge    <- as.vector(predict(freq_ridge_model, newx = val_mat, type = "response"))
pred_lasso    <- as.vector(predict(freq_lasso_model, newx = val_mat, type = "response"))
pred_enet     <- as.vector(predict(freq_enet_model,  newx = val_mat, type = "response"))

# (4) Create Comparison Table ---------------------------------------------

comparison_table <- data.table(
  Model_Architecture  = c("Base GLM", "Stepwise AIC", "Stepwise BIC", 
                          "Ridge GLM", "Lasso GLM", sprintf("Elastic Net (alpha=%.1f)", best_alpha)),
  Parameters          = c(
    length(coef(freq_base_model)),
    length(coef(freq_step_aic)),
    length(coef(freq_step_bic)),
    sum(as.matrix(coef(freq_ridge_model)) != 0),
    sum(as.matrix(coef(freq_lasso_model)) != 0),
    sum(as.matrix(coef(freq_enet_model)) != 0)
  ),
  Validation_Deviance = round(c(
    calc_poisson_dev(val_y, pred_base),
    calc_poisson_dev(val_y, pred_step_aic),
    calc_poisson_dev(val_y, pred_step_bic),
    calc_poisson_dev(val_y, pred_ridge),
    calc_poisson_dev(val_y, pred_lasso),
    calc_poisson_dev(val_y, pred_enet)
  ), 2),
  Validation_AE_Ratio = round(c(
    sum(val_y) / sum(pred_base),
    sum(val_y) / sum(pred_step_aic),
    sum(val_y) / sum(pred_step_bic),
    sum(val_y) / sum(pred_ridge),
    sum(val_y) / sum(pred_lasso),
    sum(val_y) / sum(pred_enet)
  ), 4)
)

print(comparison_table)

# (5) View Models ---------------------------------------------------------

cat("\n--- BASE GLM SUMMARY ---\n")
print(summary(freq_base_model))

cat("\n--- STEPWISE AIC SUMMARY ---\n")
print(summary(freq_step_aic))

cat("\n--- STEPWISE BIC SUMMARY ---\n")
print(summary(freq_step_bic))

# Regularised non-zero coefficients
cat("\n--- LASSO NON-ZERO COEFFICIENTS ---\n")
lasso_active <- as.matrix(coef(freq_lasso_model))
print(lasso_active[lasso_active != 0, , drop = FALSE])

cat("\n--- ELASTIC NET NON-ZERO COEFFICIENTS ---\n")
enet_active <- as.matrix(coef(freq_enet_model))
print(enet_active[enet_active != 0, , drop = FALSE])

# Cross-validation curves for lambda selection
par(mfrow = c(1, 2))
plot(freq_lasso_cv, main = "Lasso CV: Lambda Selection")
plot(freq_ridge_cv, main = "Ridge CV: Lambda Selection")
par(mfrow = c(1, 1))

# (6) Model Validation - Residuals, Diagnostics ---------------------------

# 0. Deviance residuals for Stepwise AIC
dev_res <- residuals(freq_step_aic, type = "deviance")

par(mfrow = c(1, 2))
qqnorm(dev_res, main = "Normal Q-Q (Stepwise AIC Residuals)")
qqline(dev_res, col = "red", lwd = 2)

plot(predict(freq_step_aic, type = "link"), dev_res,
     xlab = "Linear Predictor (eta)", ylab = "Deviance Residuals",
     main = "Residuals vs Fitted (eta)", pch = 20, col = rgb(0, 0, 0, 0.2))
abline(h = 0, col = "red", lty = 2)
par(mfrow = c(1, 1))

# 1. Dispersion parameter (check if variance equals mean)
pearson_res <- residuals(freq_step_aic, type = "pearson")
phi_hat     <- sum(pearson_res^2) / df.residual(freq_step_aic)
cat(sprintf("Dispersion parameter (phi): %.4f\n", phi_hat))

# 2. Gini coefficient and Lorenz curve on validation set
val_eval <- copy(model_selection_set)
val_eval[, pred := predict(freq_step_aic, newdata = val_eval, type = "response")]
setorder(val_eval, pred)

val_eval[, `:=`(
  cum_policies = (1:.N) / .N,
  cum_claims   = cumsum(ClaimNb) / sum(ClaimNb)
)]

# Include the origin (0, 0) for exact trapezoidal area
x <- c(0, val_eval$cum_policies)
y <- c(0, val_eval$cum_claims)

# Area under the Lorenz curve (AUC)
auc <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
gini_score <- round(1 - 2 * auc, 4)

cat(sprintf("Validation Gini score: %.4f\n", gini_score))

# Plot Lorenz Curve
plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "Cumulative Policies", ylab = "Cumulative Claims",
     main = sprintf("Lorenz Curve (Gini = %.4f)", gini_score))
abline(0, 1, col = "grey50", lty = 2)

# 3. Final model assessment on 2026 test set
test_eval <- copy(test_set)
test_eval[, pred := predict(freq_step_aic, newdata = test_eval, type = "response")]

calc_poisson_dev <- function(actual, predicted) {
  2 * sum(ifelse(actual == 0, 0, actual * log(actual / predicted)) - (actual - predicted))
}

test_deviance <- calc_poisson_dev(test_eval$ClaimNb, test_eval$pred)
test_ae_ratio <- sum(test_eval$ClaimNb) / sum(test_eval$pred)

cat(sprintf("2026 Test A/E: %.4f\n", test_ae_ratio))
cat(sprintf("2026 Test Deviance: %.2f\n", test_deviance))

# (7) More Detailed Breakdown of AvE into deciles -------------------------

# 1. Generate predictions on 2026 test_set
test_eval <- copy(test_set)
test_eval[, pred := predict(freq_step_aic, newdata = test_eval, type = "response")]

# 2. Divide 2026 policies into 10 risk deciles
test_eval[, decile := cut(pred, 
                          breaks = quantile(pred, probs = seq(0, 1, 0.1)), 
                          include.lowest = TRUE, 
                          labels = 1:10)]

# 3. Aggregate observed vs predicted metrics per decile
test_decile_table <- test_eval[, .(
  Policies        = .N,
  Actual_Claims   = sum(ClaimNb),
  Expected_Claims = round(sum(pred), 1),
  Actual_Rate     = round(mean(ClaimNb), 4),
  Predicted_Rate  = round(mean(pred), 4),
  AE_Ratio        = round(sum(ClaimNb) / sum(pred), 4)
), keyby = decile]

print(test_decile_table)


#  (8) Checking Frequency Trend -------------------------------------------
# 1. Fit Raw & Mix-Adjusted Poisson GLMs
fit_raw <- glm(
  ClaimNb ~ Year + offset(log(Exposure)), 
  family = poisson(link = "log"), 
  data = data
)

fit_adj <- glm(
  ClaimNb ~ Year + RiskZone + AnnualKms + GaragingLocation + 
    VehicleUse + PaymentFrequency + factor(ExcessChosen) + 
    factor(NCDLevel) + offset(log(Exposure)), 
  family = poisson(link = "log"), 
  data = data
)

# 2. Function to Extract Key Statistics
extract_trend <- function(fit) {
  b    <- coef(fit)["Year"]
  ci   <- confint.default(fit)["Year", ]
  pval <- summary(fit)$coefficients["Year", "Pr(>|z|)"]
  
  c(
    Annual_Pct_Change = paste0(round((exp(b) - 1) * 100, 2), "%"),
    CI_95_Lower       = paste0(round((exp(ci[1]) - 1) * 100, 2), "%"),
    CI_95_Upper       = paste0(round((exp(ci[2]) - 1) * 100, 2), "%"),
    p_value           = round(pval, 4)
  )
}

# 3. View Summary Table
as.data.frame(rbind(
  Raw_Trend          = extract_trend(fit_raw),
  Mix_Adjusted_Trend = extract_trend(fit_adj)
))

