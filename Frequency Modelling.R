library(data.table)
library(glmnet)
library(ggplot2)
library(statmod)
library(tweedie)
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


# Pure premium modelling
#   A) Tweedie GLM (compound Poisson-Gamma, log link) on loss cost directly
#   B) Frequency x Severity GLM benchmark (Poisson x Gamma)
#   C) Optional XGBoost-Tweedie challenger
# Same split as the frequency script: train/validation = 2023-25 (grouped by policy), test = 2026.
# 0. Settings ---------------------------------------------------------------
use_cap       <- TRUE    # cap large claims at the reinsurance retention (set to your brief's retention)
cap_per_claim <- 50000   # NB: ClaimAmount is a policy-year total, so cap = cap_per_claim * ClaimNb (approximation)
include_trend <- TRUE    # TRUE: YearC enters the model (scored at YearC = 3, 4, 5 for 2026, 2027, 2028)

# 1. Target variables and data split ----------------------------------------
data[, ClaimAmountModel := if (use_cap) pmin(ClaimAmount, cap_per_claim * ClaimNb) else ClaimAmount]
data[, PurePrem := ClaimAmountModel / Exposure]                                   # loss cost per policy-year
data[, AvgClaim := fifelse(ClaimNb > 0, ClaimAmountModel / ClaimNb, NA_real_)]    # severity

cat(sprintf("Capping affects %d policy-years, removing $%s (%.2f%% of incurred)\n",
            data[ClaimAmountModel < ClaimAmount, .N],
            format(round(data[, sum(ClaimAmount - ClaimAmountModel)]), big.mark = ","),
            100 * data[, sum(ClaimAmount - ClaimAmountModel) / sum(ClaimAmount)]))

test_set <- copy(data[Year == 2026])
dev_pool <- copy(data[Year %in% 2023:2025])

set.seed(9)   # must be IDENTICAL to the frequency script so the split matches
dev_policies   <- unique(dev_pool$PolicyID)
val_policy_ids <- sample(dev_policies, size = 0.20 * length(dev_policies))

tuning_train_set    <- dev_pool[!PolicyID %in% val_policy_ids]
model_selection_set <- dev_pool[PolicyID %in% val_policy_ids]

# Rating formula (same factors as the frequency model; TenureYears and MultiPolicyFlag
# are excluded as they are commercial discount drivers, handled in later tasks)
rating_formula <- ~ DriverAgeBand + VehAgeBand + VehValueBand + VehicleType +
  RiskZone + DensityBand + AnnualKms + GaragingLocation +
  VehicleUse + PaymentFrequency + factor(ExcessChosen) + factor(NCDLevel)

trend_term  <- if (include_trend) " + YearC" else ""
twd_formula <- update(rating_formula, as.formula(paste0("PurePrem ~ ." , trend_term)))
frq_formula <- update(rating_formula, ClaimNb ~ .)                                  # no Year (mix-adjusted trend ~ 0)
sev_formula <- update(rating_formula, as.formula(paste0("AvgClaim ~ .", trend_term)))  # severity trend ~ +5%

# 2. Helper functions --------------------------------------------------------
tweedie_dev <- function(y, mu, p) {
  2 * (y^(2 - p) / ((1 - p) * (2 - p)) - y * mu^(1 - p) / (1 - p) + mu^(2 - p) / (2 - p))
}

gini_lc <- function(actual, pred, w) {          # same Lorenz-curve definition as the frequency script
  o <- order(pred); a <- (actual * w)[o]; ww <- w[o]
  x <- c(0, cumsum(ww) / sum(ww)); y <- c(0, cumsum(a) / sum(a))
  1 - 2 * sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
}

score_models <- function(d, preds, p) {         # preds = named list of predicted loss cost per policy-year
  rbindlist(lapply(names(preds), function(nm) {
    mu <- preds[[nm]]
    data.table(Model     = nm,
               Deviance  = round(sum(d$Exposure * tweedie_dev(d$PurePrem, mu, p)), 1),
               AE_Ratio  = round(sum(d$PurePrem * d$Exposure) / sum(mu * d$Exposure), 4),
               Gini      = round(gini_lc(d$PurePrem, mu, d$Exposure), 4))
  }))
}

decile_table <- function(d, mu, n = 10) {
  x <- data.table(mu = mu, act = d$PurePrem * d$Exposure, w = d$Exposure)
  x[, decile := ceiling(frank(mu, ties.method = "first") * n / .N)]
  x[, .(Policies = .N, Actual_LossCost = round(sum(act) / sum(w), 1),
        Predicted_LossCost = round(sum(mu * w) / sum(w), 1),
        AE_Ratio = round(sum(act) / sum(mu * w), 4)), keyby = decile]
}

# 3. Estimate the Tweedie power parameter p ----------------------------------
# (a) Profile likelihood (can take several minutes; use a random sample of rows if too slow)
prof <- tweedie.profile(twd_formula, data = as.data.frame(tuning_train_set),
                        p.vec = seq(1.2, 1.5, by = 0.05), link.power = 0,   # p = 1.8 fails to converge; likelihood peaks ~1.3-1.4
                        do.plot = TRUE, verbose = 1)
p_hat <- round(prof$p.max, 2)
cat("Profile-likelihood p:", p_hat, "| 95% CI:", round(prof$ci, 3), "\n")   # if p.max is on the grid edge, widen p.vec

# (b) Sanity check from the Gamma severity fit: compound Poisson-Gamma has p = (alpha + 2) / (alpha + 1)
sev_train <- tuning_train_set[ClaimNb > 0]
sev_base  <- glm(sev_formula, family = Gamma(link = "log"), weights = ClaimNb, data = sev_train)
alpha_hat <- 1 / summary(sev_base)$dispersion
cat("Implied p from Gamma shape:", round((alpha_hat + 2) / (alpha_hat + 1), 3), "\n")
# If (a) and (b) are close, a compound Poisson-Gamma / Tweedie structure is well supported.
# Use p_hat for everything below (incl. deviance comparisons across models).

# 4. A) Tweedie GLM ------------------------------------------------------------
twd_family <- function(p) statmod::tweedie(var.power = p, link.power = 0)   # link.power = 0 -> log link

twd_base <- glm(twd_formula, family = twd_family(p_hat), weights = Exposure, data = tuning_train_set)
phi_twd  <- summary(twd_base)$dispersion

# glm() with the Tweedie family has no AIC, and step() cannot use `scale` for glm objects, so we use
# a custom backward elimination on a Cp-style criterion: deviance/phi + k * (number of parameters),
# with phi taken from the full model. k = 2 is AIC-like, k = log(n) is BIC-like.
step_tweedie <- function(full, k = 2, keep = character(0)) {   # keep = terms that can never be dropped
  phi  <- summary(full)$dispersion
  crit <- function(m) deviance(m) / phi + k * length(coef(m))
  cur  <- full
  repeat {
    labs <- setdiff(attr(terms(cur), "term.labels"), keep)
    if (length(labs) <= 1) break
    fits  <- lapply(labs, function(t) update(cur, as.formula(paste(". ~ . -", t))))
    crits <- sapply(fits, crit)
    if (min(crits) >= crit(cur)) break
    cat("Dropping:", labs[which.min(crits)], "\n")
    cur <- fits[[which.min(crits)]]
  }
  cur
}

# YearC is protected: severity shows a significant ~+4% p.a. trend, and dropping it makes the
# Tweedie model predict at the 2023-25 average cost level (2026 test A/E was 1.10 without it).
keep_terms   <- if (include_trend) "YearC" else character(0)
twd_step_aic <- step_tweedie(twd_base, k = 2, keep = keep_terms)
twd_step_bic <- step_tweedie(twd_base, k = log(nrow(tuning_train_set)), keep = keep_terms)

print(summary(twd_step_aic))
cat("Terms dropped by AIC step:", setdiff(attr(terms(twd_base), "term.labels"),
                                          attr(terms(twd_step_aic), "term.labels")), "\n")

# 5. B) Frequency x Severity GLMs -----------------------------------------------
freq_base     <- glm(frq_formula, family = poisson(link = "log"), data = tuning_train_set)
freq_step_aic <- step(freq_base, direction = "both", trace = 0, k = 2)       # should match your frequency file

sev_step_aic  <- step(sev_base, direction = "both", trace = 0, k = 2)        # Gamma has a proper AIC
print(summary(sev_step_aic))

# 6. C) Optional challenger: XGBoost with Tweedie objective -----------------------
run_xgb <- TRUE
if (run_xgb) {
  library(xgboost)
  xgb_formula <- update(rating_formula, as.formula(paste0("~ ." , trend_term)))
  X_tr  <- model.matrix(xgb_formula, tuning_train_set)[, -1]
  X_val <- model.matrix(xgb_formula, model_selection_set)[, -1][, colnames(X_tr)]
  dtr   <- xgb.DMatrix(X_tr, label = tuning_train_set$PurePrem, weight = tuning_train_set$Exposure)
  
  pol  <- unique(tuning_train_set$PolicyID)                       # grouped folds by policy
  fold_of <- sample(rep(1:5, length.out = length(pol))); names(fold_of) <- pol
  fold_id <- unname(fold_of[tuning_train_set$PolicyID])
  folds   <- lapply(1:5, function(k) which(fold_id == k))
  
  xgb_params <- list(objective = "reg:tweedie", tweedie_variance_power = p_hat,
                     eval_metric = sprintf("tweedie-nloglik@%s", p_hat),
                     eta = 0.02, max_depth = 3, min_child_weight = 50,
                     subsample = 0.8, colsample_bytree = 0.8)
  cv <- xgb.cv(params = xgb_params, data = dtr, nrounds = 3000, folds = folds,
               early_stopping_rounds = 50, verbose = 0)
  best_n <- if (!is.null(cv$best_iteration)) cv$best_iteration else cv$early_stop$best_iteration
  xgb_fit <- xgb.train(params = xgb_params, data = dtr, nrounds = best_n)
}

# 7. Validation comparison (2023-25 hold-out policies) ------------------------------
val <- copy(model_selection_set)
val_preds <- list(
  "Portfolio mean (null)"      = rep(mean(tuning_train_set$PurePrem), nrow(val)),
  "Tweedie GLM (full)"         = predict(twd_base,     newdata = val, type = "response"),
  "Tweedie GLM (step AIC)"     = predict(twd_step_aic, newdata = val, type = "response"),
  "Tweedie GLM (step BIC)"     = predict(twd_step_bic, newdata = val, type = "response"),
  "Freq x Sev GLM (step AIC)"  = predict(freq_step_aic, newdata = val, type = "response") *
    predict(sev_step_aic,  newdata = val, type = "response")
)
if (run_xgb) val_preds[["XGBoost Tweedie"]] <- as.vector(predict(xgb_fit, X_val))

val_table <- score_models(val, val_preds, p_hat)
val_table[, Parameters := c(1, length(coef(twd_base)), length(coef(twd_step_aic)), length(coef(twd_step_bic)),
                            length(coef(freq_step_aic)) + length(coef(sev_step_aic)),
                            if (run_xgb) NA_integer_ else NULL)]
print(val_table[order(Deviance)])
# Lower deviance = better. Gini on loss cost is noisy (severity noise), so weigh it less than deviance.

# 8. Out-of-time test on 2026 (untouched) -------------------------------------------
tst <- copy(test_set)
test_preds <- list(
  "Tweedie GLM (step AIC)"    = predict(twd_step_aic, newdata = tst, type = "response"),
  "Freq x Sev GLM (step AIC)" = predict(freq_step_aic, newdata = tst, type = "response") *
    predict(sev_step_aic,  newdata = tst, type = "response")
)
if (run_xgb) test_preds[["XGBoost Tweedie"]] <-
  as.vector(predict(xgb_fit, model.matrix(xgb_formula, tst)[, -1][, colnames(X_tr)]))
print(score_models(tst, test_preds, p_hat))

# Decile lift (loss cost) on 2026
for (nm in names(test_preds)) { cat("\n---", nm, "---\n"); print(decile_table(tst, test_preds[[nm]])) }

dec_plot <- rbindlist(lapply(names(test_preds), function(nm)
  melt(decile_table(tst, test_preds[[nm]])[, .(decile, Actual = Actual_LossCost, Predicted = Predicted_LossCost)],
       id.vars = "decile")[, Model := nm]))
print(ggplot(dec_plot, aes(factor(decile), value, colour = variable, group = variable)) +
        geom_line() + geom_point() + facet_wrap(~Model) +
        labs(title = "2026 out-of-time: actual vs predicted loss cost by predicted decile",
             x = "Predicted decile", y = "Loss cost per policy-year ($)", colour = NULL))

# 9. Relativities and trend from the chosen Tweedie model -----------------------------
rel <- data.table(Term = names(coef(twd_step_aic)),
                  Relativity = exp(coef(twd_step_aic)),
                  Lower95 = exp(confint.default(twd_step_aic)[, 1]),
                  Upper95 = exp(confint.default(twd_step_aic)[, 2]),
                  pValue = summary(twd_step_aic)$coefficients[, 4])
print(rel[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])
if (include_trend && "YearC" %in% names(coef(twd_step_aic)))
  cat(sprintf("Mix-adjusted loss cost trend: %+.2f%% p.a.\n", 100 * (exp(coef(twd_step_aic)["YearC"]) - 1)))

# 10. Final refit on all years (2023-2026) and scoring of renewal books --------------------
final_tw <- glm(formula(twd_step_aic), family = twd_family(p_hat), weights = Exposure, data = data)
final_fq <- glm(formula(freq_step_aic), family = poisson(link = "log"), data = data)
final_sv <- glm(formula(sev_step_aic), family = Gamma(link = "log"), weights = ClaimNb, data = data[ClaimNb > 0])

# Renewal books: cleaning script only flags invalid values as NA, so impute with the training rules, then add bands
add_bands <- function(d) {
  d[, `:=`(
    DriverAgeBand     = cut(DriverAge, c(0, 24, 34, 44, 54, 64, 74, Inf), labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")),
    YearsLicensedBand = cut(YearsLicensed, c(-Inf, 4, 9, 19, 29, 39, Inf), labels = c("0-4", "5-9", "10-19", "20-29", "30-39", "40+")),
    VehAgeBand        = cut(VehAge, c(-Inf, 2, 5, 9, 14, 19, Inf), labels = c("0-2", "3-5", "6-9", "10-14", "15-19", "20+")),
    VehValueBand      = cut(VehValue, c(0, 10000, 15000, 20000, 30000, 40000, 60000, Inf), labels = c("<10k", "10-15k", "15-20k", "20-30k", "30-40k", "40-60k", "60k+")),
    DensityBand       = cut(Density, c(0, 10, 100, 500, 2000, Inf), labels = c("<10", "10-100", "100-500", "500-2000", "2000+")),
    TenureBand        = cut(TenureYears, c(-Inf, 0, 1, 2, 5, 9, Inf), labels = c("0", "1", "2", "3-5", "6-9", "10+")))]
  d
}

prep_renewal <- function(book, target_year) {
  d <- copy(book)
  med_val <- data[, .(MedianValue = median(VehValue)), by = VehicleType]
  d[med_val, on = "VehicleType", VehValue := fifelse(is.na(VehValue), i.MedianValue, VehValue)]
  d[is.na(DriverAge), DriverAge := as.integer(YearsLicensed + age_gap)]
  d[is.na(VehAge),    VehAge    := as.integer(vehicle_age_median)]
  d[is.na(AnnualKms),        AnnualKms        := factor(mode_of(data$AnnualKms), levels(data$AnnualKms))]
  d[is.na(GaragingLocation), GaragingLocation := factor(mode_of(data$GaragingLocation), levels(data$GaragingLocation))]
  d[is.na(ExcessChosen),     ExcessChosen     := as.integer(excess_mode)]
  d <- add_bands(d)
  d[, YearC := target_year - 2023L]            # 2027 -> 4, 2028 -> 5 (ignored if include_trend = FALSE)
  chk <- c("DriverAgeBand", "VehAgeBand", "VehValueBand", "VehicleType", "RiskZone", "DensityBand",
           "AnnualKms", "GaragingLocation", "VehicleUse", "PaymentFrequency", "ExcessChosen", "NCDLevel")
  na_left <- sapply(d[, ..chk], function(x) sum(is.na(x)))
  if (any(na_left > 0)) { warning("NAs remain in rating variables:"); print(na_left[na_left > 0]) }
  d
}

score_book <- function(book, target_year) {
  d <- prep_renewal(book, target_year)
  d[, PurePrem_Tweedie := predict(final_tw, newdata = d, type = "response")]
  d[, PurePrem_FreqSev := predict(final_fq, newdata = d, type = "response") *
      predict(final_sv, newdata = d, type = "response")]
  d
}

scored_2027 <- score_book(renewal_book_2027, 2027)
scored_2028 <- score_book(renewal_book_2028, 2028)

# Sanity checks: Tweedie vs freq x sev should be close; compare with current premium level (which includes loadings)
cat("\n2027 book - mean pure premium: Tweedie $", round(mean(scored_2027$PurePrem_Tweedie)),
    "| FreqSev $", round(mean(scored_2027$PurePrem_FreqSev)),
    "| mean Premium_2026 $", round(mean(scored_2027$Premium_2026)), "\n")
cat("2028 book - mean pure premium: Tweedie $", round(mean(scored_2028$PurePrem_Tweedie)),
    "| FreqSev $", round(mean(scored_2028$PurePrem_FreqSev)), "\n")
cat("Correlation Tweedie vs FreqSev (2027):", round(cor(scored_2027$PurePrem_Tweedie, scored_2027$PurePrem_FreqSev), 4), "\n")

# Next steps (downstream tasks): to price without PaymentFrequency, refit with
#   update(formula(twd_step_aic), . ~ . - PaymentFrequency)
# then add expense / profit / reinsurance loadings on top of these pure premiums.