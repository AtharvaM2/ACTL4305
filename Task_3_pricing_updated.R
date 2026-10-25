# ============================================================================
# TASK 3: PRICE THE 2028 RENEWAL BOOK
# ============================================================================
# HOW TO RUN: run your Task 2 script first, in the same R session. This script
# needs `data`, `renewal_book_2028` (from Data Cleaning and EDA.R) and the
# priced 2027 book `book_2027`. If `book_2027` is not in memory it falls back to
# "priced_2027.csv", which this script writes whenever `book_2027` is available.
#
# WATERFALL (same order as Task 2, so promised discounts survive the full run):
#   1. Technical price = frequency x severity, PaymentFrequency REMOVED
#   2. Loaded premium  = (technical price at NCD 0 + $85) / (1 - 10% - 8%)
#   3. Tiered cap / cup of the loaded premium vs the 2027 CapCupLoadedPremium
#      (new business: no prior premium, so no cap / cup)
#   4. Discounts on the capped / cupped premium: NCD, multi-policy, loyalty
#      (new business: loyalty = 0, no tenure history)
#   5. 3% instalment loading, applied last, monthly payers only
#
# BASELINE CARRIED FORWARD: 2027 CapCupLoadedPremium (pre-discount,
# pre-instalment, post cap/cup). Justification: the 2028 loaded premium is also
# a pre-discount (NCD 0) base, so the cap/cup compares like with like. Carrying
# forward the 2027 FinalPrice would embed last year's NCD, loyalty, multi-policy
# and instalment effects in the baseline, and those discounts would then be
# applied a second time in 2028 (double counting). It would also let a change
# in NCD or tenure move the "capped" figure instead of the discount.
# ============================================================================

library(data.table)

# 0. Setup and 2027 baseline -----------------------------------------------
if (!exists("data") || !exists("renewal_book_2028")) source("Data Cleaning and EDA.R")
stopifnot("VehValueBand" %in% names(data))

t3_out <- "task3_outputs"
dir.create(t3_out, showWarnings = FALSE)

baseline_cols <- c("PolicyID", "LegacyGrossBase_2026", "TechnicalPrice", "LoadedPremium",
                   "RawRateChange", "EffectiveRateChange", "CapCupLoadedPremium", "FinalPrice")
export_2027_cols <- unique(c(baseline_cols, "NCDDiscountDollar", "MultiPolicyDiscountDollar",
                             "LoyaltyDiscountDollar", "TotalDiscountDollar"))

if (exists("book_2027") && all(export_2027_cols %in% names(book_2027))) {
  fwrite(book_2027[, ..export_2027_cols], "priced_2027.csv")
  prior_2027 <- copy(book_2027[, ..baseline_cols])
} else if (file.exists("priced_2027.csv")) {
  prior_2027 <- fread("priced_2027.csv", select = baseline_cols)
} else {
  stop("Run Task 2 first (needs book_2027 with CapCupLoadedPremium) or provide priced_2027.csv.")
}

keep_names <- setdiff(names(prior_2027), c("PolicyID", "LegacyGrossBase_2026"))
setnames(prior_2027, keep_names, paste0(keep_names, "_2027"))
stopifnot(!anyDuplicated(prior_2027$PolicyID),
          all(is.finite(prior_2027$CapCupLoadedPremium_2027)),
          all(prior_2027$CapCupLoadedPremium_2027 > 0))

# 1. Commercial parameters (identical to Task 2) -----------------------------
FIXED_COST     <- 85
VAR_EXPENSE    <- 0.10
TARGET_PROFIT  <- 0.08
NCD_RATES      <- c("0" = 0.00, "1" = 0.10, "2" = 0.20, "3" = 0.30,
                    "4" = 0.40, "5" = 0.50, "6" = 0.60)
MULTI_RATE     <- 0.10
INSTAL_RATE    <- 0.03
YEAR_INDEX_2028 <- 2L     # severity trend: 2028 - 2026 (same assumption as Task 2)

# Same tiered cap / cup design as Task 2, applied to the 2028 raw rate change
CAP_BREAKS <- c(-Inf, 0.05, 0.15, 0.225, 0.30, 0.60, Inf)
CAP_VALUES <- c(0.05, 0.10, 0.18, 0.25, 0.35, 0.50)
CUP_BREAKS <- c(-Inf, -0.20, Inf)
CUP_VALUES <- c(-0.10, -0.05)

# 2. Refit Task 1 / Task 2 models on 2023-2026, PaymentFrequency removed -----
# Formulas are the stepwise-AIC selections used in Task 2. Edit here if your
# Task 1 selections differ.
data[, YearIndex := Year - 2026]
sev_train <- copy(data[ClaimNb > 0])
sev_train[, AverageClaim := ClaimAmount / ClaimNb]

freq_formula_pf <- ClaimNb ~ RiskZone + AnnualKms + GaragingLocation + VehicleUse +
  PaymentFrequency + factor(ExcessChosen) + factor(NCDLevel)
freq_formula_2028 <- update(freq_formula_pf, . ~ . - PaymentFrequency)

fit_freq_pf   <- glm(freq_formula_pf,   family = poisson(link = "log"), data = data)  # Task 1/2 model (diagnostic only)
fit_freq_2028 <- glm(freq_formula_2028, family = poisson(link = "log"), data = data)  # 2028 pricing model
fit_sev_2028  <- glm(AverageClaim ~ VehValueBand + VehicleType + RiskZone +
                       factor(ExcessChosen) + YearIndex,
                     family = Gamma(link = "log"), weights = ClaimNb, data = sev_train)
stopifnot(fit_freq_pf$converged, fit_freq_2028$converged, fit_sev_2028$converged)

if (exists("final_freq_model") && !isTRUE(all.equal(coef(final_freq_model), coef(fit_freq_pf))))
  warning("Refitted frequency model differs from final_freq_model in your Task 2 session.")
if (exists("final_sev_model") && !isTRUE(all.equal(coef(final_sev_model), coef(fit_sev_2028))))
  warning("Refitted severity model differs from final_sev_model in your Task 2 session.")

# 3. Prepare the 2028 book and join the prior-year baseline --------------------
book_2028 <- copy(renewal_book_2028)
book_2028[, `:=`(
  YearIndex    = YEAR_INDEX_2028,
  VehValueBand = cut(VehValue,
                     breaks = c(-Inf, 10000, 15000, 20000, 30000, 40000, 60000, Inf),
                     labels = c("<10k", "10-15k", "15-20k", "20-30k", "30-40k", "40-60k", "60k+")),
  IsNewBusiness = NewBusinessFlag == 1L
)]

model_vars <- c("VehValue", "VehicleType", "RiskZone", "AnnualKms", "GaragingLocation",
                "VehicleUse", "ExcessChosen", "NCDLevel", "TenureYears",
                "MultiPolicyFlag", "PaymentFrequency")
na_counts <- sapply(book_2028[, ..model_vars], function(x) sum(is.na(x)))
if (any(na_counts > 0)) stop("Missing values in 2028 book: ",
                             paste(names(na_counts)[na_counts > 0], na_counts[na_counts > 0],
                                   sep = "=", collapse = ", "))
stopifnot(!anyDuplicated(book_2028$PolicyID),
          all(startsWith(book_2028$PolicyID, "NEW") == book_2028$IsNewBusiness))

n_book <- nrow(book_2028)
book_2028 <- merge(book_2028, prior_2027, by = "PolicyID", all.x = TRUE, sort = FALSE)
stopifnot(nrow(book_2028) == n_book,
          book_2028[IsNewBusiness == TRUE,  all(is.na(CapCupLoadedPremium_2027))],
          book_2028[IsNewBusiness == FALSE, !anyNA(CapCupLoadedPremium_2027)])
cat("Renewals:", book_2028[IsNewBusiness == FALSE, .N],
    "| New business:", book_2028[IsNewBusiness == TRUE, .N], "\n")

# 4. Waterfall functions (self-contained, same logic and order as Task 2) ------
t3_apply_cap_cup <- function(dt) {
  dt[, `:=`(RawRateChange = NA_real_, CapPct = NA_real_, CupPct = NA_real_,
            EffectiveRateChange = NA_real_)]
  dt[IsNewBusiness == FALSE, RawRateChange := LoadedPremium / CapCupLoadedPremium_2027 - 1]
  dt[IsNewBusiness == FALSE, `:=`(
    CapPct = CAP_VALUES[cut(RawRateChange, CAP_BREAKS, labels = FALSE, right = TRUE)],
    CupPct = CUP_VALUES[cut(RawRateChange, CUP_BREAKS, labels = FALSE, right = TRUE)]
  )]
  dt[IsNewBusiness == FALSE, EffectiveRateChange := pmin(pmax(RawRateChange, CupPct), CapPct)]
  dt[, CapCupLoadedPremium := fifelse(IsNewBusiness, LoadedPremium,
                                      CapCupLoadedPremium_2027 * (1 + EffectiveRateChange))]
  dt[, Capped := !IsNewBusiness & EffectiveRateChange < RawRateChange - 1e-9]
  dt[, Cupped := !IsNewBusiness & EffectiveRateChange > RawRateChange + 1e-9]
  invisible(dt)
}

t3_apply_discounts <- function(dt) {
  dt[, NCDDiscount := unname(NCD_RATES[as.character(NCDLevel)])]
  stopifnot(!anyNA(dt$NCDDiscount))
  dt[, MultiPolicyDiscount := fifelse(MultiPolicyFlag == 1, MULTI_RATE, 0)]
  dt[, LoyaltyDiscount := fcase(IsNewBusiness,      0,       # no tenure history
                                TenureYears >= 10, 0.10,
                                TenureYears >= 6,  0.06,
                                TenureYears >= 3,  0.03,
                                default = 0)]
  dt[, DiscountMultiplier := (1 - NCDDiscount) * (1 - MultiPolicyDiscount) * (1 - LoyaltyDiscount)]
  dt[, DiscountedPremium := CapCupLoadedPremium * DiscountMultiplier]
  # Waterfall dollar attribution in the same order as Task 2: NCD, multi-policy, loyalty
  dt[, NCDDiscountDollar         := CapCupLoadedPremium * NCDDiscount]
  dt[, MultiPolicyDiscountDollar := (CapCupLoadedPremium - NCDDiscountDollar) * MultiPolicyDiscount]
  dt[, LoyaltyDiscountDollar     := (CapCupLoadedPremium - NCDDiscountDollar -
                                       MultiPolicyDiscountDollar) * LoyaltyDiscount]
  dt[, TotalDiscountDollar := CapCupLoadedPremium - DiscountedPremium]
  invisible(dt)
}

t3_apply_instalment <- function(dt) {
  dt[, InstalmentRate   := fifelse(PaymentFrequency == "Monthly", INSTAL_RATE, 0)]
  dt[, InstalmentDollar := DiscountedPremium * InstalmentRate]
  dt[, FinalPrice       := DiscountedPremium + InstalmentDollar]
  dt[, AmountPerPayment := fifelse(InstalmentRate > 0, round(FinalPrice / 12, 2), round(FinalPrice, 2))]
  invisible(dt)
}

t3_price_book <- function(b) {
  d <- copy(b)
  d[, TechnicalPrice := predict(fit_freq_2028, d, type = "response") *
      predict(fit_sev_2028, d, type = "response")]
  base <- copy(d)[, NCDLevel := 0L]            # undiscounted base risk (as Task 2)
  d[, BaseTechnicalPrice := predict(fit_freq_2028, base, type = "response") *
      predict(fit_sev_2028, base, type = "response")]
  d[, LoadedPremium := (BaseTechnicalPrice + FIXED_COST) / (1 - VAR_EXPENSE - TARGET_PROFIT)]
  t3_apply_cap_cup(d)
  t3_apply_discounts(d)
  t3_apply_instalment(d)
  d
}

# 5. Price the 2028 book --------------------------------------------------------
priced_2028 <- t3_price_book(book_2028)

# Diagnostic only: what the Task 1/2 model (with PaymentFrequency) would have said
lv_pf <- levels(book_2028$PaymentFrequency)
book_pf_base <- copy(book_2028)[, `:=`(NCDLevel = 0L, PaymentFrequency = factor("Annual", levels = lv_pf))]
sev_hat <- predict(fit_sev_2028, book_2028, type = "response")
priced_2028[, TechnicalPrice_withPF := predict(fit_freq_pf, book_2028, type = "response") * sev_hat]
priced_2028[, LoadedPremium_withPF := (predict(fit_freq_pf, book_pf_base, type = "response") *
                                         predict(fit_sev_2028, book_pf_base, type = "response") +
                                         FIXED_COST) / (1 - VAR_EXPENSE - TARGET_PROFIT)]

cat("\n=== 2028 PORTFOLIO PRICING ===\n")
cat("Mean Technical Price (no PaymentFrequency) : $", round(mean(priced_2028$TechnicalPrice), 2), "\n")
cat("Mean Loaded Premium (2028 base)            : $", round(mean(priced_2028$LoadedPremium), 2), "\n")
cat("Mean Cap/Cup Loaded Premium                : $", round(mean(priced_2028$CapCupLoadedPremium), 2), "\n")
cat("Mean Total Discount Given                  : $", round(mean(priced_2028$TotalDiscountDollar), 2), "\n")
cat("Mean Final Premium                         : $", round(mean(priced_2028$FinalPrice), 2), "\n")

# Waterfall bridge from the loaded premium to the final premium
bridge_2028 <- priced_2028[, .(
  Step = c("Loaded premium", "Cap / cup adjustment", "NCD discount", "Multi-policy discount",
           "Loyalty discount", "Instalment loading", "Final premium"),
  Amount = c(sum(LoadedPremium), sum(CapCupLoadedPremium - LoadedPremium), -sum(NCDDiscountDollar),
             -sum(MultiPolicyDiscountDollar), -sum(LoyaltyDiscountDollar),
             sum(InstalmentDollar), sum(FinalPrice)))]
stopifnot(abs(sum(bridge_2028$Amount[1:6]) - bridge_2028$Amount[7]) < 1e-4)
print(bridge_2028)
fwrite(bridge_2028, file.path(t3_out, "01_waterfall_bridge_2028.csv"))

# Cap / cup status and lapse tiers (tiers use the pre-discount rate change, as in Task 2)
renewals <- priced_2028[IsNewBusiness == FALSE]
cat("\nRenewals capped:", sum(renewals$Capped), sprintf("(%.1f%%)", 100 * mean(renewals$Capped)),
    "| cupped:", sum(renewals$Cupped), sprintf("(%.1f%%)", 100 * mean(renewals$Cupped)), "\n")
lapse_tiers <- renewals[, .(Renewals = .N), keyby = .(
  RateChangeTier = fcase(EffectiveRateChange <= 0.05, "1. 0-5%  (+0% lapse)",
                         EffectiveRateChange <= 0.15, "2. 5-15% (+5% lapse)",
                         EffectiveRateChange <= 0.30, "3. 15-30% (+15% lapse)",
                         default = "4. >30% (+35% lapse)"))]
lapse_tiers[, Share := round(Renewals / sum(Renewals), 4)]
print(lapse_tiers)

# Final-price change vs 2027 (differs from the base change when NCD / tenure moved)
renewals[, FinalChange := FinalPrice / FinalPrice_2027 - 1]
cat("\nFinal-price change vs 2027 (renewals): mean",
    sprintf("%.1f%%", 100 * mean(renewals$FinalChange)), "| quantiles:\n")
print(round(100 * quantile(renewals$FinalChange, c(0, .05, .25, .5, .75, .95, 1)), 1))

# Portfolio loss / combined ratio. Same definition as Task 2: claims / FinalPrice.
t3_portfolio <- function(d, label, claims) {
  revenue  <- sum(d$FinalPrice)
  expenses <- nrow(d) * FIXED_COST + revenue * VAR_EXPENSE
  data.table(View = label, Policies = nrow(d), Total_Premium_M = round(revenue / 1e6, 2),
             Mean_Final = round(mean(d$FinalPrice), 2),
             Loss_Ratio = round(100 * sum(claims) / revenue, 1),
             Combined_Ratio = round(100 * (sum(claims) + expenses) / revenue, 1),
             Profit_Margin = round(100 - 100 * (sum(claims) + expenses) / revenue, 1))
}
portfolio_2028 <- rbind(
  t3_portfolio(priced_2028, "All policies", priced_2028$TechnicalPrice),
  t3_portfolio(renewals, "Renewals only", renewals$TechnicalPrice),
  t3_portfolio(priced_2028[IsNewBusiness == TRUE], "New business only",
               priced_2028[IsNewBusiness == TRUE, TechnicalPrice]))
# (The with-PaymentFrequency model gives the same portfolio total of expected claims,
# because a Poisson GLM with an intercept reproduces total claims; it only moves cost
# between annual and monthly payers. See pf_impact in Section 7.)
print(portfolio_2028)
fwrite(portfolio_2028, file.path(t3_out, "02_portfolio_summary_2028.csv"))

# 6. Promise tests through the FULL 2028 waterfall --------------------------------
# End-to-end counterfactuals: set every policy to a given NCD / tenure / multi-policy
# status, price it through the whole waterfall, and compare with the same policy at
# (NCD 0, tenure 0, no multi-policy). The ratio of final prices must equal the
# promised multiplier, whatever the cap / cup did. Expected values use a separate
# lookup (findInterval), independent of the pipeline code.
LOY_FROM <- c(0, 3, 6, 10); LOY_RATE <- c(0, 0.03, 0.06, 0.10)
expected_multiplier <- function(ncd, ten, multi)
  (1 - NCD_RATES[[as.character(ncd)]]) * (1 - LOY_RATE[findInterval(ten, LOY_FROM)]) *
  (1 - MULTI_RATE * multi)

set_status <- function(ncd, ten, multi) {
  d <- copy(book_2028)
  d[, `:=`(NCDLevel = as.integer(ncd), TenureYears = as.integer(ten), MultiPolicyFlag = as.integer(multi))]
  t3_price_book(d)
}
ref <- set_status(0, 0, 0)

cases <- rbindlist(list(
  data.table(Promise = "NCD",          NCD = 0:6, Tenure = 0L, Multi = 0L),
  data.table(Promise = "Loyalty",      NCD = 0L,  Tenure = c(0L, 2L, 3L, 5L, 6L, 9L, 10L, 15L), Multi = 0L),
  data.table(Promise = "Multi-policy", NCD = 0L,  Tenure = 0L, Multi = 0:1),
  data.table(Promise = "Combined",     NCD = c(3L, 6L), Tenure = c(6L, 10L), Multi = 1L)))

promise_results <- rbindlist(lapply(seq_len(nrow(cases)), function(i) {
  cs <- cases[i]
  scoped <- if (cs$Tenure > 0) !priced_2028$IsNewBusiness else rep(TRUE, nrow(priced_2028))
  d <- set_status(cs$NCD, cs$Tenure, cs$Multi)
  ratio <- (d$FinalPrice / ref$FinalPrice)[scoped]
  exp_m <- expected_multiplier(cs$NCD, cs$Tenure, cs$Multi)
  data.table(Promise = cs$Promise, NCD = cs$NCD, Tenure = cs$Tenure, Multi = cs$Multi,
             Policies = length(ratio), Promised_Multiplier = exp_m,
             Min_Delivered = min(ratio), Max_Delivered = max(ratio),
             Pass = all(abs(ratio - exp_m) < 1e-9))
}))
# New business must receive no loyalty discount, whatever TenureYears says
nb_d <- set_status(0, 10, 0)
nb_ratio <- (nb_d$FinalPrice / ref$FinalPrice)[priced_2028$IsNewBusiness]
promise_results <- rbind(promise_results,
                         data.table(Promise = "Loyalty (new business, tenure 10)", NCD = 0L, Tenure = 10L, Multi = 0L,
                                    Policies = length(nb_ratio), Promised_Multiplier = 1,
                                    Min_Delivered = min(nb_ratio), Max_Delivered = max(nb_ratio),
                                    Pass = all(abs(nb_ratio - 1) < 1e-9)))

cat("\n=== PROMISE TESTS (full 2028 waterfall, end-to-end) ===\n")
print(promise_results)
fwrite(promise_results, file.path(t3_out, "03_promise_tests_2028.csv"))
stopifnot(all(promise_results$Pass))
cat("All promised discounts delivered in full for", sum(promise_results$Policies), "policy tests.\n")

# 7. Impact of removing PaymentFrequency ---------------------------------------
# (a) Policy flip test: flipping PaymentFrequency must change ONLY the 3% instalment
flip <- copy(book_2028)
flip[, PaymentFrequency := factor(fifelse(PaymentFrequency == "Monthly", "Annual", "Monthly"),
                                  levels = lv_pf)]
flipped <- t3_price_book(flip)
was_monthly <- priced_2028$PaymentFrequency == "Monthly"
flip_checks <- data.table(
  Check = c("TechnicalPrice unchanged", "LoadedPremium unchanged", "CapCupLoadedPremium unchanged",
            "DiscountedPremium unchanged", "All discount dollars unchanged",
            "FinalPrice moves by exactly the 3% instalment loading"),
  Pass = c(isTRUE(all.equal(flipped$TechnicalPrice, priced_2028$TechnicalPrice)),
           isTRUE(all.equal(flipped$LoadedPremium, priced_2028$LoadedPremium)),
           isTRUE(all.equal(flipped$CapCupLoadedPremium, priced_2028$CapCupLoadedPremium)),
           isTRUE(all.equal(flipped$DiscountedPremium, priced_2028$DiscountedPremium)),
           isTRUE(all.equal(flipped[, NCDDiscountDollar + MultiPolicyDiscountDollar + LoyaltyDiscountDollar],
                            priced_2028[, NCDDiscountDollar + MultiPolicyDiscountDollar + LoyaltyDiscountDollar])),
           isTRUE(all.equal(flipped$FinalPrice / priced_2028$FinalPrice,
                            fifelse(was_monthly, 1 / (1 + INSTAL_RATE), 1 + INSTAL_RATE)))))
cat("\n=== PAYMENTFREQUENCY FLIP TEST ===\n")
print(flip_checks)
fwrite(flip_checks, file.path(t3_out, "04_paymentfrequency_flip_test.csv"))
stopifnot(all(flip_checks$Pass))

# (b) How much did PaymentFrequency matter in the Task 1 / Task 2 frequency model?
pf_est <- coef(summary(fit_freq_pf))["PaymentFrequencyMonthly", ]
pf_effect <- data.table(
  Term = "PaymentFrequency = Monthly (vs Annual)",
  Relativity = exp(pf_est["Estimate"]),
  Lower95 = exp(pf_est["Estimate"] - 1.96 * pf_est["Std. Error"]),
  Upper95 = exp(pf_est["Estimate"] + 1.96 * pf_est["Std. Error"]),
  pValue = pf_est["Pr(>|z|)"])
pf_fit <- data.table(
  Model = c("With PaymentFrequency (Task 1 / 2)", "Without PaymentFrequency (2028)"),
  Parameters = c(length(coef(fit_freq_pf)), length(coef(fit_freq_2028))),
  Deviance = c(deviance(fit_freq_pf), deviance(fit_freq_2028)),
  AIC = c(AIC(fit_freq_pf), AIC(fit_freq_2028)))
pf_lrt <- anova(fit_freq_2028, fit_freq_pf, test = "Chisq")
cat("\n=== HOW MUCH DID PAYMENTFREQUENCY MATTER? ===\n")
print(pf_effect)
print(pf_fit)
cat("Likelihood-ratio test p-value (PaymentFrequency):", signif(pf_lrt[2, "Pr(>Chi)"], 4), "\n")

# (c) Who pays for the change: technical price and loaded base, with vs without
pf_impact <- priced_2028[, .(
  Policies = .N,
  Mean_Tech_NoPF = round(mean(TechnicalPrice), 2),
  Mean_Tech_WithPF = round(mean(TechnicalPrice_withPF), 2),
  Cost_vs_Priced = round(sum(TechnicalPrice_withPF) / sum(TechnicalPrice), 4),
  Mean_Loaded_NoPF = round(mean(LoadedPremium), 2),
  Mean_Loaded_WithPF = round(mean(LoadedPremium_withPF), 2),
  Loaded_Shift_Pct = round(100 * (sum(LoadedPremium) / sum(LoadedPremium_withPF) - 1), 2)
), keyby = PaymentFrequency]
cat("\nCost_vs_Priced > 1 means expected claims (with PaymentFrequency) exceed the price basis (without).\n")
print(pf_impact)
fwrite(pf_impact, file.path(t3_out, "05_paymentfrequency_impact.csv"))

# 8. Compounding / loyalty-tax diagnostics ---------------------------------------
status_of <- function(capped, cupped) fcase(capped, "Capped", cupped, "Cupped", default = "Free")
renewals[, `:=`(
  Capped_2027 = EffectiveRateChange_2027 < RawRateChange_2027 - 1e-9,
  Cupped_2027 = EffectiveRateChange_2027 > RawRateChange_2027 + 1e-9)]
renewals[, `:=`(Status_2027 = status_of(Capped_2027, Cupped_2027),
                Status_2028 = status_of(Capped, Cupped))]

cat("\n=== CAP / CUP STATUS: 2027 vs 2028 (renewals) ===\n")
status_table <- dcast(renewals[, .N, by = .(Status_2027, Status_2028)],
                      Status_2027 ~ Status_2028, value.var = "N", fill = 0)
print(status_table)
fwrite(status_table, file.path(t3_out, "06_status_2027_vs_2028.csv"))

# Cumulative two-year movement of the pre-discount base vs the 2026 legacy gross base
renewals[, `:=`(Cum_Target = LoadedPremium / LegacyGrossBase_2026 - 1,
                Cum_Actual = CapCupLoadedPremium / LegacyGrossBase_2026 - 1)]
cat("\nTwo-year pre-discount movement vs 2026 gross base (renewals): mean target",
    sprintf("%.1f%%", 100 * mean(renewals$Cum_Target)), "| mean applied",
    sprintf("%.1f%%", 100 * mean(renewals$Cum_Actual)), "\n")

# Loyalty-tax test: renewal price vs the price a NEW customer with identical risk
# attributes would be quoted (uncapped loaded premium, NCD and multi-policy
# discounts, no loyalty discount, same instalment loading)
renewals[, NB_Equivalent := LoadedPremium * (1 - NCDDiscount) * (1 - MultiPolicyDiscount) *
           (1 + InstalmentRate)]
renewals[, VsNewCustomer := FinalPrice / NB_Equivalent - 1]
renewals[, LoyaltyBand := factor(fcase(TenureYears >= 10, "10+ yrs", TenureYears >= 6, "6-9 yrs",
                                       TenureYears >= 3, "3-5 yrs", default = "0-2 yrs"),
                                 levels = c("0-2 yrs", "3-5 yrs", "6-9 yrs", "10+ yrs"))]
loyalty_tax <- renewals[, .(Renewals = .N,
                            Mean_VsNewCustomer_Pct = round(100 * mean(VsNewCustomer), 2),
                            Share_Paying_More = round(mean(VsNewCustomer > 1e-9), 4),
                            Share_Cupped_2028 = round(mean(Cupped), 4),
                            Share_Cupped_Both_Years = round(mean(Cupped & Cupped_2027), 4)),
                        keyby = LoyaltyBand]
cat("\n=== RENEWAL PRICE vs EQUIVALENT NEW CUSTOMER, BY LOYALTY BAND ===\n")
print(loyalty_tax)
fwrite(loyalty_tax, file.path(t3_out, "07_loyalty_tax_check_2028.csv"))

cat("\nRenewals cupped in BOTH years :", renewals[Cupped & Cupped_2027, .N], "\n")
cat("Renewals capped in BOTH years :", renewals[Capped & Capped_2027, .N], "\n")
cat("Mean premium above new-customer equivalent, cupped both years: ",
    sprintf("%.1f%%", 100 * renewals[Cupped & Cupped_2027, mean(VsNewCustomer)]), "\n")
# ============================================================================
# 9. GRAPHS  (append to the end of task3_pricing_2028.R)
# ============================================================================
# Uses objects already created above: priced_2028, renewals, bridge_2028,
# pf_impact, CAP_BREAKS, CAP_VALUES, t3_out. PNGs go to task3_outputs/graphs/.
#
# Graphs 1-4 repeat the Task 2 graphs / tables for the 2028 book:
#   1. Distribution of raw rate changes (with the lapse-risk thresholds)
#   2. Distribution of individual policy loss ratios
#   3. Loss ratio by NCD level            (Task 2 table "Loss ratio by NCD level")
#   4. Loss ratio by cap tier             (Task 2 table "Policy distribution across cap tiers")
# Graphs 5-10 are new and support the Task 3 discussion:
#   5. Lapse-risk tiers, 2027 vs 2028     (compounding)
#   6. Waterfall from loaded to final premium
#   7. Cap / cup status, 2027 -> 2028     (compounding)
#   8. Final-price change by NCD movement (why the tail is so wide)
#   9. Renewal price vs equivalent new customer, by loyalty band (loyalty tax)
#  10. PaymentFrequency: expected claims vs price basis after removal
# ============================================================================

library(ggplot2)
library(scales)
theme_set(theme_minimal(base_size = 11))

graph_dir <- file.path(t3_out, "graphs")
dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

status_cols <- c(Capped = "#d95f02", Free = "grey65", Cupped = "#1b9e77")
status_lvls <- c("Capped", "Free", "Cupped")

save_gg <- function(p, name, w = 8, h = 5) {
  print(p)
  ggsave(file.path(graph_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 160)
}
save_base <- function(name, draw, w = 8, h = 5) {
  draw()                                              # show on screen
  png(file.path(graph_dir, paste0(name, ".png")), width = w, height = h, units = "in", res = 160)
  draw()                                              # and save
  dev.off()
}

priced_2028[, PolicyLossRatio := 100 * TechnicalPrice / FinalPrice]   # same definition as Task 2

# 1. Raw rate change distribution (Task 2 graph, 2028 vs 2027 base) -------------
raw_pct <- renewals$RawRateChange * 100
xr <- quantile(raw_pct, c(0.001, 0.999))
draw_raw <- function() {
  hist(raw_pct, breaks = 60, col = "steelblue", border = "white",
       main = "Distribution of Raw Rate Changes (2028 vs 2027 Base, renewals)",
       xlab = "Raw Rate Change (%)", xlim = c(floor(xr[1]), ceiling(xr[2])), xaxt = "n")
  axis(side = 1, at = pretty(c(floor(xr[1]), ceiling(xr[2])), n = 12))
  abline(v = -15, col = "purple", lty = 2, lwd = 2)
  abline(v = -10, col = "blue",   lty = 2, lwd = 1.5)
  abline(v = 0,   col = "gray40", lty = 2, lwd = 1.5)
  abline(v = 5,   col = "green3", lty = 2, lwd = 2)
  abline(v = 15,  col = "orange", lty = 2, lwd = 2)
  abline(v = 30,  col = "red",    lty = 2, lwd = 2)
  legend("topright", bty = "n", cex = 0.85, lty = 2, lwd = 2,
         legend = c("Cup -15%", "Cup -10%", "0% Base", "5% Tier", "15% Lapse Cliff", "30% Severe Cliff"),
         col = c("purple", "blue", "gray40", "green3", "orange", "red"))
}
save_base("01_raw_rate_change_distribution", draw_raw)

# 2. Policy loss ratio distribution (Task 2 graph) --------------------------------
lr_median <- median(priced_2028$PolicyLossRatio)
draw_lr <- function() {
  hist(pmin(priced_2028$PolicyLossRatio, 250), breaks = 50, col = "salmon", border = "white",
       main = "Distribution of Individual Policy Loss Ratios (2028)",
       xlab = "Loss Ratio (%) [Capped at 250% for Display]", xaxt = "n")
  axis(side = 1, at = seq(0, 250, by = 25))
  abline(v = 70,  col = "darkgreen", lty = 2, lwd = 2)
  abline(v = 100, col = "red",       lty = 1, lwd = 2.5)
  abline(v = lr_median, col = "blue", lty = 2, lwd = 2)
  legend("topright", bty = "n", col = c("darkgreen", "red", "blue"), lty = c(2, 1, 2), lwd = c(2, 2.5, 2),
         legend = c("Target (~70%)", "Break-Even (100%)", paste0("Median (", round(lr_median, 1), "%)")))
}
save_base("02_policy_loss_ratio_distribution", draw_lr)

# 3. Loss ratio by NCD level (Task 2 table as a chart) -----------------------------
lr_ncd <- priced_2028[, .(Policies = .N, LossRatio = 100 * sum(TechnicalPrice) / sum(FinalPrice)),
                      keyby = NCDLevel]
lr_ncd[, Label := factor(paste0("NCD ", NCDLevel, "\n(n=", comma(Policies), ")"),
                         levels = paste0("NCD ", NCDLevel, "\n(n=", comma(Policies), ")"))]
p3 <- ggplot(lr_ncd, aes(Label, LossRatio, fill = LossRatio > 100)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 100, colour = "red", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", LossRatio)), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c(`FALSE` = "steelblue", `TRUE` = "firebrick"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "2028 loss ratio by NCD level",
       subtitle = "Aggregate claims / final premium.\nThe NCD 5-6 discounts are not funded by the loaded premium.",
       x = NULL, y = "Loss ratio (%)")
save_gg(p3, "03_loss_ratio_by_ncd")

# 4. Loss ratio by cap tier (Task 2 table as a chart) ------------------------------
tier_labels <- paste0(c("<= 5%", "5-15%", "15-22.5%", "22.5-30%", "30-60%", "> 60%"),
                      "\n(cap ", round(CAP_VALUES * 100), "%)")
cap_tier <- rbind(
  renewals[, .(Tier = as.character(cut(RawRateChange, CAP_BREAKS, labels = tier_labels, right = TRUE)),
               TechnicalPrice, FinalPrice)],
  priced_2028[IsNewBusiness == TRUE, .(Tier = "New\nbusiness", TechnicalPrice, FinalPrice)])
cap_tier <- cap_tier[, .(Policies = .N, LossRatio = 100 * sum(TechnicalPrice) / sum(FinalPrice)), by = Tier]
cap_tier[, Tier := factor(Tier, levels = c(tier_labels, "New\nbusiness"))]
p4 <- ggplot(cap_tier, aes(Tier, LossRatio, fill = LossRatio > 100)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 100, colour = "red", linewidth = 0.8) +
  geom_text(aes(label = paste0(sprintf("%.0f%%", LossRatio), "\nn=", comma(Policies))),
            vjust = -0.3, size = 3.2, lineheight = 0.9) +
  scale_fill_manual(values = c(`FALSE` = "steelblue", `TRUE` = "firebrick"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "2028 loss ratio by raw rate-change tier",
       subtitle = "Tier = raw change vs the 2027 pre-discount premium; the cap applies within each tier",
       x = "Raw rate change tier", y = "Loss ratio (%)")
save_gg(p4, "04_loss_ratio_by_cap_tier", w = 9)

# 5. Lapse-risk tiers, 2027 vs 2028 (compounding) ----------------------------------
tier_of <- function(x) factor(fcase(x <= 0.05, "0-5%", x <= 0.15, "5-15%", x <= 0.30, "15-30%",
                                    default = ">30%"),
                              levels = c("0-5%", "5-15%", "15-30%", ">30%"))
lapse_cmp <- rbind(renewals[, .(Year = "2027", Tier = tier_of(EffectiveRateChange_2027))],
                   renewals[, .(Year = "2028", Tier = tier_of(EffectiveRateChange))])[, .N, by = .(Year, Tier)]
lapse_cmp[, Share := N / sum(N), by = Year]
p5 <- ggplot(lapse_cmp, aes(Tier, Share, fill = Year)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = percent(Share, accuracy = 0.1)), position = position_dodge(0.75),
            vjust = -0.4, size = 3.3) +
  scale_fill_manual(values = c(`2027` = "grey60", `2028` = "steelblue")) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Renewals by size of pre-discount rate change applied",
       subtitle = sprintf("Same %s renewals in both years; the >30%% group is hit a second time", comma(nrow(renewals))),
       x = "Effective rate change after cap/cup", y = "Share of renewals", fill = NULL)
save_gg(p5, "05_lapse_tiers_2027_vs_2028")

# 6. Waterfall: loaded premium -> final premium ------------------------------------
wf <- copy(bridge_2028)
wf[, Amount_M := Amount / 1e6]
cum <- cumsum(wf$Amount_M[1:6])
wf[, `:=`(i = .I,
          Start = c(0, cum[1:5], 0),
          End   = c(cum[1:6], Amount_M[7]),
          Type  = fifelse(Step %in% c("Loaded premium", "Final premium"), "Total",
                          fifelse(Amount < 0, "Decrease", "Increase")))]
wf[, Lab := vapply(strwrap(Step, 12, simplify = FALSE), paste, character(1), collapse = "\n")]
p6 <- ggplot(wf) +
  geom_rect(aes(xmin = i - 0.38, xmax = i + 0.38, ymin = pmin(Start, End), ymax = pmax(Start, End), fill = Type)) +
  geom_text(aes(x = i, y = pmax(Start, End),
                label = ifelse(Type == "Total", sprintf("$%.1fM", Amount_M), sprintf("%+.1fM", Amount_M))),
            vjust = -0.5, size = 3.3) +
  scale_x_continuous(breaks = wf$i, labels = wf$Lab) +
  scale_fill_manual(values = c(Total = "steelblue", Decrease = "firebrick", Increase = "darkgreen")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "2028 premium waterfall: loaded premium to final premium",
       subtitle = "Discounts are applied after the cap/cup, so they are delivered in full",
       x = NULL, y = "$ million", fill = NULL)
save_gg(p6, "06_waterfall_2028", w = 9)

# 7. Cap / cup status: 2027 -> 2028 (compounding) ----------------------------------
trans <- renewals[, .N, by = .(Status_2027, Status_2028)]
trans[, `:=`(Status_2027 = factor(Status_2027, levels = rev(status_lvls)),
             Status_2028 = factor(Status_2028, levels = status_lvls))]
trans[, RowShare := N / sum(N), by = Status_2027]
p7 <- ggplot(trans, aes(Status_2028, Status_2027, fill = N)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = paste0(comma(N), "\n(", percent(RowShare, accuracy = 1), " of row)")), size = 3.6) +
  scale_fill_gradient(low = "grey95", high = "steelblue", guide = "none") +
  labs(title = "Cap / cup status of each renewal: 2027 vs 2028",
       subtitle = "Capped = held below the technical target; Cupped = held above it.\nNobody becomes newly constrained in 2028.",
       x = "2028 status", y = "2027 status")
save_gg(p7, "07_cap_cup_status_transitions", w = 7, h = 5)

# 8. Final-price change by NCD movement (why the tail is wide) ------------------------
if (!exists("renewal_book_2027")) renewal_book_2027 <- fread("renewal_book_2027.csv")
renewals[renewal_book_2027, on = "PolicyID", NCD_2027 := i.NCDLevel]
renewals[, NCDMove := factor(fcase(NCDLevel < NCD_2027, "NCD down\n(claim in 2027)",
                                   NCDLevel > NCD_2027, "NCD up",
                                   default = "NCD unchanged"),
                             levels = c("NCD down\n(claim in 2027)", "NCD unchanged", "NCD up"))]
move_n <- renewals[, .(N = .N, Med = median(FinalChange)), keyby = NCDMove]
p8 <- ggplot(renewals, aes(NCDMove, FinalChange)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.15, outlier.size = 0.7) +
  geom_hline(yintercept = 0, colour = "grey30", linetype = 2) +
  geom_text(data = move_n, aes(y = -0.45, label = paste0("n=", comma(N), "\nmedian ", percent(Med, accuracy = 0.1))),
            size = 3.2, lineheight = 0.9) +
  scale_y_continuous(labels = percent) +
  labs(title = "Final-price change vs 2027, by NCD movement",
       subtitle = "The cap/cup limits the pre-discount premium only;\nthe customer's own NCD change flows through in full",
       x = NULL, y = "Change in final price")
save_gg(p8, "08_final_price_change_by_ncd_move")

# 9. Loyalty tax: renewal price vs equivalent new customer ----------------------------
renewals[, Status_2028 := factor(Status_2028, levels = status_lvls)]
p9 <- ggplot(renewals, aes(VsNewCustomer, fill = Status_2028)) +
  geom_histogram(binwidth = 0.02, boundary = 0, colour = "white", linewidth = 0.1) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~LoyaltyBand, ncol = 2) +
  scale_x_continuous(labels = percent) +
  scale_y_sqrt(breaks = c(0, 50, 250, 1000, 3000)) +
  scale_fill_manual(values = status_cols) +
  labs(title = "Renewal price vs the price of a new customer with the same risk profile",
       subtitle = "Right of 0 = renewal pays MORE than a new customer after its loyalty discount.\nOnly cupped customers are there.",
       x = "Final price vs new-customer equivalent", y = "Renewals (square-root scale)", fill = "2028 status")
save_gg(p9, "09_loyalty_tax_vs_new_customer", w = 9, h = 6)

# 10. PaymentFrequency: expected claims vs price basis after removal -------------------
p10 <- ggplot(pf_impact, aes(PaymentFrequency, Cost_vs_Priced, fill = Cost_vs_Priced > 1)) +
  geom_col(width = 0.5) +
  geom_hline(yintercept = 1, colour = "grey30") +
  geom_text(aes(label = sprintf("%+.1f%%", 100 * (Cost_vs_Priced - 1))), vjust = -0.5, size = 4) +
  coord_cartesian(ylim = c(0.95, 1.05)) +
  scale_fill_manual(values = c(`FALSE` = "steelblue", `TRUE` = "firebrick"), guide = "none") +
  scale_y_continuous(labels = function(x) sprintf("%+.0f%%", 100 * (x - 1))) +
  labs(title = "Expected claims vs price basis once PaymentFrequency is removed",
       subtitle = sprintf("Monthly payers: expected claims %+.1f%% vs price basis; annual payers: %+.1f%%",
                          100 * (pf_impact[PaymentFrequency == "Monthly", Cost_vs_Priced] - 1),
                          100 * (pf_impact[PaymentFrequency == "Annual", Cost_vs_Priced] - 1)),
       x = NULL, y = "Expected claims (with PaymentFrequency) vs technical price basis")
save_gg(p10, "10_paymentfrequency_cost_vs_price", w = 7, h = 5)

cat("\nSaved 10 graphs to ", graph_dir, "/\n", sep = "")

# 10. Export ------------------------------------------------------------------------
export_2028 <- priced_2028[, .(
  PolicyID, NewBusinessFlag, TechnicalPrice, LoadedPremium, CapCupLoadedPremium_2027,
  RawRateChange, EffectiveRateChange, Capped, Cupped, CapCupLoadedPremium,
  NCDLevel, NCDDiscountDollar, MultiPolicyFlag, MultiPolicyDiscountDollar,
  TenureYears, LoyaltyDiscountDollar, TotalDiscountDollar, DiscountedPremium,
  PaymentFrequency, InstalmentDollar, FinalPrice, AmountPerPayment)]
fwrite(export_2028, "priced_2028.csv")
cat("\nWrote priced_2028.csv (", nrow(export_2028), " policies) and diagnostics in ", t3_out, "/\n", sep = "")