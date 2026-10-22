library(data.table)

# 1. Load Cleaned Data ----------------------------------------------------
setwd("C:/Users/Ray/Documents/ACTL4305")
source("Data Cleaning and EDA.R")


# 2. Additional Data Cleaning / Prep --------------------------------------
data[, YearIndex := Year - 2026]


# 3. Fit Frequency Model --------------------------------------------------
final_freq_model <- glm(
  ClaimNb ~ RiskZone + AnnualKms + GaragingLocation + VehicleUse + 
    PaymentFrequency + factor(ExcessChosen) + factor(NCDLevel),
  family = poisson(link = "log"),
  data   = data
)

# 4. Fit Severity Model ---------------------------------------------------
sev_train <- copy(data[ClaimNb > 0])
sev_train[, AverageClaim := ClaimAmount / ClaimNb]

final_sev_model <- glm(
  AverageClaim ~ VehValueBand + VehicleType + RiskZone + factor(ExcessChosen) + YearIndex,
  family = Gamma(link = "log"),
  weights = ClaimNb,
  data = sev_train
)

# 5. Prepare 2027 Book ----------------------------------------------------
book_2027 <- copy(renewal_book_2027)
book_2027[, `:=`(
  YearIndex    = 1, # 2027 - 2026 = 1
  VehValueBand = cut(VehValue, 
                     breaks = c(-Inf, 10000, 15000, 20000, 30000, 40000, 60000, Inf), 
                     labels = c("<10k", "10-15k", "15-20k", "20-30k", "30-40k", "40-60k", "60k+"))
)]


# 6. Legacy Price of 2026 Premium without NCD -----------------------------
# Driver age entering 2026
book_2027[, DriverAge_2026 := DriverAge - 1]

# Map legacy rating factors
book_2027[, `:=`(
  LegacyZoneFactor = fcase(
    RiskZone == "A_Metro", 1.25,
    RiskZone %in% c("B_InnerRegional", "C_OuterRegional"), 1.00,
    RiskZone %in% c("D_Rural", "E_RemoteRural"), 0.80,
    default = 1.00
  ),
  LegacyVehFactor = fcase(
    VehicleType == "Sports", 1.45,
    VehicleType %in% c("Sedan", "Hatch", "SUV", "Ute"), 1.00,
    default = 1.00
  ),
  LegacyAgeFactor = fcase(
    DriverAge_2026 < 25, 1.35,
    DriverAge_2026 >= 25 & DriverAge_2026 < 65, 1.00,
    DriverAge_2026 >= 65, 1.15,
    default = 1.00
  ),
  LegacyValFactor = (VehValue / 24000) ^ 0.30
)]

# Reconstruct 2026 undiscounted gross base (NCD = 0%, 35% loading, floored at $350)
book_2027[, LegacyGrossBase_2026 := pmax(
  1000 * LegacyZoneFactor * LegacyVehFactor * LegacyAgeFactor * 1.00 * LegacyValFactor * 1.35, 
  350
)]

# Step 6 Checks
cat("Mean 2026 Undiscounted Gross Base : $", round(mean(book_2027$LegacyGrossBase_2026), 2), "\n")
cat("Min / Max 2026 Gross Base         : $", round(min(book_2027$LegacyGrossBase_2026), 2), 
    "/ $", round(max(book_2027$LegacyGrossBase_2026), 2), "\n")


# 7. Compute 2027 TechnicalPrice and LoadedPremium ------------------------

# a. True technical price (expected claim cost for priced_2027.csv)
book_2027[, TechnicalPrice := predict(final_freq_model, book_2027, type = "response") * 
            predict(final_sev_model,  book_2027, type = "response")]

# b. Rebase to NCD = 0 & Annual payment to get the undiscounted base risk
book_base <- copy(book_2027)[, `:=`(
  NCDLevel         = 0L,
  PaymentFrequency = "Annual"
)]

book_2027[, BaseTechnicalPrice := predict(final_freq_model, book_base, type = "response") * 
            predict(final_sev_model,  book_base, type = "response")]

# c. Loaded Premium (List price: Fixed $85, 10% variable, 8% profit)
# For the loaded premium, NCD and payment frequency are set to the baseline to 
# avoid double counting, the TechnicalPrice captures the cost of claims
book_2027[, LoadedPremium := (BaseTechnicalPrice + 85) / 0.82]

cat("Mean Technical Price (True Risk) : $", round(mean(book_2027$TechnicalPrice), 2), "\n")
cat("Mean Base Tech Price (NCD = 0)   : $", round(mean(book_2027$BaseTechnicalPrice), 2), "\n")
cat("Mean Loaded Premium (2027 Base)  : $", round(mean(book_2027$LoadedPremium), 2), "\n")
cat("Min / Max Loaded Premium         : $", round(min(book_2027$LoadedPremium), 2), 
    "/ $", round(max(book_2027$LoadedPremium), 2), "\n")


# 8. Cap/Cups Testing & Application ---------------------------------------

# Distribution histogram with lapse risk thresholds
book_2027[, RawRateChange := (LoadedPremium / LegacyGrossBase_2026) - 1]
raw_pct <- book_2027$RawRateChange * 100
hist(
  raw_pct,
  breaks = 60,
  col    = "steelblue",
  border = "white",
  main   = "Distribution of Raw Rate Changes (2027 vs 2026 Base)",
  xlab   = "Raw Rate Change (%)",
  xlim   = c(-60, 220),
  xaxt   = "n"
)

axis(side = 1, at = seq(-50, 225, by = 25))

abline(v = -15, col = "purple",  lty = 2, lwd = 2)   # Deep cup (-15%)
abline(v = -10, col = "blue",    lty = 2, lwd = 1.5) # Moderate cup (-10%)
abline(v = 0,   col = "gray40",  lty = 2, lwd = 1.5) # 0% no change
abline(v = 5,   col = "green3",  lty = 2, lwd = 2)   # 5% tier
abline(v = 15,  col = "orange",  lty = 2, lwd = 2)   # 15% lapse cliff
abline(v = 30,  col = "red",     lty = 2, lwd = 2)   # 30% severe cliff

legend(
  "topright",
  legend  = c("Cup -15%", "Cup -10%", "0% Base", "5% Tier", "15% Lapse Cliff", "30% Severe Cliff"),
  col     = c("purple", "blue", "gray40", "green3", "orange", "red"),
  lty     = 2,
  lwd     = 2,
  bty     = "n",
  cex     = 0.85
)

# Function to test different cap/cups
apply_tiered_cap_cup <- function(dt,
                                 cap_breaks = c(-Inf, 0.05, 0.15, 0.30, Inf),
                                 cap_values = c(0.05, 0.10, 0.15, 0.20),
                                 cup_breaks = c(-Inf, -0.20, Inf),
                                 cup_values = c(-0.15, -0.10),
                                 verbose    = TRUE) {
  
  # Ensure RawRateChange exists
  if (!"RawRateChange" %in% names(dt)) {
    dt[, RawRateChange := (LoadedPremium / LegacyGrossBase_2026) - 1]
  }
  
  # Assign custom caps and cups using cut()
  dt[, `:=`(
    CapPct = as.numeric(as.character(cut(RawRateChange, breaks = cap_breaks, labels = cap_values, right = TRUE))),
    CupPct = as.numeric(as.character(cut(RawRateChange, breaks = cup_breaks, labels = cup_values, right = TRUE)))
  )]
  
  # Bound rate change and compute CapCupLoadedPremium
  dt[, EffectiveRateChange := pmin(pmax(RawRateChange, CupPct), CapPct)]
  dt[, CapCupLoadedPremium := LegacyGrossBase_2026 * (1 + EffectiveRateChange)]
  
  if (verbose) {
    cat("--------------------------------------------------\n")
    cat("Mean Raw Rate Change       :", round(mean(dt$RawRateChange) * 100, 1), "%\n")
    cat("Mean Effective Rate Change :", round(mean(dt$EffectiveRateChange) * 100, 1), "%\n")
    cat("Mean CapCupLoadedPremium   : $", round(mean(dt$CapCupLoadedPremium), 2), "\n")
    cat("Policies hitting Cap       :", round(mean(dt$RawRateChange >= dt$CapPct) * 100, 1), "%\n")
    cat("Policies hitting Cup       :", round(mean(dt$RawRateChange <= dt$CupPct) * 100, 1), "%\n")
    cat("--------------------------------------------------\n")
  }
  
  return(invisible(dt))
}


# 9. Applying Discounts ---------------------------------------------------
apply_commercial_discounts <- function(dt,
                                       ncd_rates = c("0" = 0.00, "1" = 0.10, "2" = 0.20, 
                                                     "3" = 0.30, "4" = 0.40, "5" = 0.50, "6" = 0.60),
                                       multipolicy_rate = 0.10,
                                       loyalty_per_year = 0.01,
                                       loyalty_max      = 0.10,
                                       compound         = TRUE,
                                       verbose          = TRUE) {
  
  # a. Map discount percentages
  dt[, NCDDiscount := ncd_rates[as.character(NCDLevel)]]
  dt[is.na(NCDDiscount), NCDDiscount := 0]
  
  mp_col <- intersect(c("MultiPolicy", "HasMultiPolicy", "MultiPolicyFlag"), names(dt))[1]
  dt[, MultiPolicyDiscount := if (!is.na(mp_col)) fifelse(get(mp_col) %in% c(1, "1", "Yes", TRUE), multipolicy_rate, 0) else 0]
  
  tenure_col <- intersect(c("Tenure", "CustomerTenure", "PolicyTenure", "LoyaltyYears"), names(dt))[1]
  dt[, LoyaltyDiscount := if (!is.na(tenure_col)) pmin(pmax(get(tenure_col), 0) * loyalty_per_year, loyalty_max) else 0]
  
  # b. Dollar breakdown (sequential cascade if compounded)
  if (compound) {
    dt[, DiscountMultiplier := (1 - NCDDiscount) * (1 - MultiPolicyDiscount) * (1 - LoyaltyDiscount)]
    dt[, DiscountedPremium  := CapCupLoadedPremium * DiscountMultiplier]
    
    # Exact waterfall dollar attribution
    dt[, NCDDiscountDollar         := CapCupLoadedPremium * NCDDiscount]
    dt[, MultiPolicyDiscountDollar := (CapCupLoadedPremium - NCDDiscountDollar) * MultiPolicyDiscount]
    dt[, LoyaltyDiscountDollar     := (CapCupLoadedPremium - NCDDiscountDollar - MultiPolicyDiscountDollar) * LoyaltyDiscount]
  } else {
    dt[, DiscountMultiplier := pmax(0, 1 - (NCDDiscount + MultiPolicyDiscount + LoyaltyDiscount))]
    dt[, DiscountedPremium  := CapCupLoadedPremium * DiscountMultiplier]
    
    dt[, NCDDiscountDollar         := CapCupLoadedPremium * NCDDiscount]
    dt[, MultiPolicyDiscountDollar := CapCupLoadedPremium * MultiPolicyDiscount]
    dt[, LoyaltyDiscountDollar     := CapCupLoadedPremium * LoyaltyDiscount]
  }
  
  dt[, TotalDiscountDollar := CapCupLoadedPremium - DiscountedPremium]
  
  if (verbose) {
    cat("--------------------------------------------------\n")
    cat("Mean Cap/Cup Loaded Premium : $", round(mean(dt$CapCupLoadedPremium), 2), "\n")
    cat("Mean NCD Savings            : -$ ", round(mean(dt$NCDDiscountDollar), 2), "\n")
    cat("Mean Multi-Policy Savings   : -$ ", round(mean(dt$MultiPolicyDiscountDollar), 2), "\n")
    cat("Mean Loyalty Savings        : -$ ", round(mean(dt$LoyaltyDiscountDollar), 2), "\n")
    cat("Mean Total Discount Given   : -$ ", round(mean(dt$TotalDiscountDollar), 2), 
        "(", round(mean(dt$TotalDiscountDollar / dt$CapCupLoadedPremium) * 100, 1), "%)\n")
    cat("Mean Discounted Premium     : $", round(mean(dt$DiscountedPremium), 2), "\n")
    cat("--------------------------------------------------\n")
  }
  
  return(invisible(dt))
}


# 10. Instalment Costs ----------------------------------------------------

apply_instalment_surcharge <- function(dt, 
                                       surcharge_rate = 0.08, # standard 8% surcharge for monthly pay
                                       verbose        = TRUE) {
  
  # a. Identify payment frequency column
  freq_col <- intersect(c("PaymentFrequency", "PayFreq", "PaymentMethod"), names(dt))[1]
  
  if (!is.na(freq_col)) {
    dt[, InstalmentSurchargeRate := fifelse(get(freq_col) %in% c("Monthly", "M", 12), surcharge_rate, 0.0)]
  } else {
    dt[, InstalmentSurchargeRate := 0.0]
  }
  
  # b. Final Premium (annualised total payable)
  dt[, FinalPrice := DiscountedPremium * (1 + InstalmentSurchargeRate)]
  
  # c. Per-period bill amount
  dt[, AmountPerPayment := fifelse(
    InstalmentSurchargeRate > 0, 
    round(FinalPrice / 12, 2), 
    round(FinalPrice, 2)
  )]
  
  # d. Diagnostics
  if (verbose) {
    cat("--------------------------------------------------\n")
    cat("Mean Discounted Premium    : $", round(mean(dt$DiscountedPremium), 2), "\n")
    cat("Mean Final Premium         : $", round(mean(dt$FinalPrice), 2), "\n")
    cat("Monthly Payers (%)         :", round(mean(dt$InstalmentSurchargeRate > 0) * 100, 1), "%\n")
    cat("Mean Monthly Payment       : $", round(mean(dt[InstalmentSurchargeRate > 0]$AmountPerPayment), 2), "/ month\n")
    cat("Min / Max Final Premium    : $", round(min(dt$FinalPrice), 2), 
        "/ $", round(max(dt$FinalPrice), 2), "\n")
    cat("--------------------------------------------------\n")
  }
  
  return(invisible(dt))
}

# 11. Loss Ratio and Portfolio Summary ------------------------------------

test_pricing_scenario <- function(dt,
                                  scenario_name = "Custom",
                                  cap_breaks    = c(-Inf, 0.05, 0.15, 0.30, Inf),
                                  cap_values    = c(0.05, 0.10, 0.12, 0.15),
                                  cup_breaks    = c(-Inf, -0.20, Inf),
                                  cup_values    = c(-0.15, -0.10),
                                  fixed_cost    = 85,
                                  var_expense   = 0.10) {
  
  d <- copy(dt)
  
  # Step A: Apply Cap/Cup
  apply_tiered_cap_cup(d, cap_breaks, cap_values, cup_breaks, cup_values, verbose = FALSE)
  
  # Step B: Apply Discounts
  apply_commercial_discounts(d, verbose = FALSE)
  
  # Step C: Apply Instalment
  apply_instalment_surcharge(d, verbose = FALSE)
  
  # Step D: Portfolio Financial Metrics
  total_claims   <- sum(d$TechnicalPrice)
  total_revenue  <- sum(d$FinalPrice)
  total_expenses <- (nrow(d) * fixed_cost) + (total_revenue * var_expense)
  
  loss_ratio     <- (total_claims / total_revenue) * 100
  expense_ratio  <- (total_expenses / total_revenue) * 100
  combined_ratio <- loss_ratio + expense_ratio
  profit_margin  <- 100 - combined_ratio
  
  # Step E: Full Lapse Risk Tier Breakdown
  pct_0to5   <- mean(d$EffectiveRateChange <= 0.05) * 100
  pct_5to15  <- mean(d$EffectiveRateChange > 0.05 & d$EffectiveRateChange <= 0.15) * 100
  pct_15to30 <- mean(d$EffectiveRateChange > 0.15 & d$EffectiveRateChange <= 0.30) * 100
  pct_gt30   <- mean(d$EffectiveRateChange > 0.30) * 100
  
  data.table(
    Scenario        = scenario_name,
    Mean_Final_Prem = paste0("$", round(mean(d$FinalPrice), 2)),
    Total_Prem_M    = paste0("$", round(total_revenue / 1e6, 2), "M"),
    Loss_Ratio      = paste0(round(loss_ratio, 1), "%"),
    Combined_Ratio  = paste0(round(combined_ratio, 1), "%"),
    Profit_Margin   = paste0(round(profit_margin, 1), "%"),
    Lapse_0_5pct    = paste0(round(pct_0to5, 1), "%"),   # +0% lapse penalty
    Lapse_5_15pct   = paste0(round(pct_5to15, 1), "%"),  # +5% lapse penalty
    Lapse_15_30pct  = paste0(round(pct_15to30, 1), "%"), # +15% lapse penalty
    Lapse_gt30pct   = paste0(round(pct_gt30, 1), "%")   # +35% lapse penalty
  )
}

# 12. Testing Different Cap/Cups ------------------------------------------

rbind(
  # Benchmark: Your previous Scenario B (+15% Max, -15% Cup)
  test_pricing_scenario(
    book_2027,
    scenario_name = "1. Baseline (+15% Cap / -15% Cup)",
    cap_breaks    = c(-Inf, 0.05, 0.15, 0.30, Inf),
    cap_values    = c(0.05, 0.10, 0.12, 0.15),
    cup_breaks    = c(-Inf, -0.20, Inf),
    cup_values    = c(-0.15, -0.10)
  ),
  
  # Strategy A: Boundary Optimisation (Max out 14.9% and 29.9% without hitting >30% cliff)
  test_pricing_scenario(
    book_2027,
    scenario_name = "2. Boundary Optimised (29.9% Max)",
    cap_breaks    = c(-Inf, 0.05, 0.15, 0.30, Inf),
    cap_values    = c(0.05, 0.149, 0.20, 0.299),
    cup_breaks    = c(-Inf, -0.20, Inf),
    cup_values    = c(-0.10, -0.05)
  ),
  
  # Strategy B: Inflation Floor + Boundary Optimisation (Zero rate cuts allowed)
  test_pricing_scenario(
    book_2027,
    scenario_name = "3. Zero-Cut Floor + 29.9% Cap",
    cap_breaks    = c(-Inf, 0.05, 0.15, 0.30, Inf),
    cap_values    = c(0.05, 0.149, 0.22, 0.299),
    cup_breaks    = c(-Inf, Inf),
    cup_values    = c(0.00) # Floor at 0%: no customer gets a price reduction
  ),
  
  # Strategy C: Inflation Indexation (+2% Minimum Floor + 29.9% Cap)
  test_pricing_scenario(
    book_2027,
    scenario_name = "4. +2% Inflation Floor + 29.9% Cap",
    cap_breaks    = c(-Inf, 0.05, 0.15, 0.30, Inf),
    cap_values    = c(0.05, 0.149, 0.24, 0.299),
    cup_breaks    = c(-Inf, Inf),
    cup_values    = c(0.02) # Minimum +2% increase (still in 0-5% tier = 0% lapse)
  ),
  
  # Strategy D: Targeted De-Selection (Pass +45% to extreme toxic risks >60% to trigger lapse)
  test_pricing_scenario(
    book_2027,
    scenario_name = "5. De-Select Worst Risks (+45% on >60%)",
    cap_breaks    = c(-Inf, 0.05, 0.15, 0.30, 0.60, Inf),
    cap_values    = c(0.05, 0.149, 0.22, 0.299, 0.45),
    cup_breaks    = c(-Inf, Inf),
    cup_values    = c(0.00)
  )
)



# 13. Diagnostic of High Loss Ratio Policies ------------------------------

# 1. Run Chosen Strategy
diag_dt <- copy(book_2027)

apply_tiered_cap_cup(
  diag_dt,
  cap_breaks = c(-Inf, 0.05, 0.15, 0.225, 0.30, 0.60,Inf),
  cap_values = c(0.05, 0.10, 0.18, 0.25, 0.35, 0.50),
  cup_breaks = c(-Inf, -0.20, Inf),
  cup_values = c(-0.10, -0.05),
  verbose    = FALSE
)
apply_commercial_discounts(diag_dt, verbose = FALSE)
apply_instalment_surcharge(diag_dt, verbose = FALSE)

# 2. Compute Policy-Level Loss Ratio
diag_dt[, PolicyLossRatio := (TechnicalPrice / FinalPrice) * 100]

# -------------------------------------------------------------------------
# A. Overall Loss Ratio Summary Statistics
# -------------------------------------------------------------------------
cat("=== POLICY LOSS RATIO SUMMARY ===\n")
cat("Mean Loss Ratio           :", round(mean(diag_dt$PolicyLossRatio), 1), "%\n")
cat("Median Loss Ratio         :", round(median(diag_dt$PolicyLossRatio), 1), "%\n")
cat("Policies with LR > 100%   :", round(mean(diag_dt$PolicyLossRatio > 100) * 100, 1), "%\n")
cat("Policies with LR > 150%   :", round(mean(diag_dt$PolicyLossRatio > 150) * 100, 1), "%\n")
cat("Policies with LR < 70%    :", round(mean(diag_dt$PolicyLossRatio < 70) * 100, 1), "%\n\n")

# -------------------------------------------------------------------------
# B. Frequency Table by Loss Ratio Bands
# -------------------------------------------------------------------------
lr_bands <- diag_dt[, .(
  Count = .N,
  Pct_of_Book = paste0(round(.N / nrow(diag_dt) * 100, 1), "%"),
  Mean_TechPrice = paste0("$", round(mean(TechnicalPrice), 0)),
  Mean_FinalPrem = paste0("$", round(mean(FinalPrice), 0))
), keyby = .(
  Band = cut(
    PolicyLossRatio,
    breaks = c(0, 60, 80, 100, 125, 150, Inf),
    labels = c("Very Profitable (<60%)", "Target Range (60-80%)", "Marginal (80-100%)",
               "Underwriting Loss (100-125%)", "Severe Loss (125-150%)", "Insolvent (>150%)")
  )
)]
print(lr_bands)

# -------------------------------------------------------------------------
# C. Plot the Loss Ratio Distribution
# -------------------------------------------------------------------------
hist(
  pmin(diag_dt$PolicyLossRatio, 250), # cap at 250% for plot visibility
  breaks = 50,
  col    = "salmon",
  border = "white",
  main   = "Distribution of Individual Policy Loss Ratios (2027)",
  xlab   = "Loss Ratio (%) [Capped at 250% for Display]",
  xaxt   = "n"
)
axis(side = 1, at = seq(0, 250, by = 25))

# Reference lines
abline(v = 70,  col = "darkgreen", lty = 2, lwd = 2) # Target Loss Ratio (~70%)
abline(v = 100, col = "red",       lty = 1, lwd = 2.5) # Break-even line (Claims = Premium)
abline(v = median(diag_dt$PolicyLossRatio), col = "blue", lty = 2, lwd = 2) # Median

legend(
  "topright",
  legend = c("Target (~70%)", "Break-Even (100%)", paste0("Median (", round(median(diag_dt$PolicyLossRatio), 1), "%)")),
  col    = c("darkgreen", "red", "blue"),
  lty    = c(2, 1, 2),
  lwd    = c(2, 2.5, 2),
  bty    = "n"
)

# -------------------------------------------------------------------------
# D. Key Driver Attribution Tables
# -------------------------------------------------------------------------

# 1. Driver 1: Impact of Capping (Policies hitting cap vs not)
diag_dt[, HitCap := RawRateChange >= CapPct]
cat("\n=== LOSS RATIO BY CAP STATUS ===\n")
print(diag_dt[, .(
  Count = .N,
  Pct = paste0(round(.N / nrow(diag_dt) * 100, 1), "%"),
  Mean_LR = paste0(round(mean(PolicyLossRatio), 1), "%"),
  Pct_Over_100 = paste0(round(mean(PolicyLossRatio > 100) * 100, 1), "%")
), by = HitCap])

# 2. Driver 2: Impact of NCD Level
cat("\n=== LOSS RATIO BY NCD LEVEL ===\n")
print(diag_dt[, .(
  Count = .N,
  Mean_TechPrice = paste0("$", round(mean(TechnicalPrice), 0)),
  Mean_FinalPrem = paste0("$", round(mean(FinalPrice), 0)),
  Mean_LR = paste0(round(mean(PolicyLossRatio), 1), "%"),
  Pct_Over_100 = paste0(round(mean(PolicyLossRatio > 100) * 100, 1), "%")
), keyby = NCDLevel])

# 3. Driver 3: Impact of Non-Risk Discounts (Multi-Policy & Loyalty)
diag_dt[, Discount_Bucket := fcase(
  MultiPolicyDiscount > 0 & LoyaltyDiscount > 0.05, "Multi-Policy + High Loyalty",
  MultiPolicyDiscount > 0, "Multi-Policy Only",
  LoyaltyDiscount > 0.05, "High Loyalty Only",
  default = "Standard"
)]
cat("\n=== LOSS RATIO BY COMMERCIAL DISCOUNT STACKING ===\n")
print(diag_dt[, .(
  Count = .N,
  Mean_LR = paste0(round(mean(PolicyLossRatio), 1), "%"),
  Pct_Over_100 = paste0(round(mean(PolicyLossRatio > 100) * 100, 1), "%")
), by = Discount_Bucket])


diag_dt[, CapBand := cut(
  RawRateChange,
  breaks = c(-Inf, 0.05, 0.15, 0.225, 0.30, 0.60, Inf),
  labels = c(
    "1. <= 5% (Cap 5%)",
    "2. 5% - 15% (Cap 10%)",
    "3. 15% - 22.5% (Cap 18%)",
    "4. 22.5% - 30% (Cap 25%)",
    "5. 30% - 60% (Cap 35%)",
    "6. > 60% (Cap 50%)"
  ),
  right = TRUE
)]

cat("\n=== POLICY DISTRIBUTION ACROSS CAP TIERS ===\n")
cap_tier_summary <- diag_dt[, .(
  Total_Policies   = .N,
  Pct_of_Book      = paste0(round(.N / nrow(diag_dt) * 100, 1), "%"),
  Policies_Capped  = sum(RawRateChange >= CapPct),
  Pct_Capped       = paste0(round(mean(RawRateChange >= CapPct) * 100, 1), "%"),
  Mean_Raw_Chg     = paste0(round(mean(RawRateChange) * 100, 1), "%"),
  Mean_Eff_Chg     = paste0(round(mean(EffectiveRateChange) * 100, 1), "%"),
  Mean_TechPrice   = paste0("$", round(mean(TechnicalPrice), 0)),
  Mean_FinalPrem   = paste0("$", round(mean(FinalPrice), 0)),
  Mean_LR          = paste0(round(mean(PolicyLossRatio), 1), "%"),
  Pct_Over_100     = paste0(round(mean(PolicyLossRatio > 100) * 100, 1), "%")
), keyby = CapBand]

print(cap_tier_summary)

# 14. Final Price for 2027 Book -------------------------------------------

apply_tiered_cap_cup(
  book_2027,
  cap_breaks = c(-Inf, 0.05, 0.15, 0.225, 0.30, 0.60, Inf),
  cap_values = c(0.05, 0.10, 0.18,  0.25,  0.35, 0.50),
  cup_breaks = c(-Inf, -0.20, Inf),
  cup_values = c(-0.10, -0.05),
  verbose    = TRUE
)
apply_commercial_discounts(book_2027)
apply_instalment_surcharge(book_2027)
