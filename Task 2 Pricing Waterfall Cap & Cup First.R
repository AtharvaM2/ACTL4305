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
book_2027[, RawRateChangePct := (LoadedPremium / LegacyGrossBase_2026) - 1]
book_2027[, RawRateChangeDollarAmt := LoadedPremium - LegacyGrossBase_2026]
raw_pct <- book_2027$RawRateChangePct * 100
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
  
  # Ensure RawRateChangePct exists
  if (!"RawRateChangePct" %in% names(dt)) {
    dt[, RawRateChangePct := (LoadedPremium / LegacyGrossBase_2026) - 1]
  }
  
  # Assign custom caps and cups
  dt[, `:=`(
    CapPct = as.numeric(as.character(cut(RawRateChangePct, breaks = cap_breaks, labels = cap_values, right = TRUE))),
    CupPct = as.numeric(as.character(cut(RawRateChangePct, breaks = cup_breaks, labels = cup_values, right = TRUE)))
  )]
  
  # Effective changes and dollar impacts
  dt[, EffectiveRateChangePct := pmin(pmax(RawRateChangePct, CupPct), CapPct)]
  dt[, CapCupLoadedPremium := LegacyGrossBase_2026 * (1 + EffectiveRateChangePct)]
  
  # Dollar amount tracking
  dt[, RawRateChangeDollarAmt       := LoadedPremium - LegacyGrossBase_2026]
  dt[, EffectiveRateChangeDollarAmt := CapCupLoadedPremium - LegacyGrossBase_2026]
  dt[, CapCupAdjustmentDollarAmt    := CapCupLoadedPremium - LoadedPremium] # Captures both cap and cup dollar impacts
  
  if (verbose) {
    cat("--------------------------------------------------\n")
    cat("Mean Raw Rate Change       :", round(mean(dt$RawRateChangePct) * 100, 1), "%\n")
    cat("Mean Effective Rate Change :", round(mean(dt$EffectiveRateChangePct) * 100, 1), "%\n")
    cat("Mean CapCupLoadedPremium   : $", round(mean(dt$CapCupLoadedPremium), 2), "\n")
    cat("Policies hitting Cap       :", round(mean(dt$RawRateChangePct >= dt$CapPct) * 100, 1), "%\n")
    cat("Policies hitting Cup       :", round(mean(dt$RawRateChangePct <= dt$CupPct) * 100, 1), "%\n")
    cat("--------------------------------------------------\n")
  }
  
  return(invisible(dt))
}


# 9. Applying Discounts ---------------------------------------------------
apply_commercial_discounts <- function(dt,
                                       ncd_rates = c("0" = 0.00, "1" = 0.10, "2" = 0.20, 
                                                     "3" = 0.30, "4" = 0.40, "5" = 0.50, "6" = 0.60),
                                       multipolicy_rate = 0.10,
                                       compound         = TRUE,
                                       verbose          = TRUE) {
  
  # a. Map NCD Discount %
  dt[, NCDDiscountPct := ncd_rates[as.character(NCDLevel)]]
  dt[is.na(NCDDiscountPct), NCDDiscountPct := 0]
  
  # b. Map Multi-Policy Discount %
  mp_col <- intersect(c("MultiPolicyFlag", "MultiPolicy", "HasMultiPolicy"), names(dt))[1]
  dt[, MultiPolicyDiscountPct := if (!is.na(mp_col)) fifelse(get(mp_col) %in% c(1, "1", "Yes", TRUE), multipolicy_rate, 0) else 0]
  
  # c. Map Loyalty Discount %
  tenure_col <- intersect(c("TenureYears", "Tenure", "CustomerTenure"), names(dt))[1]
  dt[, LoyaltyDiscountPct := fcase(
    get(tenure_col) >= 10, 0.10,
    get(tenure_col) >= 6,  0.06,
    get(tenure_col) >= 3,  0.03,
    default = 0.00
  )]
  
  # d. Sequential Waterfall Dollar Breakdown
  if (compound) {
    dt[, DiscountMultiplier := (1 - NCDDiscountPct) * (1 - MultiPolicyDiscountPct) * (1 - LoyaltyDiscountPct)]
    dt[, DiscountedPremium  := CapCupLoadedPremium * DiscountMultiplier]
    
    dt[, NCDDiscountDollarAmt         := CapCupLoadedPremium * NCDDiscountPct]
    dt[, MultiPolicyDiscountDollarAmt := (CapCupLoadedPremium - NCDDiscountDollarAmt) * MultiPolicyDiscountPct]
    dt[, LoyaltyDiscountDollarAmt     := (CapCupLoadedPremium - NCDDiscountDollarAmt - MultiPolicyDiscountDollarAmt) * LoyaltyDiscountPct]
  } else {
    dt[, DiscountMultiplier := pmax(0, 1 - (NCDDiscountPct + MultiPolicyDiscountPct + LoyaltyDiscountPct))]
    dt[, DiscountedPremium  := CapCupLoadedPremium * DiscountMultiplier]
    
    dt[, NCDDiscountDollarAmt         := CapCupLoadedPremium * NCDDiscountPct]
    dt[, MultiPolicyDiscountDollarAmt := CapCupLoadedPremium * MultiPolicyDiscountPct]
    dt[, LoyaltyDiscountDollarAmt     := CapCupLoadedPremium * LoyaltyDiscountPct]
  }
  
  # e. Total Discount Totals (% and $)
  dt[, TotalDiscountDollarAmt := CapCupLoadedPremium - DiscountedPremium]
  dt[, TotalDiscountPct       := fifelse(CapCupLoadedPremium > 0, TotalDiscountDollarAmt / CapCupLoadedPremium, 0)]
  
  if (verbose) {
    cat("--------------------------------------------------\n")
    cat("Mean Cap/Cup Loaded Premium : $", round(mean(dt$CapCupLoadedPremium), 2), "\n")
    cat("Mean NCD Savings            : -$ ", round(mean(dt$NCDDiscountDollarAmt), 2), "\n")
    cat("Mean Multi-Policy Savings   : -$ ", round(mean(dt$MultiPolicyDiscountDollarAmt), 2), "\n")
    cat("Mean Loyalty Savings        : -$ ", round(mean(dt$LoyaltyDiscountDollarAmt), 2), "\n")
    cat("Mean Total Discount Given   : -$ ", round(mean(dt$TotalDiscountDollarAmt), 2), 
        "(", round(mean(dt$TotalDiscountPct) * 100, 1), "%)\n")
    cat("Mean Discounted Premium     : $", round(mean(dt$DiscountedPremium), 2), "\n")
    cat("--------------------------------------------------\n")
  }
  
  return(invisible(dt))
}

# 10. Instalment Costs ----------------------------------------------------

apply_instalment_surcharge <- function(dt, 
                                       surcharge_rate = 0.03, # 3% surcharge for monthly pay
                                       verbose        = TRUE) {
  
  # a. Identify payment frequency column
  freq_col <- intersect(c("PaymentFrequency", "PayFreq", "PaymentMethod"), names(dt))[1]
  
  if (!is.na(freq_col)) {
    dt[, InstalmentSurchargePct := fifelse(get(freq_col) %in% c("Monthly", "M", 12), surcharge_rate, 0.0)]
  } else {
    dt[, InstalmentSurchargePct := 0.0]
  }
  
  # b. Dollar Surcharge and Final Price
  dt[, InstalmentSurchargeDollarAmt := round(DiscountedPremium * InstalmentSurchargePct, 2)]
  dt[, FinalPrice                   := DiscountedPremium + InstalmentSurchargeDollarAmt]
  
  # c. Per-period bill amount
  dt[, AmountPerPayment := fifelse(
    InstalmentSurchargePct > 0, 
    round(FinalPrice / 12, 2), 
    round(FinalPrice, 2)
  )]
  
  # d. Diagnostics
  if (verbose) {
    cat("--------------------------------------------------\n")
    cat("Mean Discounted Premium    : $", round(mean(dt$DiscountedPremium), 2), "\n")
    cat("Mean Instalment Fee ($)    : $", round(mean(dt$InstalmentSurchargeDollarAmt), 2), "\n")
    cat("Mean Final Premium         : $", round(mean(dt$FinalPrice), 2), "\n")
    cat("Monthly Payers (%)         :", round(mean(dt$InstalmentSurchargePct > 0) * 100, 1), "%\n")
    cat("Mean Monthly Payment       : $", round(mean(dt[InstalmentSurchargePct > 0]$AmountPerPayment), 2), "/ month\n")
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
  pct_0to5   <- mean(d$EffectiveRateChangePct <= 0.05) * 100
  pct_5to15  <- mean(d$EffectiveRateChangePct > 0.05 & d$EffectiveRateChangePct <= 0.15) * 100
  pct_15to30 <- mean(d$EffectiveRateChangePct > 0.15 & d$EffectiveRateChangePct <= 0.30) * 100
  pct_gt30   <- mean(d$EffectiveRateChangePct > 0.30) * 100
  
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
diag_dt[, HitCap := RawRateChangePct >= CapPct]
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
  MultiPolicyDiscountPct > 0 & LoyaltyDiscountPct > 0.05, "Multi-Policy + High Loyalty",
  MultiPolicyDiscountPct > 0, "Multi-Policy Only",
  LoyaltyDiscountPct > 0.05, "High Loyalty Only",
  default = "Standard"
)]
cat("\n=== LOSS RATIO BY COMMERCIAL DISCOUNT STACKING ===\n")
print(diag_dt[, .(
  Count = .N,
  Mean_LR = paste0(round(mean(PolicyLossRatio), 1), "%"),
  Pct_Over_100 = paste0(round(mean(PolicyLossRatio > 100) * 100, 1), "%")
), by = Discount_Bucket])


diag_dt[, CapBand := cut(
  RawRateChangePct,
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
  Policies_Capped  = sum(RawRateChangePct >= CapPct),
  Pct_Capped       = paste0(round(mean(RawRateChangePct >= CapPct) * 100, 1), "%"),
  Mean_Raw_Chg     = paste0(round(mean(RawRateChangePct) * 100, 1), "%"),
  Mean_Eff_Chg     = paste0(round(mean(EffectiveRateChangePct) * 100, 1), "%"),
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
mean(book_2027$FinalPrice)


# 15. Order columns -------------------------------------------------------

setcolorder(book_2027, c(
  # --- Raw Policy Inputs ---
  "PolicyID",                     # Raw input
  "DriverAge", "VehValue", "VehValueBand", "VehicleType", "VehicleUse",
  "GaragingLocation", "AnnualKms", "RiskZone", "ExcessChosen",
  "TenureYears", "NCDLevel", "MultiPolicyFlag", "PaymentFrequency",
  "Premium_2026",                 # Raw 2026 billed premium (legacy discounted)
  "YearIndex",                    # Derived: Year - 2026
  
  # --- Stage 0: Risk Pricing ---
  "TechnicalPrice",               # Derived: predict(Freq) * predict(Sev) [True claim cost]
  "BaseTechnicalPrice",           # Derived: predict(Freq | NCD=0, Ann) * predict(Sev) [Base risk]
  
  # --- Stage 1: Loaded Shelf Price ---
  "LoadedPremium",                # Derived: (BaseTechnicalPrice + 85) / 0.82
  
  # --- Stage 2: 2026 Baseline & Capping / Collaring ---
  "DriverAge_2026",               # Derived: DriverAge - 1
  "LegacyZoneFactor", "LegacyVehFactor", "LegacyAgeFactor", "LegacyValFactor", # Legacy rating factors
  "LegacyGrossBase_2026",         # Derived: max(1000 * LegacyFactors * 1.35, 350)
  "RawRateChangePct",             # Derived: (LoadedPremium / LegacyGrossBase_2026) - 1
  "RawRateChangeDollarAmt",       # Derived: LoadedPremium - LegacyGrossBase_2026
  "CapPct",                       # Derived: cut(RawRateChangePct) upper bound
  "CupPct",                       # Derived: cut(RawRateChangePct) lower bound
  "EffectiveRateChangePct",       # Derived: pmin(pmax(RawRateChangePct, CupPct), CapPct)
  "EffectiveRateChangeDollarAmt", # Derived: CapCupLoadedPremium - LegacyGrossBase_2026
  "CapCupAdjustmentDollarAmt",    # Derived: CapCupLoadedPremium - LoadedPremium (cap/cup impact)
  "CapCupLoadedPremium",          # Derived: LegacyGrossBase_2026 * (1 + EffectiveRateChangePct)
  
  # --- Stage 3: Commercial Discounts ---
  "NCDDiscountPct",               # Derived: NCDLevel * 0.10
  "NCDDiscountDollarAmt",         # Derived: CapCupLoadedPremium * NCDDiscountPct
  "MultiPolicyDiscountPct",       # Derived: fifelse(MultiPolicyFlag == 1, 0.10, 0)
  "MultiPolicyDiscountDollarAmt", # Derived: (CapCupLoadedPremium - NCDDiscountDollarAmt) * MultiPolicyDiscountPct
  "LoyaltyDiscountPct",           # Derived: fcase(TenureYears >= 10, 0.10, >= 6, 0.06, >= 3, 0.03, 0)
  "LoyaltyDiscountDollarAmt",     # Derived: (BalanceAfterMulti) * LoyaltyDiscountPct
  "TotalDiscountDollarAmt",       # Derived: CapCupLoadedPremium - DiscountedPremium
  "TotalDiscountPct",             # Derived: TotalDiscountDollarAmt / CapCupLoadedPremium
  "DiscountedPremium",            # Derived: CapCupLoadedPremium * (1 - NCD) * (1 - MP) * (1 - Loyal)
  
  # --- Stage 4: Surcharge & Final Price ---
  "InstalmentSurchargePct",       # Derived: fifelse(PaymentFrequency == "Monthly", 0.03, 0)
  "InstalmentSurchargeDollarAmt", # Derived: round(DiscountedPremium * InstalmentSurchargePct, 2)
  "FinalPrice",                   # Derived: DiscountedPremium + InstalmentSurchargeDollarAmt
  "AmountPerPayment"              # Derived: fifelse(Monthly, FinalPrice / 12, FinalPrice)
))


# 16a. Extract Single Policy Details to check correct waterfall app --------

# Inspects any policy end-to-end to verify waterfall mechanics or extract
# case-study figures for Task 4.

inspect_policy <- function(dt, id) {
  p <- dt[PolicyID == id]
  if (nrow(p) == 0) stop("PolicyID not found.")
  
  # Math integrity check for this specific policy
  disc_bal_ok  <- abs((p$CapCupLoadedPremium - (p$NCDDiscountDollarAmt + 
                                                  p$MultiPolicyDiscountDollarAmt + 
                                                  p$LoyaltyDiscountDollarAmt)) - p$DiscountedPremium) < 1e-4
  final_bal_ok <- abs((p$DiscountedPremium + p$InstalmentSurchargeDollarAmt) - p$FinalPrice) < 1e-4
  
  status_msg <- if (disc_bal_ok && final_bal_ok) "PASS (Discounts add up)" else "FAIL (Discounts not adding up)"
  
  data.frame(
    Stage = c(
      "0. Pure Risk Cost (Task 1)",
      "0. Base Risk (NCD=0, Annual)",
      "1. Loaded Gross Shelf Price",
      "2. 2026 Gross Baseline",
      "2. Raw Rate Movement (%)",
      "2. Effective Rate Movement (%)",
      "2. Cap/Cup Adjustment ($)",
      "2. Capped Shelf Base",
      "3. NCD Discount ($)",
      "3. Multi-Policy Discount ($)",
      "3. Loyalty Discount ($)",
      "3. Discounted Annual Premium",
      "4. Instalment Surcharge ($)",
      "4. Final Billed Premium",
      "--- AUDIT CHECK ---"
    ),
    Variable = c(
      "TechnicalPrice",
      "BaseTechnicalPrice",
      "LoadedPremium",
      "LegacyGrossBase_2026",
      "RawRateChangePct",
      "EffectiveRateChangePct",
      "CapCupAdjustmentDollarAmt",
      "CapCupLoadedPremium",
      "NCDDiscountDollarAmt (Pct)",
      "MultiPolicyDiscountDollarAmt (Pct)",
      "LoyaltyDiscountDollarAmt (Pct)",
      "DiscountedPremium",
      "InstalmentSurchargeDollarAmt (Pct)",
      "FinalPrice",
      "[Integrity Check]"
    ),
    Amount = c(
      sprintf("$%.2f", p$TechnicalPrice),
      sprintf("$%.2f", p$BaseTechnicalPrice),
      sprintf("$%.2f", p$LoadedPremium),
      sprintf("$%.2f", p$LegacyGrossBase_2026),
      sprintf("%+.2f%%", p$RawRateChangePct * 100),
      sprintf("%+.2f%%", p$EffectiveRateChangePct * 100),
      sprintf("%+.2f", p$CapCupAdjustmentDollarAmt),
      sprintf("$%.2f", p$CapCupLoadedPremium),
      sprintf("-$%.2f (%d%%)", p$NCDDiscountDollarAmt, round(p$NCDDiscountPct * 100)),
      sprintf("-$%.2f (%d%%)", p$MultiPolicyDiscountDollarAmt, round(p$MultiPolicyDiscountPct * 100)),
      sprintf("-$%.2f (%d%%)", p$LoyaltyDiscountDollarAmt, round(p$LoyaltyDiscountPct * 100)),
      sprintf("$%.2f", p$DiscountedPremium),
      sprintf("+$%.2f (%d%%)", p$InstalmentSurchargeDollarAmt, round(p$InstalmentSurchargePct * 100)),
      sprintf("$%.2f", p$FinalPrice),
      status_msg
    ),
    stringsAsFactors = FALSE
  )
}

# Spot-check policy
print(inspect_policy(book_2027, book_2027$PolicyID[37]))


# 16b. Testing all policies across the portfolio --------------------------
# TEST 1: Check that the final premiums, discounts and technical prices line up
# Verifies that:
#   (1) Capped Base - All 3 Discount Dollars == DiscountedPremium
#   (2) DiscountedPremium + Surcharge Dollar == FinalPrice

# 1. Total discount dollars subtracted from capped shelf base
reconciled_discounted_prem <- book_2027$CapCupLoadedPremium - 
  (book_2027$NCDDiscountDollarAmt + 
     book_2027$MultiPolicyDiscountDollarAmt + 
     book_2027$LoyaltyDiscountDollarAmt)

# 2. Maximum discrepancy across all 17,200 policies
max_discount_leak <- max(abs(book_2027$DiscountedPremium - reconciled_discounted_prem))
max_final_leak    <- max(abs(book_2027$FinalPrice - (book_2027$DiscountedPremium + book_2027$InstalmentSurchargeDollarAmt)))

# Assert exact dollar balance
stopifnot(max_discount_leak < 1e-4)
stopifnot(max_final_leak < 1e-4)

cat(sprintf("[PASS] Test 1: Dollar Conservation verified across 100%% of policies (Max error = $%.6f).\n", 
            max(max_discount_leak, max_final_leak)))


# 16c. Combinations test - ensure promises are delivered ----------------

# To prove all discounts work together across every possible customer type:
# 1. We test every combination of NCD (0-6), Multi-Policy (0/1), and Loyalty (0%, 3%, 6%, 10%).
#    This forms 7 x 2 x 4 = 56 distinct customer archetypes.
# 2. For each policy, we calculate the delivered percentage for each discount against
#    its active balance, as well as the combined total discount percentage received.
# 3. We group by all 3 discount factors simultaneously and display the actual promised
#    vs delivered percentages side-by-side so we can manually review each combination.
# 4. We run stopifnot() across all 17,200 rows to prove that no single policyholder
#    in any combination experienced any discount loss.

# 1. Calculate active balance entering each discount stage
bal_after_ncd   <- book_2027$CapCupLoadedPremium - book_2027$NCDDiscountDollarAmt
bal_after_multi <- bal_after_ncd - book_2027$MultiPolicyDiscountDollarAmt


# 2. Reverse-engineer the empirical % each customer actually received
# We calculate the delivered rate for each individual discount plus the total combined discount
book_2027[, `:=`(
  DeliveredNCDPct   = NCDDiscountDollarAmt / CapCupLoadedPremium,
  DeliveredMultiPct = fifelse(bal_after_ncd > 0, MultiPolicyDiscountDollarAmt / bal_after_ncd, 0),
  DeliveredLoyalPct = fifelse(bal_after_multi > 0, LoyaltyDiscountDollarAmt / bal_after_multi, 0),
  DeliveredTotalPct = TotalDiscountDollarAmt / CapCupLoadedPremium
)]


# 3. Aggregate across all 56 combinations to inspect promised vs delivered
combo_audit <- book_2027[, .(
  Policies        = .N,
  Promised_NCD    = paste0(round(first(NCDDiscountPct) * 100), "%"),
  Delivered_NCD   = paste0(round(mean(DeliveredNCDPct) * 100, 1), "%"),
  Promised_Multi  = paste0(round(first(MultiPolicyDiscountPct) * 100), "%"),
  Delivered_Multi = paste0(round(mean(DeliveredMultiPct) * 100, 1), "%"),
  Promised_Loyal  = paste0(round(first(LoyaltyDiscountPct) * 100), "%"),
  Delivered_Loyal = paste0(round(mean(DeliveredLoyalPct) * 100, 1), "%"),
  Promised_Total  = paste0(round((1 - (1 - first(NCDDiscountPct)) * (1 - first(MultiPolicyDiscountPct)) * (1 - first(LoyaltyDiscountPct))) * 100, 1), "%"),
  Delivered_Total = paste0(round(mean(DeliveredTotalPct) * 100, 1), "%")
), keyby = .(NCDLevel, MultiPolicyFlag, LoyaltyDiscountPct)]

cat("\n--- 56-ARCHETYPE COMBINATIONS AUDIT TABLE ---\n")
print(combo_audit, nrows = 60)

# 4. Programmatic row-by-row assertion across all 17,200 policies
# Checks every single individual policy to ensure zero discount discrepancy
max_ncd_err   <- max(abs(book_2027$DeliveredNCDPct - book_2027$NCDDiscountPct))
max_multi_err <- max(abs(book_2027$DeliveredMultiPct - book_2027$MultiPolicyDiscountPct))
max_loyal_err <- max(abs(book_2027$DeliveredLoyalPct - book_2027$LoyaltyDiscountPct))

promised_total_pct <- 1 - (1 - book_2027$NCDDiscountPct) * (1 - book_2027$MultiPolicyDiscountPct) * (1 - book_2027$LoyaltyDiscountPct)
max_total_err      <- max(abs(book_2027$DeliveredTotalPct - promised_total_pct))

stopifnot(max_ncd_err < 1e-6)
stopifnot(max_multi_err < 1e-6)
stopifnot(max_loyal_err < 1e-6)
stopifnot(max_total_err < 1e-6)

cat("\n[PASS] All 56 combinations delivered 100% of promised discounts across all 17,200 policies.\n")
cat(sprintf("       Worst-case portfolio error: NCD = %.1e, Multi = %.1e, Loyal = %.1e, Total = %.1e\n",
            max_ncd_err, max_multi_err, max_loyal_err, max_total_err))


# 16d. Boundary Invariance Test -------------------------------------------

# To prove zero discount erosion across price boundaries:
# 1. We classify every policy into one of 3 regulatory boundary states:
#    - Cap Binding: Customer rate increase was limited by the cap ceiling.
#    - Collar Binding: Customer rate decrease was limited by the collar floor.
#    - Unconstrained: Customer rate change fell inside the corridor.
# 2. We compare promised vs delivered discount rates across these 3 groups.
# 3. We run stopifnot() to verify that delivered rates match promised schedules
#    regardless of whether the customer's price was capped or collared.


# STEP 1: CLASSIFY POLICIES BY REGULATORY BOUNDARY STATE
book_2027[, BoundaryState := fcase(
  RawRateChangePct >= CapPct, "Cap Binding",
  RawRateChangePct <= CupPct, "Collar Binding",
  default                   = "Unconstrained"
)]


# STEP 2: GENERATE THE ASIC AUDIT SUMMARY TABLE
# Note: DeliveredNCDPct, DeliveredMultiPct, and DeliveredLoyalPct were derived in 16c
boundary_audit <- book_2027[, .(
  Policies        = .N,
  Pct_Of_Book     = paste0(round(.N / nrow(book_2027) * 100, 1), "%"),
  Mean_Cap_Adj    = paste0("$", round(mean(CapCupAdjustmentDollarAmt), 2)),
  Promised_NCD    = paste0(round(mean(NCDDiscountPct) * 100, 1), "%"),
  Delivered_NCD   = paste0(round(mean(DeliveredNCDPct) * 100, 1), "%"),
  Promised_Multi  = paste0(round(mean(MultiPolicyDiscountPct) * 100, 1), "%"),
  Delivered_Multi = paste0(round(mean(DeliveredMultiPct) * 100, 1), "%"),
  Promised_Loyal  = paste0(round(mean(LoyaltyDiscountPct) * 100, 1), "%"),
  Delivered_Loyal = paste0(round(mean(DeliveredLoyalPct) * 100, 1), "%")
), keyby = BoundaryState]

cat("\n--- ASIC REP 765 BOUNDARY INVARIANCE AUDIT ---\n")
print(boundary_audit)


# STEP 3: PROGRAMMATIC INTEGRITY ASSERTION
# Assert that delivered equals promised on average within every single boundary cohort
stopifnot(all(boundary_audit$Promised_NCD   == boundary_audit$Delivered_NCD))
stopifnot(all(boundary_audit$Promised_Multi == boundary_audit$Delivered_Multi))
stopifnot(all(boundary_audit$Promised_Loyal == boundary_audit$Delivered_Loyal))

cat("\n[PASS] Test 3: Zero discount erosion verified across all boundary states (Cap, Collar, Unconstrained).\n")


# 17. Waterfall Table and Chart -------------------------------------------
library(ggplot2)
library(scales)

# A. Aggregate portfolio flows ($M)
n_policies       <- nrow(book_2027)
tot_base_risk    <- sum(book_2027$BaseTechnicalPrice) / 1e6
tot_expenses     <- (sum(book_2027$LoadedPremium) - sum(book_2027$BaseTechnicalPrice)) / 1e6
tot_loaded_gross <- sum(book_2027$LoadedPremium) / 1e6
tot_capcup_adj   <- sum(book_2027$CapCupAdjustmentDollarAmt) / 1e6
tot_ncd_disc     <- -sum(book_2027$NCDDiscountDollarAmt) / 1e6
tot_multi_disc   <- -sum(book_2027$MultiPolicyDiscountDollarAmt) / 1e6
tot_loyal_disc   <- -sum(book_2027$LoyaltyDiscountDollarAmt) / 1e6
tot_surcharge    <- sum(book_2027$InstalmentSurchargeDollarAmt) / 1e6
tot_final_rev    <- sum(book_2027$FinalPrice) / 1e6

waterfall_summary <- data.frame(
  Stage = c(
    "1. Base Pure Risk (NCD=0, Ann)",
    "2. Expense & Profit Loading",
    "3. Gross Loaded Premium",
    "4. Rate Smoothing (Caps & Collars)",
    "5. NCD Discounts",
    "6. Multi-Policy Discounts",
    "7. Loyalty Discounts",
    "8. Instalment Surcharges (Monthly)",
    "9. Final Billed Revenue"
  ),
  Total_M = c(
    tot_base_risk,
    tot_expenses,
    tot_loaded_gross,
    tot_capcup_adj,
    tot_ncd_disc,
    tot_multi_disc,
    tot_loyal_disc,
    tot_surcharge,
    tot_final_rev
  ),
  Pct_Of_Gross_Loaded = c(
    paste0(round(tot_base_risk / tot_loaded_gross * 100, 1), "%"),
    paste0(round(tot_expenses / tot_loaded_gross * 100, 1), "%"),
    "100.0%",
    paste0(round(tot_capcup_adj / tot_loaded_gross * 100, 1), "%"),
    paste0(round(tot_ncd_disc / tot_loaded_gross * 100, 1), "%"),
    paste0(round(tot_multi_disc / tot_loaded_gross * 100, 1), "%"),
    paste0(round(tot_loyal_disc / tot_loaded_gross * 100, 1), "%"),
    paste0(round(tot_surcharge / tot_loaded_gross * 100, 1), "%"),
    paste0(round(tot_final_rev / tot_loaded_gross * 100, 1), "%")
  )
)

print(waterfall_summary)


# B. Structure data and compute floating bar bounds
wf_data <- data.frame(
  step = factor(
    c("Gross Loaded", "Cap/Collar Adj", "NCD Disc", "Multi-Policy", "Loyalty Disc", "Instalment Fee", "Billed Revenue"),
    levels = c("Gross Loaded", "Cap/Collar Adj", "NCD Disc", "Multi-Policy", "Loyalty Disc", "Instalment Fee", "Billed Revenue")
  ),
  delta = c(
    tot_loaded_gross,
    tot_capcup_adj,
    tot_ncd_disc,
    tot_multi_disc,
    tot_loyal_disc,
    tot_surcharge,
    tot_final_rev
  ),
  type = c("Total", "Adj", "Reduction", "Reduction", "Reduction", "Addition", "Total")
)

bal <- c(tot_loaded_gross, tot_loaded_gross + cumsum(wf_data$delta[2:6]))

start_vals <- c(0, bal[1:5], 0)
end_vals   <- c(bal[1:6], tot_final_rev)

wf_data$ymin <- pmin(start_vals, end_vals)
wf_data$ymax <- pmax(start_vals, end_vals)
wf_data$label_y <- wf_data$ymax + 0.35

wf_data$label_text <- ifelse(
  wf_data$type == "Total",
  sprintf("$%.2fM", wf_data$delta),
  ifelse(
    wf_data$delta >= 0,
    sprintf("+$%.2fM", wf_data$delta),
    sprintf("-$%.2fM", abs(wf_data$delta))
  )
)


# C. Render ggplot2 waterfall chart
p_waterfall <- ggplot(wf_data, aes(x = step, fill = type)) +
  geom_rect(aes(xmin = as.numeric(step) - 0.35, xmax = as.numeric(step) + 0.35,
                ymin = ymin, ymax = ymax), color = "black", linewidth = 0.3) +
  geom_text(aes(x = step, y = label_y, label = label_text), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Total"     = "#2b5c8f",
    "Reduction" = "#c0392b",
    "Addition"  = "#27ae60",
    "Adj"       = "#d35400"
  )) +
  scale_y_continuous(labels = dollar_format(suffix = "M"), limits = c(0, max(wf_data$ymax) * 1.15)) +
  labs(
    title = "Saltbush Direct 2027 Portfolio Pricing Waterfall",
    subtitle = sprintf("Macro reconciliation from Gross Loaded Premium to Final Billed Revenue (N = %s)", comma(n_policies)),
    x = NULL,
    y = "Portfolio Premium ($M)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10, angle = 15, hjust = 1)
  )

print(p_waterfall)



# 18. Export Priced Book --------------------------------------------------
clean_cols <- c(
  "PolicyID",
  "TechnicalPrice",              # True expected claims cost
  "BaseTechnicalPrice",          # Unbundled pure risk base (NCD=0, Annual)
  "CapCupLoadedPremium",         # Capped gross shelf base (what discounts actually come off)
  "NCDDiscountDollarAmt",
  "MultiPolicyDiscountDollarAmt",
  "LoyaltyDiscountDollarAmt",
  "InstalmentSurchargeDollarAmt",
  "FinalPrice"
)

# 2. Extract clean submission table
submission_2027 <- book_2027[, ..clean_cols]

# 3. Pre-export validation
stopifnot(nrow(submission_2027) == nrow(book_2027))
stopifnot(!anyDuplicated(submission_2027$PolicyID))
stopifnot(all(!is.na(submission_2027)))
stopifnot(all(submission_2027$FinalPrice > 0))
stopifnot(all(submission_2027$TechnicalPrice > 0))

# 4. Write clean CSV to current working directory
output_file <- "priced_2027.csv"
fwrite(submission_2027, output_file)

cat(sprintf("\n[SUCCESS] Clean deliverable exported (%d columns, %s rows) to: %s/%s\n", 
            ncol(submission_2027), scales::comma(nrow(submission_2027)), getwd(), output_file))

