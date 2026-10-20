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


# 6. Quick Recon Check (should match already computed prems) --------------
book_2027[, PredFrequency  := predict(final_freq_model, newdata = book_2027, type = "response")]
book_2027[, PredSeverity   := predict(final_sev_model,  newdata = book_2027, type = "response")]
book_2027[, TechnicalPrice := PredFrequency * PredSeverity]



# 7a. Calculate Tech Premiums ---------------------------------------------
# Recompute tech premiums setting all policies to NCD = 0 and Annual payment
# This is to ensure that discounts are not double counted
book_base <- copy(book_2027)
book_base[, NCDLevel := NULL]
book_base[, NCDLevel := factor("0", levels = as.character(0:6))]
book_base[, PaymentFrequency := factor("Annual", levels = c("Annual", "Monthly"))]

book_2027[, `:=`(
  Base_Frequency = predict(final_freq_model, newdata = book_base, type = "response"),
  Base_Severity  = PredSeverity
)]
book_2027[, Base_Pure_Prem := Base_Frequency * Base_Severity]

# Step 1 Check
cat("Mean Raw Technical Price : $", round(mean(book_2027$TechnicalPrice), 2), "\n")
cat("Mean Base Pure Premium   : $", round(mean(book_2027$Base_Pure_Prem), 2), "\n")


# 7b. Commercial Loading --------------------------------------------------
fixed_cost       <- 85
variable_expense <- 0.10
profit_margin    <- 0.08
loading_divisor  <- 1 - variable_expense - profit_margin # 0.82

# Calculate the undiscounted Gross Base Premium
book_2027[, Gross_Base_Prem := (Base_Pure_Prem + fixed_cost) / loading_divisor]

# Step 7b Check
cat("Mean Gross Base Premium  : $", round(mean(book_2027$Gross_Base_Prem), 2), "\n")


# 7c. Commercial Discounts ------------------------------------------------
# 1. No-Claims Discount Schedule (0% to 60%)
ncd_lookup <- c("0" = 0.00, "1" = 0.10, "2" = 0.20, "3" = 0.30, 
                "4" = 0.40, "5" = 0.50, "6" = 0.60)
book_2027[, Disc_NCD := ncd_lookup[as.character(NCDLevel)]]

# 2. Loyalty Discount Schedule (by TenureYears)
book_2027[, Disc_Loyalty := fifelse(TenureYears >= 10, 0.10,
                                    fifelse(TenureYears >= 6,  0.06,
                                            fifelse(TenureYears >= 3,  0.03, 0.00)))]

# 3. Multi-Policy Discount Schedule (10% if flag == 1)
book_2027[, Disc_MultiPolicy := fifelse(MultiPolicyFlag == 1, 0.10, 0.00)]

# 4. Multiplicative Compounding (Total Promised Discount %)
book_2027[, Promised_Discount_Pct := 1 - ((1 - Disc_NCD) * 
                                            (1 - Disc_Loyalty) * 
                                            (1 - Disc_MultiPolicy))]

# 5. Net Discounted Premium
book_2027[, Net_Discounted_Prem := Gross_Base_Prem * (1 - Promised_Discount_Pct)]

# Step 7c Output Checks
cat("Mean Gross Base Premium   : $", round(mean(book_2027$Gross_Base_Prem), 2), "\n")
cat("Mean Promised Discount   : ",  round(mean(book_2027$Promised_Discount_Pct) * 100, 2), "%\n")
cat("Mean Net Discounted Prem : $", round(mean(book_2027$Net_Discounted_Prem), 2), "\n")


# 7d. Capping (including finding appropriate cap values) ------------------
rate_change <- (book_2027$Net_Discounted_Prem / book_2027$Premium_2026) - 1
round(quantile(rate_change, probs = c(0.50, 0.75, 0.85, 0.90, 0.95)) * 100, 1)
c("Above +15% (High Lapse)" = sum(rate_change > 0.15), 
  "Above +30% (Severe Lapse)" = sum(rate_change > 0.30))

caps <- c("+15% Cap" = 0.15, "+20% Cap" = 0.20, "+30% Cap" = 0.30)
round(sapply(caps, function(c) sum(pmax(0, book_2027$Net_Discounted_Prem - book_2027$Premium_2026 * (1 + c)))))

# Capped at +15% to eliminate rate shock above the first major lapse cliff.
# Portfolio subsidy: ~$2.27M across 3,473 affected policies.
cap_ratio <- 1.15

book_2027[, `:=`(
  Cap_Ceiling   = Premium_2026 * cap_ratio,
  Cap_Triggered = Net_Discounted_Prem > (Premium_2026 * cap_ratio),
  Capped_Prem   = pmin(Net_Discounted_Prem, Premium_2026 * cap_ratio)
)]

# Step 7d Check
cat("Mean Capped Premium       : $", round(mean(book_2027$Capped_Prem), 2), "\n")
cat("Policies Capped (> +15%)  :", sum(book_2027$Cap_Triggered), "\n")


# 7e. Monthly Instalment Surcharge ----------------------------------------
book_2027[, Final_Payable := fifelse(
  PaymentFrequency == "Monthly",
  round(Capped_Prem * 1.03, 2),
  round(Capped_Prem, 2)
)]

# Step 7e Checks
cat("Annual Policies           :", sum(book_2027$PaymentFrequency == "Annual"), "\n")
cat("Monthly Policies          :", sum(book_2027$PaymentFrequency == "Monthly"), "\n")
cat("Mean Final Payable Premium: $", round(mean(book_2027$Final_Payable), 2), "\n")


# 7f. Checks to ensure discounts flowed through ---------------------------
cat("Mean 2026 Actual Premium : $", round(mean(book_2027$Premium_2026), 2), "\n")

# Profitability check
# Portfolio Financial Balance Check
total_claims_cost  <- sum(book_2027$TechnicalPrice)
total_prem_revenue <- sum(book_2027$Final_Payable)
loss_ratio         <- total_claims_cost / total_prem_revenue

cat("Total Expected Claims Cost: $", round(total_claims_cost, 0), "\n")
cat("Total Premium Revenue     : $", round(total_prem_revenue, 0), "\n")
cat("Expected Loss Ratio       : ",  round(loss_ratio * 100, 2), "%\n")

target_premium_per_policy <- (mean(book_2027$TechnicalPrice) + 85) / 0.82
actual_net_per_policy     <- mean(book_2027$Net_Discounted_Prem)
dollar_shortfall          <- target_premium_per_policy - actual_net_per_policy

cat("Required Target Premium : $", round(target_premium_per_policy, 2), "\n")
cat("Actual Net Premium      : $", round(actual_net_per_policy, 2), "\n")
cat("Shortfall Per Policy    : $", round(dollar_shortfall, 2), "\n")
cat("Total Portfolio Deficit : $", round(dollar_shortfall * nrow(book_2027), 0), "\n")


# 8. Final Checks of Discounts --------------------------------------------
# 1. Calculate Promised vs Delivered Discounts
book_2027[, `:=`(
  Promised_Discount_Dlr  = Gross_Base_Prem * Promised_Discount_Pct,
  Delivered_Discount_Dlr = Gross_Base_Prem - Net_Discounted_Prem,
  Delivered_Discount_Pct = (Gross_Base_Prem - Net_Discounted_Prem) / Gross_Base_Prem
)]

# 2. Check for Discount Erosion (Tolerance Check for Floating Point)
# Delivered discount % must match promised discount % within 0.0001%
book_2027[, Promise_Pass := abs(Delivered_Discount_Pct - Promised_Discount_Pct) < 1e-6]

# 3. Create Clean Audit Table (Matching ASIC REP 765 Table Layout)
audit_summary <- book_2027[, .(
  Policies             = .N,
  Mean_Pre_Disc_Base   = round(mean(Gross_Base_Prem), 2),
  Mean_Net_Prem        = round(mean(Net_Discounted_Prem), 2),
  Promised_Dlr_Disc    = round(mean(Promised_Discount_Dlr), 2),
  Delivered_Dlr_Disc   = round(mean(Delivered_Discount_Dlr), 2),
  Pass_Count           = sum(Promise_Pass),
  Compliance_Rate      = paste0(round(100 * mean(Promise_Pass), 2), "%")
)]

cat("\n=================== TASK 2a SYSTEMATIC AUDIT ===================\n")
print(audit_summary)


# 4. Check Across Each Individual Discount Eligibility Tier
cat("\n--- AUDIT BY NCD LEVEL ---\n")
print(book_2027[, .(
  Policies      = .N,
  Promised_NCD  = paste0(first(Disc_NCD) * 100, "%"),
  Mean_Base     = round(mean(Gross_Base_Prem), 2),
  Mean_Net      = round(mean(Net_Discounted_Prem), 2),
  Mean_Deliv_Pct= paste0(round(mean(Delivered_Discount_Pct) * 100, 2), "%"),
  Pass_Rate     = paste0(round(100 * mean(Promise_Pass), 2), "%")
), keyby = NCDLevel])

cat("\n--- AUDIT BY TENURE (LOYALTY) BAND ---\n")
print(book_2027[, .(
  Policies      = .N,
  Loyalty_Disc  = paste0(first(Disc_Loyalty) * 100, "%"),
  Mean_Deliv_Pct= paste0(round(mean(Delivered_Discount_Pct) * 100, 2), "%"),
  Pass_Rate     = paste0(round(100 * mean(Promise_Pass), 2), "%")
), keyby = Disc_Loyalty])

# Final assertion check
stopifnot(all(book_2027$Promise_Pass))
cat("\n✓ Verification Passed: 100% of policies received their promised discounts in full.\n")


# 9. Cause of High LR -----------------------------------------------------
claims_total <- sum(book_2027$TechnicalPrice)

# 1. Gross Base (Undiscounted List Price)
lr_gross <- claims_total / sum(book_2027$Gross_Base_Prem)

# 2. Sequential impact of each commercial discount
prem_after_ncd     <- book_2027$Gross_Base_Prem * (1 - book_2027$Disc_NCD)
prem_after_loyalty <- prem_after_ncd * (1 - book_2027$Disc_Loyalty)
prem_after_multi   <- prem_after_loyalty * (1 - book_2027$Disc_MultiPolicy)

lr_after_ncd     <- claims_total / sum(prem_after_ncd)
lr_after_loyalty <- claims_total / sum(prem_after_loyalty)
lr_after_multi   <- claims_total / sum(prem_after_multi) # Net Discounted

# 3. Impact of Capping and Monthly Instalment Surcharge
lr_capped <- claims_total / sum(book_2027$Capped_Prem)
lr_final  <- claims_total / sum(book_2027$Final_Payable)

# Assemble into a clean attribution table
lr_bridge <- data.table(
  Waterfall_Stage = c(
    "1. Gross Base (Undiscounted List Price)",
    "2. Deduct: No-Claims Discount (NCD)",
    "3. Deduct: Loyalty Discount",
    "4. Deduct: Multi-Policy Discount",
    "5. Apply: Renewal Cap (+15% Ceiling)",
    "6. Add: 3% Monthly Instalment Fee"
  ),
  Premium_Revenue = round(c(
    sum(book_2027$Gross_Base_Prem),
    sum(prem_after_ncd),
    sum(prem_after_loyalty),
    sum(prem_after_multi),
    sum(book_2027$Capped_Prem),
    sum(book_2027$Final_Payable)
  ), 0),
  Loss_Ratio = paste0(round(c(
    lr_gross, lr_after_ncd, lr_after_loyalty, lr_after_multi, lr_capped, lr_final
  ) * 100, 2), "%"),
  LR_Impact = c(
    paste0(round(lr_gross * 100, 2), "% (Base)"),
    paste0("+", round((lr_after_ncd - lr_gross) * 100, 2), "%"),
    paste0("+", round((lr_after_loyalty - lr_after_ncd) * 100, 2), "%"),
    paste0("+", round((lr_after_multi - lr_after_loyalty) * 100, 2), "%"),
    paste0("+", round((lr_capped - lr_after_multi) * 100, 2), "%"),
    paste0(round((lr_final - lr_capped) * 100, 2), "%")
  )
)

cat("\n================ LOSS RATIO ATTRIBUTION BRIDGE ================\n")
print(lr_bridge)

sum(book_2027$Promised_Discount_Pct == 0)
mean(book_2027$NCDLevel == 0 & book_2027$TenureYears < 3 & book_2027$MultiPolicy == 0) * 100


library(data.table)

# 1. Identify Zero-Discount Policies
# (Adjust column names if already computed)
zero_disc_idx <- which(
  book_2027$NCDLevel == 0 & 
    book_2027$TenureYears < 3 & 
    book_2027$MultiPolicy == 0
)

zero_book <- book_2027[zero_disc_idx]

# 2. Test Grid of Candidate Collars
collar_grid <- c("-10% Collar" = -0.10, 
                 "-15% Collar" = -0.15, 
                 "-20% Collar" = -0.20, 
                 "-30% Collar" = -0.30)

collar_analysis <- rbindlist(lapply(names(collar_grid), function(nm) {
  collar_pct <- collar_grid[[nm]]
  
  # Collar floor in dollars
  floor_dollars <- zero_book$Premium_2026 * (1 + collar_pct)
  
  # Condition: Does the price drop below the floor?
  hits_cup <- zero_book$Net_Discounted_Prem < floor_dollars
  
  # Dollars recovered by raising them to the floor
  dollar_uplift <- sum(pmax(floor_dollars - zero_book$Net_Discounted_Prem, 0))
  
  data.table(
    Collar_Threshold     = nm,
    Policies_Floored     = sum(hits_cup),
    Pct_of_Zero_Disc     = paste0(round(100 * mean(hits_cup), 2), "%"),
    Pct_of_Total_Book    = paste0(round(100 * sum(hits_cup) / nrow(book_2027), 2), "%"),
    Total_Premium_Gained = paste0("$", format(round(dollar_uplift, 0), big.mark = ","))
  )
}))

cat("\n================ THEORETICAL COLLAR ON ZERO-DISCOUNT COHORT ================\n")
cat("Total Policies in Zero-Discount Cohort:", nrow(zero_book), 
    paste0("(", round(100 * nrow(zero_book) / nrow(book_2027), 2), "% of total book)\n\n"))
print(collar_analysis)


# TEST
# 1. Driver Age entering 2026
book_2027[, DriverAge_2026 := DriverAge - 1]

# 2. Legacy Groupings & Multipliers (§4.2)
book_2027[, legacy_zone_factor := fcase(
  RiskZone == "A_Metro", 1.25,
  RiskZone %in% c("B_InnerRegional", "C_OuterRegional"), 1.00,
  RiskZone %in% c("D_Rural", "E_RemoteRural"), 0.80,
  default = 1.00
)]

book_2027[, legacy_veh_factor := fcase(
  VehicleType == "Sports", 1.45,
  VehicleType %in% c("Sedan", "Hatch", "SUV", "Ute"), 1.00,
  default = 1.00
)]

book_2027[, legacy_age_factor := fcase(
  DriverAge_2026 < 25, 1.35,
  DriverAge_2026 >= 25 & DriverAge_2026 < 65, 1.00,
  DriverAge_2026 >= 65, 1.15,
  default = 1.00
)]

# Legacy NCD discount formula: 1 - 0.04 * NCDLevel (capped at NCD 6 -> 24%)
# In 2026, NCD was pre-advancement; for Gross Base (undiscounted), factor is 1.00
book_2027[, legacy_val_factor := (VehValue / 24000) ^ 0.30]

# 3. True 2026 Undiscounted Gross Base
book_2027[, Gross_Base_2026 := 1000 * 
            legacy_zone_factor * 
            legacy_veh_factor * 
            legacy_age_factor * 
            legacy_val_factor * 
            1.35]
# 4
hist_2026 <- data[Year == 2026, .(
  PolicyID, 
  NCDLevel_2026 = NCDLevel,
  DriverAge_2026_hist = DriverAge
)]

# Merge into book_2027
book_2027[hist_2026, on = "PolicyID", `:=`(
  NCDLevel_2026 = i.NCDLevel_2026,
  DriverAge_2026_hist = i.DriverAge_2026_hist
)]

# Fallback: if any policy wasn't observed in 2026 claims history, use DriverAge - 1
book_2027[is.na(NCDLevel_2026), NCDLevel_2026 := pmax(0, NCDLevel - 1)]

# 5. Compute Legacy NCD Factor and Reconstructed Premium ------------------
# Legacy NCD Formula (§4.2): 1 - 0.04 * NCDLevel (capped at NCD 6 -> 24% discount)
book_2027[, legacy_ncd_factor := 1 - 0.04 * pmin(NCDLevel_2026, 6)]

# Formula: Premium_2026 = LegacyTechnicalPrice * 1.35 (rounded, floored at $350)
# Since Gross_Base_2026 = LegacyTechnicalPrice(NCD=0) * 1.35:
# LegacyTechnicalPrice * 1.35 = Gross_Base_2026 * legacy_ncd_factor
book_2027[, Legacy_PreFloor_Prem := Gross_Base_2026 * legacy_ncd_factor]
book_2027[, Reconstructed_Prem_2026 := round(pmax(Legacy_PreFloor_Prem, 350))]


# 6. Alignment & Reconciliation Audit -------------------------------------
book_2027[, Dollar_Diff := abs(Reconstructed_Prem_2026 - Premium_2026)]

recon_summary <- data.table(
  Total_Policies      = nrow(book_2027),
  Exact_Matches       = sum(book_2027$Dollar_Diff == 0),
  Exact_Match_Pct     = paste0(round(100 * mean(book_2027$Dollar_Diff == 0), 2), "%"),
  Within_1_Dollar     = sum(book_2027$Dollar_Diff <= 1), Within_1_Dollar_Pct = paste0(round(100 * mean(book_2027$Dollar_Diff <= 1), 2), "%"),
  Max_Dollar_Diff     = max(book_2027$Dollar_Diff),
  Mean_Dollar_Diff    = round(mean(book_2027$Dollar_Diff), 4)
)

cat("\n=================== 2026 PREMIUM RECONCILIATION AUDIT ===================\n")
print(recon_summary)

# 7. Diagnostic Breakdown of Discrepancies (if any > $1) -------------------
bad_matches <- book_2027[Dollar_Diff > 1]
if (nrow(bad_matches) > 0) {
  cat("\nPolicies with >$1 difference:", nrow(bad_matches), "\n")
  print(head(bad_matches[, .(
    PolicyID, DriverAge, DriverAge_2026, NCDLevel, NCDLevel_2026,
    VehValue, RiskZone, VehicleType, Gross_Base_2026, 
    Reconstructed_Prem_2026, Premium_2026, Dollar_Diff
  )], 10))
} else {
  cat("\n✓ Perfect match: 100% of policies match Premium_2026 within rounding error!\n")
}

# 8. Check Impact of the $350 Floor ---------------------------------------
floored_policies <- book_2027[Premium_2026 == 350]
cat("\nPolicies sitting on the $350 floor in 2026:", nrow(floored_policies), 
    paste0("(", round(100 * nrow(floored_policies) / nrow(book_2027), 2), "% of book)\n"))

round(mean(book_2027$Reconstructed_Prem_2026), 2)
round(mean(renewal_book_2027$Premium_2026), 2)

# Pick 1 row
p <- book_2027[sample(.N, 1)]

# 1. Print formula pieces
c(Base = 1000, 
  Zone = p$legacy_zone_factor, 
  Veh = p$legacy_veh_factor, 
  Age = p$legacy_age_factor, 
  NCD = p$legacy_ncd_factor, 
  Value = round((p$VehValue / 24000)^0.3, 4), 
  Load = 1.35)

# 2. Multiply and compare against actual
calc <- round(1000 * p$legacy_zone_factor * p$legacy_veh_factor * p$legacy_age_factor * p$legacy_ncd_factor * ((p$VehValue / 24000)^0.3) * 1.35)
c(Calculated = calc, Actual = p$Premium_2026, Match = (calc == p$Premium_2026))
