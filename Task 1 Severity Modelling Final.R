# ACTL4305/5305: ONE SCRIPT -- EDA -> FREQUENCY -> SEVERITY -> TECHNICAL PRICE
# Run from the folder containing the three CSV files.
# Change this example to your own folder and uncomment if needed:
# setwd("C:/Users/hoohu/Downloads")
# One-time setup if packages are missing:
# install.packages(c("data.table", "ggplot2", "scales", "glmnet", "sandwich"))
# EDA is retained with small shift/retention/age-band/zero-density corrections.
# Modelling uses EDA's cleaned pre-imputation snapshot with train-only recipes.
# R/runtime execution still needs verification on your actual CSV files.

# ========================================================================
# PART A: YOUR EXISTING CLEANING AND EDA
# ========================================================================
# ACTL4305/5305 Datathon 2026
# Task 1a - Data Cleaning and Exploratory Data Analysis

library(data.table)
library(ggplot2)
library(scales)
theme_set(theme_minimal())

# 1. Load data --------------------------------------------------------------
claims_history_2023_2026 <- fread("claims_history_2023_2026.csv",
                                  na.strings = c("", "NA", "N/A", "NULL", "null"))
renewal_book_2027 <- fread("renewal_book_2027.csv")
renewal_book_2028 <- fread("renewal_book_2028.csv")

cat("Claims history:", nrow(claims_history_2023_2026), "rows x",
    ncol(claims_history_2023_2026), "columns\n")
cat("Unique policies:", uniqueN(claims_history_2023_2026$PolicyID), "\n")

profile_var <- function(x, nm) {
  num <- is.numeric(x)
  data.table(Variable = nm, Class = class(x)[1], Missing = sum(is.na(x)),
             PctMissing = round(100 * mean(is.na(x)), 2), Unique = uniqueN(x),
             Min = if (num) min(x, na.rm = TRUE) else NA_real_,
             Median = if (num) median(x, na.rm = TRUE) else NA_real_,
             Mean = if (num) round(mean(x, na.rm = TRUE), 2) else NA_real_,
             Max = if (num) max(x, na.rm = TRUE) else NA_real_)
}

issue_log <- data.table(ID = integer(), Issue = character(), Variable = character(),
                        RowsAffected = integer(), Action = character(), Rationale = character())

log_issue <- function(issue, variable, n, action, rationale) {
  issue_log <<- rbind(issue_log, data.table(
    ID = nrow(issue_log) + 1L, Issue = issue, Variable = variable,
    RowsAffected = as.integer(n), Action = action, Rationale = rationale))
  cat(sprintf("[Issue %d] %s (%s): %d rows -> %s\n",
              nrow(issue_log), issue, variable, as.integer(n), action))
}

# 2. Initial profile --------------------------------------------------------
claims_profile <- rbindlist(Map(profile_var, claims_history_2023_2026,
                                names(claims_history_2023_2026)))
print(claims_profile)

cat_cols <- c("VehicleType", "RiskZone", "AnnualKms", "GaragingLocation",
              "VehicleUse", "PaymentFrequency")
claims_levels <- rbindlist(lapply(cat_cols, function(v) {
  claims_history_2023_2026[, .N, by = v][, .(Variable = v,
                                             Level = as.character(get(v)), N)]
}))
print(claims_levels)

# 3a. Data cleaning Claims Dataset  -----------------------------------------
data <- copy(claims_history_2023_2026)
num_cols <- c("DriverAge", "YearsLicensed", "VehAge", "VehValue")
data[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# Duplicate policy-year records
dup_key <- duplicated(data, by = c("PolicyID", "Year"))
dup_full <- duplicated(data)
if (sum(dup_key) != sum(dup_full))
  warning("Some duplicated PolicyID-Year keys are not exact duplicates.")

n_dup <- sum(dup_key)
dup_claims <- data[dup_key, sum(ClaimNb)]
dup_cost <- data[dup_key, sum(ClaimAmount)]
data <- unique(data, by = c("PolicyID", "Year"))
log_issue("Duplicate policy-year rows", "PolicyID, Year", n_dup,
          "Removed repeated rows and kept the first row",
          sprintf("Avoids double counting (%d claims, $%s of cost)", dup_claims,
                  format(round(dup_cost), big.mark = ",")))

# Standardise category labels
canonical <- list(
  VehicleType = c("Sedan", "Hatch", "SUV", "Ute", "Sports"),
  RiskZone = c("A_Metro", "B_InnerRegional", "C_OuterRegional", "D_Rural", "E_RemoteRural"),
  AnnualKms = c("Low", "Medium", "High"),
  GaragingLocation = c("Garage", "Carport", "Street"),
  VehicleUse = c("Private", "Private_Business"),
  PaymentFrequency = c("Annual", "Monthly"))

for (v in names(canonical)) {
  before <- data[[v]]
  key <- tolower(trimws(before))
  if (v == "VehicleType") key[key == "sport"] <- "sports"
  after <- canonical[[v]][match(key, tolower(canonical[[v]]))]
  bad <- !is.na(before) & is.na(after)
  if (any(bad)) stop("Unrecognised labels in ", v, ": ", paste(unique(before[bad]), collapse = ", "))
  changed <- sum(!is.na(before) & before != after)
  if (changed > 0)
    log_issue("Inconsistent category labels", v, changed,
              "Standardised category labels", "Prevents the same category being split")
  data[, (v) := after]
}

# Impossible values become missing
bad_age <- which(!is.na(data$DriverAge) & (data$DriverAge < 17 | data$DriverAge > 100))
sentinel_age <- sort(unique(data$DriverAge[bad_age]))
data[bad_age, DriverAge := NA_real_]
bad_veh <- which(!is.na(data$VehAge) & data$VehAge < 0)
n_bad_veh <- length(bad_veh)
data[bad_veh, VehAge := NA_real_]
bad_exc <- which(!is.na(data$ExcessChosen) & !(data$ExcessChosen %in% c(500, 750, 1000)))
sentinel_exc <- sort(unique(data$ExcessChosen[bad_exc]))
data[bad_exc, ExcessChosen := NA_real_]
data[, ExcessChosen := as.numeric(ExcessChosen)]
data_before_imputation <- copy(data)

# Recover values from other years of the same policy
recover_progressive <- function(data, variable) {
  anchors <- data[!is.na(get(variable)), .(Anchor = median(get(variable) - Year)), by = PolicyID]
  data[anchors, on = "PolicyID", Anchor := i.Anchor]
  inconsistent <- data[!is.na(get(variable)) & !is.na(Anchor) &
                         (get(variable) - Year) != Anchor, .N]
  n_missing <- data[is.na(get(variable)), .N]
  data[is.na(get(variable)) & !is.na(Anchor), (variable) := round(Year + Anchor)]
  n_remaining <- data[is.na(get(variable)), .N]
  data[, Anchor := NULL]
  cat(variable, ":", n_missing, "missing,", inconsistent, "inconsistent,",
      n_missing - n_remaining, "recovered,", n_remaining, "remaining\n")
  list(n_missing = n_missing, n_recovered = n_missing - n_remaining,
       n_remaining = n_remaining, n_inconsistent = inconsistent)
}

flag_cols <- c("DriverAge", "VehAge", "VehValue", "AnnualKms", "GaragingLocation", "ExcessChosen")
for (v in flag_cols) data[, (paste0(v, "_Imputed")) := as.integer(is.na(get(v)))]

age_recovery <- recover_progressive(data, "DriverAge")
vehicle_age_recovery <- recover_progressive(data, "VehAge")
stopifnot(age_recovery$n_inconsistent == 0, vehicle_age_recovery$n_inconsistent == 0)

age_gap <- data[!is.na(DriverAge), median(DriverAge - YearsLicensed)]
data[is.na(DriverAge), DriverAge := YearsLicensed + age_gap]
vehicle_age_median <- data[!is.na(VehAge), median(VehAge)]
data[is.na(VehAge), VehAge := vehicle_age_median]

log_issue(sprintf("Missing DriverAge and impossible ages (%s) treated as missing",
                  paste(sentinel_age, collapse = ", ")), "DriverAge", age_recovery$n_missing,
          sprintf("%d recovered and %d filled using YearsLicensed + %d",
                  age_recovery$n_recovered, age_recovery$n_remaining, age_gap),
          "Driver age increases by one each year within a policy")
log_issue(sprintf("Negative VehAge (%d rows) treated as missing", n_bad_veh), "VehAge",
          vehicle_age_recovery$n_missing,
          sprintf("%d recovered and %d filled with median (%g)", vehicle_age_recovery$n_recovered,
                  vehicle_age_recovery$n_remaining, vehicle_age_median),
          "Vehicle age increases by one each year within a policy")

# Variables expected to remain constant within a policy
static_vars <- c("VehValue", "AnnualKms", "GaragingLocation", "ExcessChosen",
                 "VehicleType", "VehicleUse", "RiskZone", "Density",
                 "MultiPolicyFlag", "PaymentFrequency")
static_check <- rbindlist(lapply(static_vars, function(v) data.table(
  Variable = v, PoliciesWithChangingValue = data[, uniqueN(get(v), na.rm = TRUE) > 1,
                                                 by = PolicyID][, sum(V1)])))
print(static_check)

mode_of <- function(x) {
  x <- x[!is.na(x)]
  names(sort(table(x), decreasing = TRUE))[1]
}
fill_static <- function(data, variable) {
  data[, (variable) := {x <- get(variable); if (anyNA(x) && !all(is.na(x)))
    replace(x, is.na(x), x[!is.na(x)][1]) else x}, by = PolicyID]
}

for (v in c("VehValue", "AnnualKms", "GaragingLocation", "ExcessChosen")) {
  n_missing <- data[is.na(get(v)), .N]
  fill_static(data, v)
  n_remaining <- data[is.na(get(v)), .N]
  if (v == "VehValue") {
    median_by_type <- data[, .(MedianValue = median(VehValue, na.rm = TRUE)), by = VehicleType]
    data[median_by_type, on = "VehicleType",
         VehValue := fifelse(is.na(VehValue), i.MedianValue, VehValue)]
    fallback <- "median by VehicleType"
  } else if (v == "ExcessChosen") {
    excess_mode <- as.numeric(mode_of(data$ExcessChosen))
    data[is.na(ExcessChosen), ExcessChosen := excess_mode]
    fallback <- paste0("portfolio mode ($", excess_mode, ")")
  } else {
    variable_mode <- mode_of(data[[v]])
    data[is.na(get(v)), (v) := variable_mode]
    fallback <- paste0("portfolio mode (", variable_mode, ")")
  }
  issue_name <- if (v == "ExcessChosen")
    paste0("Invalid excess code (", paste(sentinel_exc, collapse = ", "), ") treated as missing")
  else "Missing values"
  log_issue(issue_name, v, n_missing,
            sprintf("%d filled from other policy years; %d filled using %s",
                    n_missing - n_remaining, n_remaining, fallback),
            "These variables are expected to remain constant within a policy")
}

# Logical consistency checks
setorder(data, PolicyID, Year)
checks <- list(
  "ClaimNb == 0 but ClaimAmount > 0" = data[ClaimNb == 0 & ClaimAmount > 0, .N],
  "ClaimNb > 0 but ClaimAmount == 0" = data[ClaimNb > 0 & ClaimAmount == 0, .N],
  "Negative ClaimNb or ClaimAmount" = data[ClaimNb < 0 | ClaimAmount < 0, .N],
  "Exposure not equal to 1" = data[Exposure != 1, .N],
  "YearsLicensed > DriverAge - 16" = data[YearsLicensed > DriverAge - 16, .N],
  "PolicyID not in POL###### format" = data[!grepl("^POL[0-9]{6}$", PolicyID), .N],
  "Non-consecutive years within policy" = data[, sum(diff(Year) != 1, na.rm = TRUE), by = PolicyID][, sum(V1)],
  "TenureYears not rising by 1 a year" = data[, sum(diff(TenureYears) != 1, na.rm = TRUE), by = PolicyID][, sum(V1)],
  "YearsLicensed not rising by 1 a year" = data[, sum(diff(YearsLicensed) != 1, na.rm = TRUE), by = PolicyID][, sum(V1)])
consistency <- data.table(Check = names(checks), Failures = unlist(checks))
print(consistency)

# Unusual values that are retained
n_ncd_gt_ten <- data[NCDLevel > TenureYears, .N]
n_old_veh <- data[VehAge > 25, .N]
n_zone_cap <- data[Density == max(Density), .N]
q999 <- data[ClaimNb > 0, quantile(ClaimAmount, .999)]
n_large <- data[ClaimAmount > 50000, .N]
log_issue("NCDLevel exceeds TenureYears", "NCDLevel", n_ncd_gt_ten, "Kept",
          "Customers may have transferred a previous NCD")
log_issue("Vehicles older than 25 years", "VehAge", n_old_veh, "Kept",
          "These values are possible and form a small upper tail")
log_issue("Large claims greater than $50,000", "ClaimAmount", n_large, "Kept",
          sprintf("99.9th percentile is about $%s", format(round(q999), big.mark = ",")))
log_issue("Density is capped at a maximum value in A_Metro", "Density", n_zone_cap,
          "Kept", "The maximum appears to be top-coded")

data[, `:=`(
  VehicleType = factor(VehicleType, canonical$VehicleType),
  RiskZone = factor(RiskZone, canonical$RiskZone),
  AnnualKms = factor(AnnualKms, canonical$AnnualKms),
  GaragingLocation = factor(GaragingLocation, canonical$GaragingLocation),
  VehicleUse = factor(VehicleUse, canonical$VehicleUse),
  PaymentFrequency = factor(PaymentFrequency, canonical$PaymentFrequency),
  ExcessChosen = as.integer(ExcessChosen), DriverAge = as.integer(DriverAge), VehAge = as.integer(VehAge))]

model_cols <- c("DriverAge", "YearsLicensed", "VehAge", "VehValue", "VehicleType", "RiskZone",
                "Density", "AnnualKms", "ExcessChosen", "GaragingLocation", "VehicleUse",
                "PaymentFrequency", "NCDLevel", "TenureYears", "MultiPolicyFlag", "Exposure",
                "ClaimNb", "ClaimAmount")
stopifnot(!anyNA(data[, ..model_cols]), !anyDuplicated(data, by = c("PolicyID", "Year")),
          data[, all(DriverAge >= 17 & DriverAge <= 100)], data[, all(VehAge >= 0)],
          data[, all(ExcessChosen %in% c(500L, 750L, 1000L))],
          data[, all(ClaimNb >= 0 & ClaimAmount >= 0)],
          uniqueN(data$PolicyID) == uniqueN(claims_history_2023_2026$PolicyID))
cat("\nAll post-cleaning validation checks passed.\n")

reconciliation <- data.table(
  Step = c("Raw rows", "Duplicate policy-years removed", "Clean rows", "Policies (unchanged)",
           "Claims in raw file", "Claims after cleaning", "Incurred cost in raw file ($)",
           "Incurred cost after cleaning ($)"),
  Value = c(nrow(claims_history_2023_2026), n_dup, nrow(data), uniqueN(data$PolicyID),
            sum(claims_history_2023_2026$ClaimNb), sum(data$ClaimNb),
            round(sum(claims_history_2023_2026$ClaimAmount)), round(sum(data$ClaimAmount))))
print(reconciliation)
cat("\nData quality log:\n")
print(issue_log[, .(ID, Issue, Variable, RowsAffected)])

# 3b. Clean renewal books 2027 and 2028 ----------------------------------------
# Define canonical category levels (from claims history cleaning)
canonical <- list(
  VehicleType = c("Sedan", "Hatch", "SUV", "Ute", "Sports"),
  RiskZone = c("A_Metro", "B_InnerRegional", "C_OuterRegional", "D_Rural", "E_RemoteRural"),
  AnnualKms = c("Low", "Medium", "High"),
  GaragingLocation = c("Garage", "Carport", "Street"),
  VehicleUse = c("Private", "Private_Business"),
  PaymentFrequency = c("Annual", "Monthly"))

clean_renewal_book <- function(renewal_data, year_name) {
  
  data_clean <- copy(renewal_data)
  
  # Convert numeric columns
  num_cols <- c("DriverAge", "YearsLicensed", "VehAge", "VehValue")
  data_clean[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  # Standardise category labels
  for (v in names(canonical)) {
    if (!(v %in% names(data_clean))) next
    
    before <- data_clean[[v]]
    key <- tolower(trimws(before))
    if (v == "VehicleType") key[key == "sport"] <- "sports"
    after <- canonical[[v]][match(key, tolower(canonical[[v]]))]
    bad <- !is.na(before) & is.na(after)
    
    if (any(bad)) {
      stop(sprintf("Unrecognised labels in %s: %s", v, paste(unique(before[bad]), collapse = ", ")))
    }
    
    data_clean[, (v) := after]
  }
  
  # Check for invalid values
  bad_age <- which(!is.na(data_clean$DriverAge) & (data_clean$DriverAge < 17 | data_clean$DriverAge > 100))
  bad_veh <- which(!is.na(data_clean$VehAge) & data_clean$VehAge < 0)
  bad_exc <- which(!is.na(data_clean$ExcessChosen) & !(data_clean$ExcessChosen %in% c(500, 750, 1000)))
  
  if (length(bad_age) > 0) data_clean[bad_age, DriverAge := NA_real_]
  if (length(bad_veh) > 0) data_clean[bad_veh, VehAge := NA_real_]
  if (length(bad_exc) > 0) data_clean[bad_exc, ExcessChosen := NA_real_]
  
  # Convert to factors
  data_clean[, `:=`(
    VehicleType = factor(VehicleType, canonical$VehicleType),
    RiskZone = factor(RiskZone, canonical$RiskZone),
    AnnualKms = factor(AnnualKms, canonical$AnnualKms),
    GaragingLocation = factor(GaragingLocation, canonical$GaragingLocation),
    VehicleUse = factor(VehicleUse, canonical$VehicleUse),
    PaymentFrequency = factor(PaymentFrequency, canonical$PaymentFrequency),
    ExcessChosen = as.integer(ExcessChosen),
    DriverAge = as.integer(DriverAge),
    VehAge = as.integer(VehAge)
  )]
  
  # Validation checks
  checks <- list(
    "Duplicate PolicyID" = data_clean[, sum(duplicated(PolicyID))],
    "Invalid DriverAge" = length(which(!is.na(data_clean$DriverAge) & 
                                         (data_clean$DriverAge < 17 | data_clean$DriverAge > 100))),
    "Invalid VehAge" = length(which(!is.na(data_clean$VehAge) & data_clean$VehAge < 0)),
    "Invalid ExcessChosen" = length(which(!is.na(data_clean$ExcessChosen) & 
                                            !(data_clean$ExcessChosen %in% c(500, 750, 1000)))),
    "NCDLevel outside 0–6" = data_clean[NCDLevel < 0 | NCDLevel > 6, .N],
    "TenureYears < 0" = data_clean[TenureYears < 0, .N]
  )
  
  failures <- data.table(Check = names(checks), Failures = unlist(checks))
  
  if (failures[, any(Failures > 0)]) {
    cat(sprintf("\n%s Validation Issues:\n", year_name))
    print(failures[Failures > 0])
  } else {
    cat(sprintf("\n%s: All validation checks passed ✓\n", year_name))
  }
  
  cat(sprintf("  %d policies × %d columns\n", nrow(data_clean), ncol(data_clean)))
  
  return(data_clean)
}

# Clean both renewal books
renewal_book_2027 <- clean_renewal_book(renewal_book_2027, "2027 Renewal Book")
renewal_book_2028 <- clean_renewal_book(renewal_book_2028, "2028 Renewal Book")

cat("\n✓ Renewal books cleaned and ready for modelling\n")

# 4. Exploratory data analysis ---------------------------------------------
missingness_table <- rbindlist(lapply(flag_cols, function(v) {
  missing <- is.na(data_before_imputation[[v]])
  data.table(Variable = v, MissingRows = sum(missing), PctMissing = round(100 * mean(missing), 2),
             FreqIfMissing = round(data_before_imputation[missing, sum(ClaimNb) / sum(Exposure)], 4),
             FreqIfPresent = round(data_before_imputation[!missing, sum(ClaimNb) / sum(Exposure)], 4))
}))
print(missingness_table)

data[, FirstYear := min(Year), by = PolicyID]
portfolio <- data[, .(PolicyYears = .N, Policies = uniqueN(PolicyID), NewToPanel = sum(Year == FirstYear),
                      PctMonthly = round(100 * mean(PaymentFrequency == "Monthly"), 1),
                      PctMultiPolicy = round(100 * mean(MultiPolicyFlag == 1), 1), MeanNCD = round(mean(NCDLevel), 2),
                      MeanTenure = round(mean(TenureYears), 2), MeanDriverAge = round(mean(DriverAge), 1)), by = Year][order(Year)]
print(portfolio)

policy_length <- data[, .N, by = PolicyID][, .(Policies = .N), keyby = .(YearsObserved = N)]
print(policy_length)

claim_counts <- data[, .(PolicyYears = .N), keyby = ClaimNb]
claim_counts[, Share := round(PolicyYears / sum(PolicyYears), 4)]
print(claim_counts)
cat(sprintf("Overall frequency: %.4f; mean severity: $%.0f; loss cost: $%.0f\n",
            data[, sum(ClaimNb) / sum(Exposure)], data[ClaimNb > 0, sum(ClaimAmount) / sum(ClaimNb)],
            data[, sum(ClaimAmount) / sum(Exposure)]))
cat(sprintf("Variance/mean of ClaimNb = %.3f (Poisson = 1)\n", data[, var(ClaimNb) / mean(ClaimNb)]))

severity_data <- data[ClaimNb > 0]
severity_data[, AverageClaim := ClaimAmount / ClaimNb]
severity_quantiles <- severity_data[, .(Quantile = c("Min", "25%", "50%", "75%", "90%", "95%", "99%", "99.9%", "Max"),
                                        AverageClaim = round(quantile(AverageClaim, c(0, .25, .5, .75, .9, .95, .99, .999, 1)), 0))]
print(severity_quantiles)
print(head(data[order(-ClaimAmount), .(PolicyID, Year, ClaimNb, ClaimAmount, VehicleType, VehValue, RiskZone)], 20))

severity_plot <- ggplot(severity_data, aes(AverageClaim)) + geom_histogram(bins = 60) +
  scale_x_log10(labels = label_dollar()) +
  labs(title = "Average cost per claim", subtitle = "Claim-years only, shown on a log scale",
       x = "Average claim cost", y = "Policy-years")
print(severity_plot)

numeric_data <- melt(data[, lapply(.SD, as.numeric), .SDcols = c("DriverAge", "YearsLicensed", "VehAge", "VehValue", "Density", "TenureYears")],
                     measure.vars = c("DriverAge", "YearsLicensed", "VehAge", "VehValue", "Density", "TenureYears"))
numeric_plot <- ggplot(numeric_data, aes(value)) + geom_histogram(bins = 40) +
  facet_wrap(~variable, scales = "free") + labs(title = "Distribution of numeric rating factors", x = NULL, y = "Policy-years")
print(numeric_plot)

categorical_data <- melt(data[, .(VehicleType = as.character(VehicleType), RiskZone = as.character(RiskZone),
                                  AnnualKms = as.character(AnnualKms), GaragingLocation = as.character(GaragingLocation),
                                  VehicleUse = as.character(VehicleUse), PaymentFrequency = as.character(PaymentFrequency),
                                  ExcessChosen = as.character(ExcessChosen), NCDLevel = as.character(NCDLevel),
                                  MultiPolicyFlag = as.character(MultiPolicyFlag))], measure.vars = 1:9)
categorical_plot <- ggplot(categorical_data, aes(value)) + geom_bar() +
  facet_wrap(~variable, scales = "free_x", ncol = 3) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(title = "Distribution of categorical rating factors", x = NULL, y = "Policy-years")
print(categorical_plot)

# 5. Frequency and severity by rating factor -------------------------------
data[, `:=`(
  DriverAgeBand = cut(DriverAge, c(0, 24, 34, 44, 54, 64, 74, Inf), labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")),
  YearsLicensedBand = cut(YearsLicensed, c(-Inf, 4, 9, 19, 29, 39, Inf), labels = c("0-4", "5-9", "10-19", "20-29", "30-39", "40+")),
  VehAgeBand = cut(VehAge, c(-Inf, 2, 5, 9, 14, 19, Inf), labels = c("0-2", "3-5", "6-9", "10-14", "15-19", "20+")),
  VehValueBand = cut(VehValue, c(0, 10000, 15000, 20000, 30000, 40000, 60000, Inf), labels = c("<10k", "10-15k", "15-20k", "20-30k", "30-40k", "40-60k", "60k+")),
  DensityBand = cut(Density, c(-Inf, 10, 100, 500, 2000, Inf), labels = c("<10", "10-100", "100-500", "500-2000", "2000+")),
  TenureBand = cut(TenureYears, c(-Inf, 0, 1, 2, 5, 9, Inf), labels = c("0", "1", "2", "3-5", "6-9", "10+")))]

eda_vars <- c("Year", "DriverAgeBand", "YearsLicensedBand", "VehAgeBand", "VehValueBand", "VehicleType", "RiskZone", "DensityBand", "AnnualKms", "ExcessChosen", "GaragingLocation", "VehicleUse", "PaymentFrequency", "NCDLevel", "TenureBand", "MultiPolicyFlag")
univariate_summary <- function(data, variable) {
  x <- data[, .(PolicyYears = sum(Exposure), Claims = sum(ClaimNb), ClaimCost = sum(ClaimAmount)), keyby = variable]
  setnames(x, variable, "Level")
  x[, `:=`(Variable = variable, Level = as.character(Level), ExposureShare = round(PolicyYears / sum(PolicyYears), 4),
           Frequency = Claims / PolicyYears, FreqSE = sqrt(Claims) / PolicyYears,
           AvgSeverity = fifelse(Claims > 0, ClaimCost / Claims, NA_real_), LossCost = ClaimCost / PolicyYears)]
  x
}
univariate <- rbindlist(lapply(eda_vars, univariate_summary, data = data))
univariate[, Key := paste(Variable, Level, sep = "|")]
print(univariate[, .(Variable, Level, PolicyYears, ExposureShare, Claims,
                     Frequency = round(Frequency, 4), AvgSeverity = round(AvgSeverity, 0), LossCost = round(LossCost, 1))])

overall_frequency <- data[, sum(ClaimNb) / sum(Exposure)]
overall_severity <- data[ClaimNb > 0, sum(ClaimAmount) / sum(ClaimNb)]
strip_prefix <- function(x) sub("^.*\\|", "", x)

frequency_plot <- ggplot(univariate, aes(Key, Frequency)) +
  geom_hline(yintercept = overall_frequency, linetype = 2, colour = "grey50") +
  geom_errorbar(aes(ymin = pmax(0, Frequency - 1.96 * FreqSE), ymax = Frequency + 1.96 * FreqSE), width = .25) +
  geom_point(size = 1.8) + facet_wrap(~Variable, scales = "free_x", ncol = 4) +
  scale_x_discrete(labels = strip_prefix) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), axis.title.x = element_blank()) +
  labs(title = "Claim frequency by rating factor", subtitle = "Dashed line shows portfolio frequency", y = "Claims per policy-year")
print(frequency_plot)

severity_factor_plot <- ggplot(univariate[!is.na(AvgSeverity)], aes(Key, AvgSeverity)) +
  geom_hline(yintercept = overall_severity, linetype = 2, colour = "grey50") +
  geom_point(aes(size = Claims)) + facet_wrap(~Variable, scales = "free_x", ncol = 4) +
  scale_x_discrete(labels = strip_prefix) + scale_y_continuous(labels = label_dollar()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), axis.title.x = element_blank()) +
  labs(title = "Average claim cost by rating factor", subtitle = "Dashed line shows portfolio average", y = "Average cost per claim")
print(severity_factor_plot)

# 6. Trends over time -------------------------------------------------------
year_summary <- univariate[Variable == "Year", .(Year = as.integer(Level), PolicyYears, Claims,
                                                 Frequency = round(Frequency, 4), AvgSeverity = round(AvgSeverity, 0), LossCost = round(LossCost, 1))]
print(year_summary)
data[, YearC := Year - 2023]
adjustment_terms <- paste("RiskZone", "DriverAgeBand", "VehicleType", "AnnualKms", "GaragingLocation", "VehicleUse", "PaymentFrequency", sep = " + ")

trend_fit <- function(formula, family, data, weighted = FALSE) {
  d <- copy(data); d[, Weight := if (weighted) ClaimNb else 1]
  m <- glm(formula, family = family, data = d, weights = Weight)
  est <- coef(m)["YearC"]; ci <- confint.default(m)["YearC", ]
  c(AnnualChange = exp(est) - 1, Lower = exp(ci[1]) - 1, Upper = exp(ci[2]) - 1,
    pValue = summary(m)$coefficients["YearC", 4])
}

severity_data <- data[ClaimNb > 0]; severity_data[, AverageClaim := ClaimAmount / ClaimNb]
trend_table <- rbind(
  data.table(Model = "Frequency (unadjusted)", t(trend_fit(ClaimNb ~ YearC, poisson(), data))),
  data.table(Model = "Frequency (mix-adjusted)", t(trend_fit(as.formula(paste("ClaimNb ~ YearC +", adjustment_terms)), poisson(), data))),
  data.table(Model = "Severity (unadjusted)", t(trend_fit(AverageClaim ~ YearC, Gamma("log"), severity_data, TRUE))),
  data.table(Model = "Severity (mix-adjusted)", t(trend_fit(as.formula(paste("AverageClaim ~ YearC +", adjustment_terms)), Gamma("log"), severity_data, TRUE))))
trend_table[, 2:4 := lapply(.SD, function(x) round(100 * x, 2)), .SDcols = 2:4]
setnames(trend_table, c("Model", "AnnualChange_pct", "Lower95_pct", "Upper95_pct", "pValue"))
print(trend_table)

trend_plot <- ggplot(melt(year_summary[, .(Year, Frequency, AvgSeverity)], id.vars = "Year"), aes(Year, value)) +
  geom_line() + geom_point(size = 2) + facet_wrap(~variable, scales = "free_y") +
  labs(title = "Claim frequency and average claim cost by year", x = NULL, y = NULL)
print(trend_plot)

# 7. Relationships between rating factors ---------------------------------
density_plot <- ggplot(data, aes(RiskZone, Density)) + geom_boxplot() + scale_y_log10() +
  labs(title = "Density by RiskZone", x = NULL, y = "People per km2") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
print(density_plot)

set.seed(1)
sample_data <- data[sample(.N, min(.N, 8000))]
age_plot <- ggplot(sample_data, aes(DriverAge, YearsLicensed)) + geom_point(alpha = .15, size = .8) +
  labs(title = "Driver age versus years licensed", subtitle = "The two variables are closely related")
print(age_plot)

ncd_tenure <- data[, .N, by = .(TenureYears, NCDLevel)]
ncd_plot <- ggplot(ncd_tenure, aes(factor(TenureYears), factor(NCDLevel), fill = N)) + geom_tile() +
  scale_fill_viridis_c(trans = "log10") +
  labs(title = "NCDLevel versus TenureYears", x = "TenureYears", y = "NCDLevel", fill = "Policy-years")
print(ncd_plot)

numeric_variables <- c("DriverAge", "YearsLicensed", "VehAge", "VehValue", "Density", "NCDLevel", "TenureYears", "ExcessChosen", "MultiPolicyFlag")
correlation_matrix <- cor(data[, ..numeric_variables], method = "spearman")
print(data.table(Variable = rownames(correlation_matrix), round(correlation_matrix, 3)))
correlation_long <- as.data.table(as.table(correlation_matrix))
correlation_plot <- ggplot(correlation_long, aes(V1, V2, fill = N)) + geom_tile() +
  geom_text(aes(label = sprintf("%.2f", N)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  labs(title = "Spearman correlation of numeric factors", x = NULL, y = NULL, fill = NULL)
print(correlation_plot)

cramers_v <- function(x, y) {
  tab <- table(x, y)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  sqrt(as.numeric(chi) / (sum(tab) * (min(dim(tab)) - 1)))
}
categorical_variables <- c("VehicleType", "RiskZone", "AnnualKms", "GaragingLocation", "VehicleUse", "PaymentFrequency", "ExcessChosen", "NCDLevel", "TenureBand", "MultiPolicyFlag", "DriverAgeBand", "VehAgeBand")
cramers_table <- CJ(A = categorical_variables, B = categorical_variables)
cramers_table[, V := mapply(function(a, b) cramers_v(data[[a]], data[[b]]), A, B)]
print(cramers_table[A < B][order(-V)][, .(A, B, V = round(V, 3))][1:15])

# 8. NCD changes after claims ----------------------------------------------
setorder(data, PolicyID, Year)
data[, `:=`(PrevClaims = shift(ClaimNb), PrevNCD = shift(NCDLevel)), by = PolicyID]
ncd_change <- data[!is.na(PrevClaims) & PrevNCD > 0 & PrevNCD < 6,
                   .(PolicyYears = .N, MeanNCDChange = round(mean(NCDLevel - PrevNCD), 2)),
                   keyby = .(PrevClaims = pmin(PrevClaims, 2))]
print(ncd_change)

#9. Portfolio Persistence (cohort attrition) -------------------------------
cohort_survival <- data[, .(PolicyYears = .N), by = .(FirstYear, Year)]
cohort_survival[, YearsSinceEntry := Year - FirstYear]

# E.g., policies entering 2023: how many remain in 2024, 2025, 2026?
# If 80% drop out by year 3, renewal-book pricing has different risk profile

#10. NCD Effectiveness  ---------------------------------------------------
data[, FutureClaimNb := shift(ClaimNb, n = 1L, type = "lead"), by = PolicyID]

# Among policies with NCDLevel = 6 this year, what's frequency NEXT year?
# vs. NCDLevel = 0?

ncd_predictive <- data[!is.na(FutureClaimNb), .(
  NextYearFreq = sum(FutureClaimNb) / .N
), by = NCDLevel][order(NCDLevel)]

# If NCDLevel=6 has lower NextYearFreq, NCD is predictive
# If not, NCD is just a discount with no risk signal

#11. Reinsurance  ----------------------------------------------------------

data[ClaimAmount > 50000, .(Count = .N, TotalCost = sum(ClaimAmount), RetentionCost = 50000 * .N)]
# Do any claims cross a reinsurance retention (e.g., >$50k is reinsured)?
# If so, your severity model for Task 1 should exclude them or down-weight them
# (your loaded premium calculation needs to reflect actual insurer cost, not incurred)

#12. Risk Zone Analysis ----------------------------------------------------
interaction_check <- data[, .(
  Frequency = sum(ClaimNb) / .N,
  Claims = sum(ClaimNb)
), by = .(RiskZone, PaymentFrequency)]
print(interaction_check)

#13. Rention Rate by Year --------------------------------------------------
# Add a minimum y-intercept line to show the 50% threshold
# Define the input used by the existing retention plot.
retention_long <- copy(cohort_survival)
retention_long[, SurvivalRate := 100 * PolicyYears /
  PolicyYears[YearsSinceEntry == 0], by = FirstYear]
retention_plot <- ggplot(retention_long, aes(YearsSinceEntry, SurvivalRate, colour = factor(FirstYear), group = FirstYear)) +
  geom_hline(yintercept = 65, linetype = "dashed", colour = "grey50", alpha = 0.5) +  # Reference line
  geom_line(size = 1) + geom_point(size = 2) +
  scale_y_continuous(limits = c(65, 105), labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = 0:3) +
  labs(title = "Cohort Survival: Policy Retention by Entry Year",
       x = "Years Since Entry", y = "Retention Rate (%)", colour = "Entry Year",
       subtitle = "Shows how many policies remain after N years") +
  theme_minimal()

print(retention_plot)

#14. Chang in Driver Age Distribution for Renewal Periods ------------------
# Define age bands
age_band_cuts <- c(0, 25, 35, 45, 55, 65, 75, Inf)
age_band_labels <- c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")

# Function to create age distribution
create_age_dist <- function(df, dataset_name) {
  df[, AgeGroup := cut(DriverAge, breaks = age_band_cuts, labels = age_band_labels, right = FALSE)]
  df[, .N, by = AgeGroup][, `:=`(Dataset = dataset_name, Pct = round(100 * N / sum(N), 1))]
}

# Create distributions for all three datasets
mix_shift <- rbind(
  create_age_dist(data[Year == 2026, .(DriverAge)], "2026 Training")[, .(AgeGroup, Dataset, N, Pct)],
  create_age_dist(copy(renewal_book_2027)[, .(DriverAge)], "2027 Renewal")[, .(AgeGroup, Dataset, N, Pct)],
  create_age_dist(copy(renewal_book_2028)[, .(DriverAge)], "2028 Renewal")[, .(AgeGroup, Dataset, N, Pct)]
)

# Order factor for proper plotting
mix_shift[, AgeGroup := factor(AgeGroup, levels = age_band_labels, ordered = TRUE)]

# Plot - histogram format
ggplot(mix_shift, aes(x = AgeGroup, y = Pct, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("2026 Training" = "#1f77b4", "2027 Renewal" = "#ff7f0e", "2028 Renewal" = "#2ca02c")) +
  labs(title = "Portfolio Mix Shift: Driver Age Distribution",
       x = "Age Band", y = "% of Policies", fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

#15. New Business Premium ----------------------------------------------
renewal_book_2027[, .(
  MeanPremium = mean(Premium_2026),
  N = .N
), by = TenureYears]



# ========================================================================
# PART B: FREQUENCY MODELLING -- YOUR TEAMMATE'S SIX APPROACHES
# ========================================================================
# Shared preparation uses the EDA snapshot; it does not modify EDA `data`.
# Inner penalty CV shares the tuning-subset recipe (see walkthrough limitation).
if (!exists("data_before_imputation"))
  stop("Run the original EDA through creation of data_before_imputation first.")
shared_history <- copy(data_before_imputation)
stopifnot(!anyDuplicated(shared_history, by = c("PolicyID", "Year")),
          !anyNA(shared_history[, .(PolicyID, Year, ClaimNb, ClaimAmount)]),
          all(shared_history$Year %in% 2023:2026), all(shared_history$ClaimNb >= 0),
          all(shared_history$ClaimNb == floor(shared_history$ClaimNb)),
          all(is.finite(shared_history$ClaimAmount)), all(shared_history$ClaimAmount >= 0))
if (shared_history[ClaimNb == 0 & ClaimAmount != 0, .N] > 0) stop("Costs without claims: investigate.")
if (shared_history[ClaimNb > 0 & ClaimAmount <= 0, .N] > 0) stop("Zero-cost claim-years need investigation before Gamma fitting.")
shared_cat_levels <- list(
  VehicleType = c("Sedan", "Hatch", "SUV", "Ute", "Sports"),
  RiskZone = c("A_Metro", "B_InnerRegional", "C_OuterRegional", "D_Rural", "E_RemoteRural"),
  AnnualKms = c("Low", "Medium", "High"), GaragingLocation = c("Garage", "Carport", "Street"),
  VehicleUse = c("Private", "Private_Business"), PaymentFrequency = c("Annual", "Monthly"),
  ExcessChosen = c("500", "750", "1000"), NCDLevel = as.character(0:6))
shared_num <- c("DriverAge", "VehAge", "VehValue", "Density")
shared_sanitise <- function(d) {
  d <- copy(as.data.table(d))
  for (v in shared_num) set(d, j = v, value = as.numeric(as.character(d[[v]])))
  d[!is.finite(DriverAge) | DriverAge < 17 | DriverAge > 100, DriverAge := NA_real_]
  d[!is.finite(VehAge) | VehAge < 0, VehAge := NA_real_]
  d[!is.finite(VehValue) | VehValue <= 0, VehValue := NA_real_]
  d[!is.finite(Density) | Density < 0, Density := NA_real_]
  for (v in names(shared_cat_levels)) {
    x <- tolower(trimws(as.character(d[[v]])))
    x[x %in% c("", "na", "n/a", "null")] <- NA_character_
    if (v == "VehicleType") x[x == "sport"] <- "sports"
    if (v == "ExcessChosen") x[!is.na(x) & !(x %in% shared_cat_levels[[v]])] <- NA_character_
    z <- shared_cat_levels[[v]][match(x, tolower(shared_cat_levels[[v]]))]
    if (any(!is.na(x) & is.na(z))) stop("Unexpected category: ", v)
    set(d, j = v, value = z)
  }
  d
}
shared_history <- shared_sanitise(shared_history)
shared_recipe <- function(d) {
  med <- vapply(shared_num, function(v) median(d[[v]], na.rm = TRUE), numeric(1))
  modes <- vapply(names(shared_cat_levels), function(v) {
    z <- d[[v]][!is.na(d[[v]])]
    if (!length(z)) stop("All-missing categorical variable: ", v)
    names(sort(table(z), decreasing = TRUE))[1]
  }, character(1))
  if (any(!is.finite(med))) stop("All-missing numeric variable.")
  list(medians = med, modes = modes)
}
shared_prepare <- function(d, recipe) {
  d <- shared_sanitise(d)
  for (v in shared_num) set(d, which(is.na(d[[v]])), v, recipe$medians[[v]])
  for (v in names(shared_cat_levels)) {
    set(d, which(is.na(d[[v]])), v, recipe$modes[[v]])
    set(d, j = v, value = factor(d[[v]], levels = shared_cat_levels[[v]]))
  }
  # Same fixed right-closed band definitions as EDA; include zero density.
  d[, `:=`(
    DriverAgeBand = cut(DriverAge, c(-Inf,24,34,44,54,64,74,Inf), labels = c("<25","25-34","35-44","45-54","55-64","65-74","75+")),
    VehAgeBand = cut(VehAge, c(-Inf,2,5,9,14,19,Inf), labels = c("0-2","3-5","6-9","10-14","15-19","20+")),
    VehValueBand = cut(VehValue, c(-Inf,10000,15000,20000,30000,40000,60000,Inf), labels = c("<10k","10-15k","15-20k","20-30k","30-40k","40-60k","60k+")),
    DensityBand = cut(Density, c(-Inf,10,100,500,2000,Inf), labels = c("<10","10-100","100-500","500-2000","2000+")),
    YearIndex = Year - 2026)]
  d
}

library(glmnet)

# (1) Splitting Out Data ---------------------------------------------------

test_set <- copy(data[Year == 2026])
dev_pool <- copy(data[Year %in% 2023:2025])

set.seed(9)
dev_policies   <- unique(dev_pool$PolicyID)
val_policy_ids <- sample(dev_policies, size = 0.20 * length(dev_policies))

# Use the cleaned pre-imputation snapshot, rather than EDA's all-year fill.
freq_raw_dev <- shared_history[Year %in% 2023:2025]
freq_tuning_recipe <- shared_recipe(freq_raw_dev[!PolicyID %in% val_policy_ids])
tuning_train_set <- shared_prepare(freq_raw_dev[!PolicyID %in% val_policy_ids], freq_tuning_recipe)
model_selection_set <- shared_prepare(freq_raw_dev[PolicyID %in% val_policy_ids], freq_tuning_recipe)

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
inner_fold_ids <- inner_map$inner_fold[match(tuning_train_set$PolicyID, inner_map$PolicyID)]
stopifnot(!anyNA(inner_fold_ids))

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
freq_ridge_model <- freq_ridge_cv$glmnet.fit

# 4. Lasso GLM (alpha = 1)
freq_lasso_cv    <- cv.glmnet(train_mat, train_y, family = "poisson", alpha = 1, foldid = inner_fold_ids)
freq_lasso_model <- freq_lasso_cv$glmnet.fit

# 5. Elastic Net GLM (alpha grid search: 0.1 to 0.9)
alpha_grid   <- seq(0.1, 0.9, by = 0.1)
cv_enet_fits <- lapply(alpha_grid, function(a) {
  cv.glmnet(train_mat, train_y, family = "poisson", alpha = a, foldid = inner_fold_ids)
})
best_alpha_idx <- which.min(sapply(cv_enet_fits, function(fit) fit$cvm[which.min(abs(fit$lambda-fit$lambda.1se))]))
best_alpha      <- alpha_grid[best_alpha_idx]
freq_enet_cv    <- cv_enet_fits[[best_alpha_idx]]

freq_enet_model <- freq_enet_cv$glmnet.fit


# (3) Validation Set Predictions ------------------------------------------

pred_base     <- predict(freq_base_model, newdata = model_selection_set, type = "response")
pred_step_aic <- predict(freq_step_aic,   newdata = model_selection_set, type = "response")
pred_step_bic <- predict(freq_step_bic,   newdata = model_selection_set, type = "response")

pred_ridge    <- as.vector(predict(freq_ridge_model, newx = val_mat, s = freq_ridge_cv$lambda.1se, type = "response"))
pred_lasso    <- as.vector(predict(freq_lasso_model, newx = val_mat, s = freq_lasso_cv$lambda.1se, type = "response"))
pred_enet     <- as.vector(predict(freq_enet_model, newx = val_mat, s = freq_enet_cv$lambda.1se, type = "response"))

# (4) Create Comparison Table ---------------------------------------------

comparison_table <- data.table(
  Model_Architecture  = c("Base GLM", "Stepwise AIC", "Stepwise BIC", 
                          "Ridge GLM", "Lasso GLM", sprintf("Elastic Net (alpha=%.1f)", best_alpha)),
  Parameters          = c(
    length(coef(freq_base_model)),
    length(coef(freq_step_aic)),
    length(coef(freq_step_bic)),
    sum(as.matrix(coef(freq_ridge_model, s = freq_ridge_cv$lambda.1se)) != 0),
    sum(as.matrix(coef(freq_lasso_model, s = freq_lasso_cv$lambda.1se)) != 0),
    sum(as.matrix(coef(freq_enet_model, s = freq_enet_cv$lambda.1se)) != 0)
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
lasso_active <- as.matrix(coef(freq_lasso_model, s = freq_lasso_cv$lambda.1se))
print(lasso_active[lasso_active != 0, , drop = FALSE])

cat("\n--- ELASTIC NET NON-ZERO COEFFICIENTS ---\n")
enet_active <- as.matrix(coef(freq_enet_model, s = freq_enet_cv$lambda.1se))
print(enet_active[enet_active != 0, , drop = FALSE])

# Cross-validation curves for lambda selection
par(mfrow = c(1, 2))
plot(freq_lasso_cv, main = "Lasso CV: Lambda Selection")
plot(freq_ridge_cv, main = "Ridge CV: Lambda Selection")
par(mfrow = c(1, 1))


# (6) Select the validation winner rather than always using Stepwise AIC -----
freq_out <- "frequency_outputs"
dir.create(freq_out, showWarnings=FALSE)
freq_candidates <- list(
  Base=list(kind="glm",fit=freq_base_model),
  StepAIC=list(kind="glm",fit=freq_step_aic),
  StepBIC=list(kind="glm",fit=freq_step_bic),
  Ridge=list(kind="penalty",fit=freq_ridge_model,alpha=0,lambda=freq_ridge_cv$lambda.1se),
  Lasso=list(kind="penalty",fit=freq_lasso_model,alpha=1,lambda=freq_lasso_cv$lambda.1se),
  ElasticNet=list(kind="penalty",fit=freq_enet_model,alpha=best_alpha,lambda=freq_enet_cv$lambda.1se))
freq_matrix <- function(d) {
  m <- model.matrix(rating_formula,d)[,-1,drop=FALSE]
  if(!identical(colnames(m),colnames(train_mat))) stop("Frequency matrix columns differ.")
  m
}
freq_predict_selected <- function(model,d) {
  p <- if(model$kind=="glm") predict(model$fit,newdata=d,type="response") else
    predict(model$fit,newx=freq_matrix(d),s=model$lambda,type="response")
  p <- as.numeric(p)
  if(length(p)!=nrow(d) || any(!is.finite(p) | p<=0)) stop("Invalid frequency predictions.")
  p
}
freq_selection <- rbindlist(lapply(names(freq_candidates),function(nm) {
  p <- freq_predict_selected(freq_candidates[[nm]],model_selection_set)
  data.table(Model=nm,ValidationDeviance=calc_poisson_dev(model_selection_set$ClaimNb,p),
    ValidationAE=sum(model_selection_set$ClaimNb)/sum(p))
}))[order(ValidationDeviance)]
freq_selected_name <- freq_selection$Model[1]
freq_selected <- freq_candidates[[freq_selected_name]]
print(freq_selection)
cat("Selected frequency architecture:",freq_selected_name,"\n")
fwrite(freq_selection,file.path(freq_out,"01_validation_comparison.csv"))
freq_refit <- function(model,d) {
  if(model$kind=="glm") {
    m <- glm(formula(model$fit),family=poisson(link="log"),data=d,
      na.action=na.fail,control=glm.control(maxit=100))
    if(!m$converged || anyNA(coef(m))) stop("Frequency refit failed; review sparse levels/collinearity.")
    list(kind="glm",fit=m)
  } else {
    path <- sort(unique(c(model$fit$lambda,model$lambda)),decreasing=TRUE)
    m <- glmnet::glmnet(freq_matrix(d),d$ClaimNb,family="poisson",alpha=model$alpha,lambda=path)
    list(kind="penalty",fit=m,alpha=model$alpha,lambda=model$lambda)
  }
}
# Refit on ALL 2023-25 before testing 2026; keep the architecture/settings fixed.
freq_dev_recipe <- shared_recipe(freq_raw_dev)
freq_dev_all <- shared_prepare(freq_raw_dev,freq_dev_recipe)
freq_test_data <- shared_prepare(shared_history[Year==2026],freq_dev_recipe)
freq_test_model <- freq_refit(freq_selected,freq_dev_all)
test_eval <- copy(freq_test_data)
test_eval[,pred:=freq_predict_selected(freq_test_model,test_eval)]
freq_test_metrics <- data.table(Model=freq_selected_name,
  PoissonDeviance=calc_poisson_dev(test_eval$ClaimNb,test_eval$pred),
  AE_Ratio=sum(test_eval$ClaimNb)/sum(test_eval$pred))
print(freq_test_metrics)
fwrite(freq_test_metrics,file.path(freq_out,"02_2026_test_scores.csv"))
# Selected-model residuals. No normality assumption for Poisson residuals.
y <- test_eval$ClaimNb; mu <- test_eval$pred
term <- numeric(length(y)); positive <- y>0
term[positive] <- y[positive]*log(y[positive]/mu[positive])
test_eval[,DevianceResidual:=sign(y-mu)*sqrt(pmax(0,2*(term-y+mu)))]
freq_res_plot <- ggplot(test_eval,aes(log(pred),DevianceResidual))+geom_point(alpha=.1)+
  geom_hline(yintercept=0,linetype=2)+theme_minimal()+
  labs(title="2026 frequency residuals: selected model",x="Log expected claims",y="Deviance residual")
ggsave(file.path(freq_out,"03_test_residuals.png"),freq_res_plot,width=8,height=5)
# Conditional Poisson dispersion on the fitted GLM only; ridge active count
# is not a residual degrees-of-freedom estimate.
if(freq_test_model$kind=="glm") {
  phi_hat <- sum(residuals(freq_test_model$fit,type="pearson")^2)/df.residual(freq_test_model$fit)
  cat("Fitted frequency GLM Pearson dispersion:",phi_hat,"\n")
}
# Ranking diagnostic on the outer validation set for the selected candidate.
val_eval <- copy(model_selection_set)
val_eval[,pred:=freq_predict_selected(freq_selected,val_eval)]
setorder(val_eval,pred)
x <- c(0,seq_len(nrow(val_eval))/nrow(val_eval))
y <- c(0,cumsum(val_eval$ClaimNb)/sum(val_eval$ClaimNb))
gini_score <- 1-2*sum(diff(x)*(y[-1]+y[-length(y)])/2)
fwrite(data.table(ValidationFrequencyConcentration=gini_score),file.path(freq_out,"04_ranking.csv"))
# (7) Decile calibration; rank groups tolerate tied predictions.
test_eval[,decile:=pmin(10L,ceiling(frank(pred,ties.method="average")/.N*10))]
test_decile_table <- test_eval[,.(Policies=.N,Actual_Claims=sum(ClaimNb),Expected_Claims=sum(pred),
  Actual_Rate=mean(ClaimNb),Predicted_Rate=mean(pred),AE_Ratio=sum(ClaimNb)/sum(pred)),keyby=decile]
print(test_decile_table)
fwrite(test_decile_table,file.path(freq_out,"05_decile_calibration.csv"))
# For this supplied history Exposure=1, expected count equals annual frequency.
stopifnot(all(shared_history$Exposure==1))
freq_final_recipe <- shared_recipe(shared_history)
freq_final_all <- shared_prepare(shared_history,freq_final_recipe)
freq_final_model <- freq_refit(freq_selected,freq_final_all)
freq_forecast <- function(book,yr) {
  d <- copy(book); d[,Year:=yr]; d <- shared_prepare(d,freq_final_recipe)
  if(anyDuplicated(d$PolicyID)) stop("Duplicate renewal IDs.")
  data.table(PolicyID=d$PolicyID,PricingYear=yr,
    PredictedAnnualFrequency=freq_predict_selected(freq_final_model,d))
}
frequency_2027 <- freq_forecast(renewal_book_2027,2027)
frequency_2028 <- freq_forecast(renewal_book_2028,2028)
frequency_test_2026 <- data.table(PolicyID=test_eval$PolicyID,PredictedAnnualFrequency=test_eval$pred)
fwrite(frequency_2027,file.path(freq_out,"frequency_predictions_2027.csv"))
fwrite(frequency_2028,file.path(freq_out,"frequency_predictions_2028.csv"))
# Explicit frequency projection: teammate's rating formulas omit calendar year.
# We investigate frequency trends below but apply zero additional calendar trend.
# Future risk characteristics still change annual frequency predictions.
fwrite(data.table(AnnualCalendarFrequencyTrend=0,Assumption="No additional calendar trend; future risk factors are used"),
  file.path(freq_out,"06_projection_assumption.csv"))
saveRDS(list(model=freq_final_model,recipe=freq_final_recipe,rating=rating_formula,
  matrix_columns=colnames(train_mat),selected=freq_selected_name),file.path(freq_out,"frequency_model.rds"))

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

# ========================================================================
# PART C: SEVERITY MODELLING -- SIX GAMMA APPROACHES
# ========================================================================
# SEVERITY: grouped validation, stepwise GLMs and penalised Gamma models.
# APPEND BELOW YOUR EXISTING EDA. No setwd(), CSV reloading or EDA source needed.
# Run this instead of earlier severity modelling blocks.
# install.packages(c("glmnet", "sandwich"))  # once, if missing
library(data.table)
library(ggplot2)
if (!requireNamespace("glmnet", quietly = TRUE)) stop("Install glmnet first.")
if (!requireNamespace("sandwich", quietly = TRUE)) stop("Install sandwich first.")
if (packageVersion("glmnet") < "4.0") stop("Gamma family objects require glmnet >= 4.0.")
sev_out <- "severity_grouped_outputs"
dir.create(sev_out, showWarnings = FALSE)

# (0) Reuse EDA cleaning, BEFORE its all-year imputation ----------------------
# Your uploaded EDA creates this snapshot after duplicate/label/invalid-value
# cleaning, but before filling from other policy years. This preserves that work
# while preventing 2026 observations from influencing historical imputation.
if (!exists("data_before_imputation"))
  stop("Run the original EDA through creation of data_before_imputation first.")
sev_history <- copy(data_before_imputation)
stopifnot(!anyDuplicated(sev_history, by = c("PolicyID", "Year")),
          !anyNA(sev_history[, .(PolicyID, Year, ClaimNb, ClaimAmount)]),
          all(sev_history$Year %in% 2023:2026), all(sev_history$ClaimNb >= 0),
          all(sev_history$ClaimNb == floor(sev_history$ClaimNb)),
          all(is.finite(sev_history$ClaimAmount)), all(sev_history$ClaimAmount >= 0))
if (sev_history[ClaimNb == 0 & ClaimAmount != 0, .N] > 0) stop("Costs without claims: investigate.")
if (sev_history[ClaimNb > 0 & ClaimAmount <= 0, .N] > 0) stop("Zero-cost claim-years need investigation before Gamma fitting.")
sev_cat_levels <- list(
  VehicleType = c("Sedan", "Hatch", "SUV", "Ute", "Sports"),
  RiskZone = c("A_Metro", "B_InnerRegional", "C_OuterRegional", "D_Rural", "E_RemoteRural"),
  AnnualKms = c("Low", "Medium", "High"), GaragingLocation = c("Garage", "Carport", "Street"),
  VehicleUse = c("Private", "Private_Business"), PaymentFrequency = c("Annual", "Monthly"),
  ExcessChosen = c("500", "750", "1000"), NCDLevel = as.character(0:6))
sev_num <- c("DriverAge", "VehAge", "VehValue", "Density")
sev_sanitise <- function(d) {
  d <- copy(as.data.table(d))
  for (v in sev_num) set(d, j = v, value = as.numeric(as.character(d[[v]])))
  d[!is.finite(DriverAge) | DriverAge < 17 | DriverAge > 100, DriverAge := NA_real_]
  d[!is.finite(VehAge) | VehAge < 0, VehAge := NA_real_]
  d[!is.finite(VehValue) | VehValue <= 0, VehValue := NA_real_]
  d[!is.finite(Density) | Density < 0, Density := NA_real_]
  for (v in names(sev_cat_levels)) {
    x <- tolower(trimws(as.character(d[[v]])))
    x[x %in% c("", "na", "n/a", "null")] <- NA_character_
    if (v == "VehicleType") x[x == "sport"] <- "sports"
    if (v == "ExcessChosen") x[!is.na(x) & !(x %in% sev_cat_levels[[v]])] <- NA_character_
    z <- sev_cat_levels[[v]][match(x, tolower(sev_cat_levels[[v]]))]
    if (any(!is.na(x) & is.na(z))) stop("Unexpected category: ", v)
    set(d, j = v, value = z)
  }
  d
}
sev_history <- sev_sanitise(sev_history)
sev_recipe <- function(d) {
  med <- vapply(sev_num, function(v) median(d[[v]], na.rm = TRUE), numeric(1))
  modes <- vapply(names(sev_cat_levels), function(v) {
    z <- d[[v]][!is.na(d[[v]])]
    if (!length(z)) stop("All-missing categorical variable: ", v)
    names(sort(table(z), decreasing = TRUE))[1]
  }, character(1))
  if (any(!is.finite(med))) stop("All-missing numeric variable.")
  list(medians = med, modes = modes)
}
sev_prepare <- function(d, recipe) {
  d <- sev_sanitise(d)
  for (v in sev_num) set(d, which(is.na(d[[v]])), v, recipe$medians[[v]])
  for (v in names(sev_cat_levels)) {
    set(d, which(is.na(d[[v]])), v, recipe$modes[[v]])
    set(d, j = v, value = factor(d[[v]], levels = sev_cat_levels[[v]]))
  }
  # Same fixed right-closed band definitions as EDA; include zero density.
  d[, `:=`(
    DriverAgeBand = cut(DriverAge, c(-Inf,24,34,44,54,64,74,Inf), labels = c("<25","25-34","35-44","45-54","55-64","65-74","75+")),
    VehAgeBand = cut(VehAge, c(-Inf,2,5,9,14,19,Inf), labels = c("0-2","3-5","6-9","10-14","15-19","20+")),
    VehValueBand = cut(VehValue, c(-Inf,10000,15000,20000,30000,40000,60000,Inf), labels = c("<10k","10-15k","15-20k","20-30k","30-40k","40-60k","60k+")),
    DensityBand = cut(Density, c(-Inf,10,100,500,2000,Inf), labels = c("<10","10-100","100-500","500-2000","2000+")),
    YearIndex = Year - 2026)]
  d
}

# (1) Policy-grouped development split; hold out 2026 -------------------------
# Match teammate's PolicyID ordering and seed when using the same EDA `data`.
sev_dev_ids <- unique(data[Year %in% 2023:2025, PolicyID])
set.seed(9)
sev_val_ids <- sample(sev_dev_ids, size = floor(.20*length(sev_dev_ids)))
sev_tune_raw <- sev_history[Year <= 2025 & !PolicyID %in% sev_val_ids]
sev_val_raw <- sev_history[Year <= 2025 & PolicyID %in% sev_val_ids]
sev_initial_recipe <- sev_recipe(sev_tune_raw)
sev_tune_all <- sev_prepare(sev_tune_raw, sev_initial_recipe)
sev_val_all <- sev_prepare(sev_val_raw, sev_initial_recipe)
sev_tune <- copy(sev_tune_all[ClaimNb > 0]); sev_tune[, AverageClaim := ClaimAmount/ClaimNb]
sev_val <- copy(sev_val_all[ClaimNb > 0]); sev_val[, AverageClaim := ClaimAmount/ClaimNb]
if (!nrow(sev_tune) || !nrow(sev_val)) stop("Split contains no claim-years.")
# Same rating factors as frequency; YearIndex is required in every model.
sev_rating <- ~ DriverAgeBand + VehAgeBand + VehValueBand + VehicleType + RiskZone +
  DensityBand + AnnualKms + GaragingLocation + VehicleUse + PaymentFrequency + ExcessChosen + NCDLevel + YearIndex
sev_formula <- update(sev_rating, AverageClaim ~ .)
sev_factor_cols <- setdiff(all.vars(sev_rating), "YearIndex")
# An ordinary GLM cannot estimate a category with no training claim-years.
for (v in sev_factor_cols) {
  unseen <- setdiff(unique(as.character(sev_val[[v]])), unique(as.character(sev_tune[[v]])))
  if (length(unseen)) stop("Validation category absent from training claim-years: ", v, " ", paste(unseen, collapse=", "), ". Consider justified pooling.")
}
sev_train_x <- model.matrix(sev_rating, data = sev_tune)[, -1, drop = FALSE]
sev_x_names <- colnames(sev_train_x)
sev_matrix <- function(d) {
  m <- model.matrix(sev_rating, data = d)[, -1, drop = FALSE]
  if (!identical(colnames(m), sev_x_names)) stop("Design-matrix columns differ.")
  m
}
sev_val_x <- sev_matrix(sev_val)
sev_penalty <- rep(1, ncol(sev_train_x)); sev_penalty[sev_x_names == "YearIndex"] <- 0
# YearIndex remains unpenalised: shrinking risk effects must not erase inflation.
# Preserve row order with match(), rather than assuming a merge preserves it.
sev_inner_policies <- unique(sev_tune_all$PolicyID)
set.seed(10)
sev_inner_map <- data.table(PolicyID = sev_inner_policies,
  Fold = sample(rep(1:10, length.out = length(sev_inner_policies))))
sev_foldid <- sev_inner_map$Fold[match(sev_tune$PolicyID, sev_inner_map$PolicyID)]
stopifnot(!anyNA(sev_foldid), uniqueN(sev_foldid) == 10)
fwrite(data.table(PolicyID = sev_dev_ids, Split = ifelse(sev_dev_ids %in% sev_val_ids, "Validation", "TuningTrain")),
       file.path(sev_out,"01_policy_split.csv"))
fwrite(sev_inner_map, file.path(sev_out,"01_inner_folds.csv"))
# Inner CV uses a recipe learned on the whole tuning subset. Outer validation
# and 2026 outcomes remain excluded. Strict fold-local imputation would require
# a custom CV loop; report this limitation if missingness is material.

# (2) Fit base, stepwise AIC/BIC, ridge, lasso and elastic net -----------------
sev_base <- glm(sev_formula, family = Gamma(link = "log"), weights = ClaimNb,
                data = sev_tune, na.action = na.fail, control = glm.control(maxit=100))
if (!sev_base$converged || anyNA(coef(sev_base))) stop("Base GLM failed/aliased; review sparse bands or redundant factors.")
sev_scope <- list(lower = AverageClaim ~ YearIndex, upper = sev_formula)
sev_aic <- step(sev_base, scope = sev_scope, direction = "both", trace = 0, k = 2)
sev_bic <- step(sev_base, scope = sev_scope, direction = "both", trace = 0, k = log(nrow(sev_tune)))
# For weighted aggregated Gamma data, BIC uses claim-year row count as an
# approximate sample size. AIC/BIC are selection heuristics; validation decides.
sev_cv <- function(alpha) glmnet::cv.glmnet(
  x = sev_train_x, y = sev_tune$AverageClaim, weights = sev_tune$ClaimNb,
  family = Gamma(link = "log"), alpha = alpha, foldid = sev_foldid,
  type.measure = "deviance", penalty.factor = sev_penalty, standardize = TRUE)
cat("Tuning ridge and lasso Gamma GLMs...\n")
sev_ridge_cv <- sev_cv(0)
sev_lasso_cv <- sev_cv(1)
sev_alpha_grid <- seq(.1,.9,.1)
cat("Tuning elastic net (nine alpha values)...\n")
sev_enet_cvs <- lapply(sev_alpha_grid, sev_cv)
# Compare alpha at the SAME one-SE rule used for deployment.
sev_cv_at_1se <- function(cv) cv$cvm[which.min(abs(cv$lambda-cv$lambda.1se))]
sev_alpha_index <- which.min(vapply(sev_enet_cvs, sev_cv_at_1se, numeric(1)))
sev_alpha <- sev_alpha_grid[sev_alpha_index]
sev_enet_cv <- sev_enet_cvs[[sev_alpha_index]]
sev_models <- list(
  Base = list(kind="glm", fit=sev_base),
  StepAIC = list(kind="glm", fit=sev_aic),
  StepBIC = list(kind="glm", fit=sev_bic),
  Ridge = list(kind="penalty", fit=sev_ridge_cv$glmnet.fit, lambda=sev_ridge_cv$lambda.1se, alpha=0),
  Lasso = list(kind="penalty", fit=sev_lasso_cv$glmnet.fit, lambda=sev_lasso_cv$lambda.1se, alpha=1),
  ElasticNet = list(kind="penalty", fit=sev_enet_cv$glmnet.fit, lambda=sev_enet_cv$lambda.1se, alpha=sev_alpha))
sev_predict <- function(model, d) {
  z <- if (model$kind == "glm") predict(model$fit,newdata=d,type="response") else
    predict(model$fit,newx=sev_matrix(d),s=model$lambda,type="response")
  z <- as.numeric(z)
  if (length(z) != nrow(d) || any(!is.finite(z) | z<=0)) stop("Invalid severity prediction.")
  z
}
sev_coef <- function(model) {
  if (model$kind=="glm") coef(model$fit) else {
    z <- as.matrix(coef(model$fit,s=model$lambda))
    setNames(as.numeric(z[,1]),rownames(z))
  }
}
sev_dev <- function(y,mu) pmax(0, 2*(y/mu-1-log(y/mu)))
sev_score <- function(model,d) {
  p <- sev_predict(model,d)
  data.table(WeightedGammaDeviance = weighted.mean(sev_dev(d$AverageClaim,p),d$ClaimNb),
    WeightedMAE = weighted.mean(abs(d$AverageClaim-p),d$ClaimNb),
    ActualSeverity = sum(d$ClaimAmount)/sum(d$ClaimNb),
    PredictedSeverity = weighted.mean(p,d$ClaimNb),
    AE_Ratio = sum(d$ClaimAmount)/sum(d$ClaimNb*p))
}

# (3)-(4) Validation predictions and comparison; choose actual winner --------
sev_comparison <- rbindlist(lapply(names(sev_models),function(nm) {
  m <- sev_models[[nm]]; b <- sev_coef(m)
  cbind(data.table(Model=nm, NonzeroCoefficients=sum(abs(b)>1e-10),
    Alpha=if(m$kind=="penalty") m$alpha else NA_real_,
    Lambda=if(m$kind=="penalty") m$lambda else NA_real_),sev_score(m,sev_val))
}))[order(WeightedGammaDeviance)]
print(sev_comparison)
fwrite(sev_comparison,file.path(sev_out,"02_validation_comparison.csv"))
# Select by unrounded deviance; simpler model breaks exact ties.
sev_selected_name <- sev_comparison[order(WeightedGammaDeviance,NonzeroCoefficients),Model][1]
sev_selected_tuning <- sev_models[[sev_selected_name]]
cat("Selected severity architecture:",sev_selected_name,"\n")
# NonzeroCoefficients is NOT effective degrees of freedom for ridge.

# (5) View fitted models and cross-validation curves ------------------------
print(summary(sev_base)); print(summary(sev_aic)); print(summary(sev_bic))
for(nm in c("Ridge","Lasso","ElasticNet")) {
  b <- sev_coef(sev_models[[nm]])
  cat("\n",nm,"nonzero coefficients:\n"); print(b[abs(b)>1e-10])
}
pdf(file.path(sev_out,"03_penalty_cv_curves.pdf"),width=10,height=5)
par(mfrow=c(1,2)); plot(sev_lasso_cv,main="Severity Lasso CV"); plot(sev_ridge_cv,main="Severity Ridge CV")
dev.off()

# (6) LOCK specification/hyperparameters; refit 2023-25, test on 2026 ---------
sev_refit <- function(model,d) {
  if(model$kind=="glm") {
    fit <- glm(formula(model$fit),family=Gamma(link="log"),weights=ClaimNb,
      data=d,na.action=na.fail,control=glm.control(maxit=100))
    if(!fit$converged || anyNA(coef(fit))) stop("GLM refit failed.")
    list(kind="glm",fit=fit)
  } else {
    # Fit a full path including the locked lambda, rather than one isolated value.
    path <- sort(unique(c(model$fit$lambda,model$lambda)),decreasing=TRUE)
    fit <- glmnet::glmnet(sev_matrix(d),d$AverageClaim,weights=d$ClaimNb,
      family=Gamma(link="log"),alpha=model$alpha,lambda=path,
      penalty.factor=sev_penalty,standardize=TRUE)
    list(kind="penalty",fit=fit,lambda=model$lambda,alpha=model$alpha)
  }
}
sev_dev_recipe <- sev_recipe(sev_history[Year<=2025])
sev_dev_all <- sev_prepare(sev_history[Year<=2025],sev_dev_recipe)
sev_dev_claims <- copy(sev_dev_all[ClaimNb>0]); sev_dev_claims[,AverageClaim:=ClaimAmount/ClaimNb]
sev_test_all <- sev_prepare(sev_history[Year==2026],sev_dev_recipe)
sev_test_claims <- copy(sev_test_all[ClaimNb>0]); sev_test_claims[,AverageClaim:=ClaimAmount/ClaimNb]
sev_test_model <- sev_refit(sev_selected_tuning,sev_dev_claims)
sev_test_baseline <- list(kind="glm",fit=glm(AverageClaim~YearIndex,
  family=Gamma(link="log"),weights=ClaimNb,data=sev_dev_claims))
sev_test_scores <- rbindlist(list(cbind(Model=sev_selected_name,sev_score(sev_test_model,sev_test_claims)),
  cbind(Model="YearOnlyBenchmark",sev_score(sev_test_baseline,sev_test_claims))))
print(sev_test_scores); fwrite(sev_test_scores,file.path(sev_out,"04_2026_test_scores.csv"))
sev_test_claims[,PredictedSeverity:=sev_predict(sev_test_model,sev_test_claims)]
sev_test_claims[,DevianceResidual:=sign(AverageClaim-PredictedSeverity)*
  sqrt(ClaimNb*sev_dev(AverageClaim,PredictedSeverity))]
sev_res_plot <- ggplot(sev_test_claims,aes(log(PredictedSeverity),DevianceResidual))+
  geom_point(alpha=.15)+geom_hline(yintercept=0,linetype=2)+theme_minimal()+
  labs(title="2026 severity residuals: selected model",x="Log predicted severity",y="Weighted deviance residual")
ggsave(file.path(sev_out,"05_test_residuals.png"),sev_res_plot,width=8,height=5)
# Gamma residuals need not be normally distributed. Normal QQ is not a pass/fail test.

# (7) Severity calibration by decile and segment -----------------------------
# Rank-based groups avoid cut(quantile(...)) errors with tied predictions.
sev_test_claims[,Decile:=pmin(10L,ceiling(frank(PredictedSeverity,ties.method="average")/.N*10))]
sev_calibrate <- function(d,group) {
  z <- d[, .(ClaimYears=.N,Claims=sum(ClaimNb),ActualCost=sum(ClaimAmount),
    ExpectedConditionalCost=sum(ClaimNb*PredictedSeverity)),by=group]
  z[,`:=`(ActualSeverity=ActualCost/Claims,PredictedSeverity=ExpectedConditionalCost/Claims,
    AE_Ratio=ActualCost/ExpectedConditionalCost)]
  z
}
for(g in c("Decile","VehicleType","RiskZone","NCDLevel","PaymentFrequency","ClaimNb"))
  fwrite(sev_calibrate(sev_test_claims,g),file.path(sev_out,paste0("06_calibration_",g,".csv")))
sev_deciles <- sev_calibrate(sev_test_claims,"Decile")[order(Decile)]
print(sev_deciles)
sev_cal_plot <- ggplot(melt(sev_deciles,id.vars="Decile",measure.vars=c("ActualSeverity","PredictedSeverity")),
  aes(Decile,value,colour=variable))+geom_line()+geom_point()+theme_minimal()+
  labs(title="2026 severity calibration",y="Dollars per claim",colour=NULL)
ggsave(file.path(sev_out,"06_decile_calibration.png"),sev_cal_plot,width=8,height=5)
# Severity concentration diagnostic: cumulative CLAIMS vs cumulative COST,
# ordered by predicted severity. This is not a standard insurance pure-premium Gini.
sev_rank <- copy(sev_test_claims); setorder(sev_rank,PredictedSeverity)
sev_lx <- c(0,cumsum(sev_rank$ClaimNb)/sum(sev_rank$ClaimNb))
sev_ly <- c(0,cumsum(sev_rank$ClaimAmount)/sum(sev_rank$ClaimAmount))
sev_concentration <- 1-2*sum(diff(sev_lx)*(sev_ly[-1]+sev_ly[-length(sev_ly)])/2)
fwrite(data.table(SeverityConcentration=sev_concentration),file.path(sev_out,"07_severity_concentration.csv"))
# Claim-count-weighted conditional cost is a diagnostic; future price needs PREDICTED frequency.

# (8) Raw and mix-adjusted severity trends -----------------------------------
# Descriptive trend investigation uses all history after locked test reporting.
sev_final_recipe <- sev_recipe(sev_history)
sev_final_all <- sev_prepare(sev_history,sev_final_recipe)
sev_final_claims <- copy(sev_final_all[ClaimNb>0]); sev_final_claims[,AverageClaim:=ClaimAmount/ClaimNb]
sev_raw_trend <- glm(AverageClaim~YearIndex,family=Gamma(link="log"),weights=ClaimNb,data=sev_final_claims)
sev_adj_trend <- glm(sev_formula,family=Gamma(link="log"),weights=ClaimNb,data=sev_final_claims)
sev_extract_trend <- function(m,label) {
  b <- coef(m)["YearIndex"]
  v <- sandwich::vcovCL(m,cluster=sev_final_claims$PolicyID,type="HC1")
  se <- sqrt(v["YearIndex","YearIndex"])
  data.table(Model=label,AnnualChangePct=100*expm1(b),
    Lower95Pct=100*expm1(b-1.96*se),Upper95Pct=100*expm1(b+1.96*se),
    PValue=2*pnorm(-abs(b/se)))
}
sev_trend_table <- rbindlist(list(sev_extract_trend(sev_raw_trend,"Raw"),sev_extract_trend(sev_adj_trend,"MixAdjusted")))
print(sev_trend_table); fwrite(sev_trend_table,file.path(sev_out,"08_severity_trends.csv"))
# Cluster intervals above apply to the diagnostic GLMs, not penalised coefficients.
sev_year_actual <- sev_final_claims[,.(Claims=sum(ClaimNb),Cost=sum(ClaimAmount)),by=Year][order(Year)]
sev_year_actual[,Severity:=Cost/Claims]
fwrite(sev_year_actual,file.path(sev_out,"08_actual_severity_by_year.csv"))

# (9) Final refit and 2027/2028 severity forecasts ----------------------------
sev_final_model <- sev_refit(sev_selected_tuning,sev_final_claims)
sev_final_b <- sev_coef(sev_final_model)
sev_year_b <- unname(sev_final_b["YearIndex"])
if(length(sev_year_b)!=1 || !is.finite(sev_year_b)) stop("Missing final year coefficient.")
sev_projection <- data.table(SelectedModel=sev_selected_name,
  AnnualSeverityInflation=expm1(sev_year_b),Factor2027=exp(sev_year_b),Factor2028=exp(2*sev_year_b))
print(sev_projection); fwrite(sev_projection,file.path(sev_out,"09_inflation_assumption.csv"))
# Assumption: final fitted log-linear severity trend continues through 2028.
# YearIndex already applies inflation; DO NOT apply another inflation multiplier.
sev_forecast <- function(book,yr) {
  d <- copy(book); d[,Year:=yr]; d <- sev_prepare(d,sev_final_recipe)
  if(anyDuplicated(d$PolicyID)) stop("Duplicate renewal IDs.")
  for(v in sev_factor_cols) {
    bad <- setdiff(unique(as.character(d[[v]])),unique(as.character(sev_final_claims[[v]])))
    if(length(bad)) stop("Renewal category without historical claim support: ",v," ",paste(bad,collapse=", "))
  }
  p <- sev_predict(sev_final_model,d)
  base <- copy(d); base[,`:=`(Year=2026,YearIndex=0)]
  data.table(PolicyID=d$PolicyID,PricingYear=yr,PredictedSeverity=p,
    Severity_2026Level=sev_predict(sev_final_model,base))
}
severity_2027 <- sev_forecast(renewal_book_2027,2027)
severity_2028 <- sev_forecast(renewal_book_2028,2028)
fwrite(severity_2027,file.path(sev_out,"severity_predictions_2027.csv"))
fwrite(severity_2028,file.path(sev_out,"severity_predictions_2028.csv"))
# Handoff for honest JOINT holdout testing: all 2026 policies using pre-2026 model.
severity_test_2026 <- data.table(PolicyID=sev_test_all$PolicyID,
  PredictedSeverity=sev_predict(sev_test_model,sev_test_all))
fwrite(severity_test_2026,file.path(sev_out,"severity_holdout_2026.csv"))
saveRDS(list(model=sev_final_model,recipe=sev_final_recipe,selected=sev_selected_name,
  matrix_columns=sev_x_names,rating_formula=sev_rating,projection=sev_projection,
  validation=sev_comparison,test=sev_test_scores),file.path(sev_out,"severity_model.rds"))
fwrite(data.table(Term=names(sev_final_b),Coefficient=as.numeric(sev_final_b),Multiplier=exp(sev_final_b)),
  file.path(sev_out,"09_final_coefficients.csv"))
capture.output(sessionInfo(),file=file.path(sev_out,"session_info.txt"))
cat("Done. Join severity_2027/2028 to annual frequency predictions by PolicyID.\n")
cat("TechnicalPrice = PredictedAnnualFrequency * PredictedSeverity.\n")

# ========================================================================
# PART D: COMBINE FREQUENCY AND SEVERITY -- TECHNICAL CLAIMS COST ONLY
# ========================================================================
combined_out <- "technical_price_outputs"
dir.create(combined_out,showWarnings=FALSE)
combine_components <- function(frequency,severity) {
  stopifnot(!anyDuplicated(frequency$PolicyID),!anyDuplicated(severity$PolicyID),
    setequal(frequency$PolicyID,severity$PolicyID))
  keys <- if("PricingYear" %in% names(frequency) && "PricingYear" %in% names(severity))
    c("PolicyID","PricingYear") else "PolicyID"
  result <- merge(frequency,severity,by=keys,all=TRUE)
  stopifnot(nrow(result)==nrow(frequency),!anyNA(result$PredictedAnnualFrequency),
    !anyNA(result$PredictedSeverity))
  result[,TechnicalPrice:=PredictedAnnualFrequency*PredictedSeverity]
  result
}
technical_2027 <- combine_components(frequency_2027,severity_2027)
technical_2028 <- combine_components(frequency_2028,severity_2028)
fwrite(technical_2027,file.path(combined_out,"technical_prices_2027.csv"))
fwrite(technical_2028,file.path(combined_out,"technical_prices_2028.csv"))
# Honest joint 2026 holdout: both component models were fitted through 2025 only.
joint_test_2026 <- combine_components(frequency_test_2026,severity_test_2026)
joint_test_2026 <- merge(joint_test_2026,shared_history[Year==2026,.(PolicyID,ClaimAmount)],by="PolicyID",all.x=TRUE)
stopifnot(!anyNA(joint_test_2026$ClaimAmount))
joint_test_2026[,RiskDecile:=pmin(10L,ceiling(frank(TechnicalPrice,ties.method="average")/.N*10))]
joint_calibration <- joint_test_2026[,.(Policies=.N,ActualCost=sum(ClaimAmount),PredictedCost=sum(TechnicalPrice),
  ActualLossCost=mean(ClaimAmount),PredictedLossCost=mean(TechnicalPrice),
  AE_Ratio=sum(ClaimAmount)/sum(TechnicalPrice)),keyby=RiskDecile]
fwrite(joint_calibration,file.path(combined_out,"2026_joint_calibration.csv"))
fwrite(data.table(CostAE=sum(joint_test_2026$ClaimAmount)/sum(joint_test_2026$TechnicalPrice)),
  file.path(combined_out,"2026_joint_test_score.csv"))
print(joint_calibration)
cat("FINISHED: technical_2027 and technical_2028 contain annual expected claims costs.\n")
cat("Commercial loadings, promised discounts and cap/collar rules belong to later tasks.\n")


# ========================================================================
# PART E: Severity Modeling Report Stats
# ========================================================================

# REPORT SECTION 1: OBJECTIVE, DATA AND SPLITS ------------------------------

required_objects <- c(
  "sev_history",
  "sev_tune_all", "sev_val_all",
  "sev_tune", "sev_val",
  "sev_dev_claims",
  "sev_test_all", "sev_test_claims"
)

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1), inherits = TRUE)
]

if (length(missing_objects) > 0) {
  stop(
    "Run Part C first. Missing objects: ",
    paste(missing_objects, collapse = ", ")
  )
}

# Summarise a dataset before filtering to positive-claim policy-years.
section1_summary <- function(d, label) {
  positive <- d[d$ClaimNb > 0, , drop = FALSE]
  
  data.frame(
    Dataset = label,
    Years = paste(sort(unique(d$Year)), collapse = ", "),
    Policies = length(unique(d$PolicyID)),
    PolicyYears = nrow(d),
    PositiveClaimPolicyYears = nrow(positive),
    Claims = sum(positive$ClaimNb),
    TotalClaimCost = sum(positive$ClaimAmount),
    AverageCostPerClaim = if (sum(positive$ClaimNb) > 0) {
      sum(positive$ClaimAmount) / sum(positive$ClaimNb)
    } else {
      NA_real_
    }
  )
}

section1_data_table <- rbind(
  section1_summary(sev_history,  "Full history"),
  section1_summary(sev_tune_all, "Training"),
  section1_summary(sev_val_all,  "Validation"),
  section1_summary(sev_test_all, "2026 test")
)

cat("\n--- 1A. DATA AND SPLIT SUMMARY ---\n")
print(section1_data_table, row.names = FALSE, digits = 6)

# Check that training and validation use separate policies.
section1_overlap <- intersect(
  unique(sev_tune$PolicyID),
  unique(sev_val$PolicyID)
)

cat("\n--- 1B. TRAINING / VALIDATION POLICY OVERLAP ---\n")
cat("Number of overlapping policies:", length(section1_overlap), "\n")

# Check that severity responses and weights are valid.
section1_check <- function(d, label) {
  data.frame(
    Dataset = label,
    Rows = nrow(d),
    InvalidClaimCounts = sum(
      !is.finite(d$ClaimNb) | d$ClaimNb <= 0
    ),
    InvalidAverageClaims = sum(
      !is.finite(d$AverageClaim) | d$AverageClaim <= 0
    ),
    ResponseMatchesCostPerClaim = isTRUE(all.equal(
      as.numeric(d$AverageClaim),
      as.numeric(d$ClaimAmount / d$ClaimNb),
      tolerance = 1e-8
    ))
  )
}

section1_checks <- rbind(
  section1_check(sev_tune,       "Training"),
  section1_check(sev_val,        "Validation"),
  section1_check(sev_dev_claims, "2023–2025 refit"),
  section1_check(sev_test_claims,"2026 test")
)

cat("\n--- 1C. RESPONSE AND WEIGHT CHECKS ---\n")
print(section1_checks, row.names = FALSE)

# Inspect the distribution of observed average claim sizes.
section1_distribution <- rbind(
  Training = quantile(
    sev_tune$AverageClaim,
    probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
    na.rm = TRUE
  ),
  Validation = quantile(
    sev_val$AverageClaim,
    probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
    na.rm = TRUE
  ),
  Test2026 = quantile(
    sev_test_claims$AverageClaim,
    probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
    na.rm = TRUE
  )
)

cat("\n--- 1D. AVERAGE CLAIM SIZE DISTRIBUTION ---\n")
print(round(section1_distribution, 2))

# REPORT SECTION 2: ------------------------------

s2_required <- c("sev_formula", "sev_models", "sev_tune", "sev_foldid",
                 "sev_alpha_grid", "sev_x_names", "sev_penalty",
                 "sev_initial_recipe")
s2_missing <- s2_required[!vapply(s2_required, exists, logical(1), inherits = TRUE)]
if (length(s2_missing)) stop("Run Part C first. Missing: ", paste(s2_missing, collapse = ", "))

cat("\n--- 2A. RESPONSE, FAMILY AND FULL RATING FORMULA ---\n")
cat("Response: AverageClaim = ClaimAmount / ClaimNb\n")
cat("Observation weights: ClaimNb\n")
cat("Family:", sev_models$Base$fit$family$family,
    "| Link:", sev_models$Base$fit$family$link, "\n")
cat("Exposure offset: none\n")
cat("YearIndex = Year - 2026; included in every candidate\n")
print(sev_formula)

cat("\n--- 2B. SIX CANDIDATE MODELS ---\n")
s2_candidates <- data.frame(
  Model = c("Base", "StepAIC", "StepBIC", "Ridge", "Lasso", "ElasticNet"),
  Approach = c("All rating terms", "Both-direction stepwise AIC",
               "Both-direction stepwise BIC", "Shrink coefficients: alpha = 0",
               "Shrink/select coefficients: alpha = 1",
               "Mix ridge and lasso: alpha = 0.1 to 0.9"),
  YearTreatment = rep("Retained; unpenalised", 6)
)
print(s2_candidates, row.names = FALSE)

cat("\n--- 2C. ORDINARY GLM FIT CHECKS ---\n")
s2_glms <- c("Base", "StepAIC", "StepBIC")
s2_fit_checks <- do.call(rbind, lapply(s2_glms, function(nm) {
  fit <- sev_models[[nm]]$fit
  data.frame(Model = nm, Converged = fit$converged,
             Observations = nobs(fit),
             CoefficientsIncludingIntercept = length(coef(fit)),
             MissingCoefficients = sum(is.na(coef(fit))),
             YearIndexRetained = "YearIndex" %in% names(coef(fit)))
}))
print(s2_fit_checks, row.names = FALSE)
cat("\nFormulas retained by ordinary GLMs:\n")
for (nm in s2_glms) {
  cat("\n", nm, "\n", sep = "")
  print(formula(sev_models[[nm]]$fit))
}

cat("\n--- 2D. POLICY-GROUPED INNER CROSS-VALIDATION ---\n")
s2_fold_rows <- data.frame(PolicyID = sev_tune$PolicyID, Fold = sev_foldid)
s2_fold_table <- do.call(rbind, lapply(sort(unique(sev_foldid)), function(k) {
  rows <- which(sev_foldid == k)
  data.frame(Fold = k, ClaimingPolicyYears = length(rows),
             PoliciesWithClaims = length(unique(sev_tune$PolicyID[rows])),
             Claims = sum(sev_tune$ClaimNb[rows]))
}))
print(s2_fold_table, row.names = FALSE)
s2_folds_per_policy <- vapply(split(s2_fold_rows$Fold, s2_fold_rows$PolicyID),
                              function(x) length(unique(x)), integer(1))
cat("Number of folds:", length(unique(sev_foldid)), "\n")
cat("Policies assigned to multiple folds:", sum(s2_folds_per_policy > 1), "\n")
cat("Missing fold assignments:", sum(is.na(sev_foldid)), "\n")
cat("Elastic-net alpha grid:", paste(sev_alpha_grid, collapse = ", "), "\n")
cat("Lambda choice: lambda.1se; CV metric: weighted Gamma deviance\n")
cat("YearIndex penalty factor:", sev_penalty[sev_x_names == "YearIndex"], "\n")

cat("\n--- 2E. TRAINING-LEARNED IMPUTATION VALUES ---\n")
print(sev_initial_recipe)
cat("\nOuter validation and 2026 outcomes are excluded from this recipe.\n")
cat("Inner CV reuses the recipe fitted to the whole training subset;\n")
cat("imputation is not refitted separately within each inner fold.\n")

# REPORT SECTION 3: ------------------------------

s3_required <- c("sev_ridge_cv", "sev_lasso_cv", "sev_enet_cv",
                 "sev_enet_cvs", "sev_alpha_grid", "sev_alpha", "sev_comparison",
                 "sev_selected_name", "sev_selected_tuning")
s3_missing <- s3_required[!vapply(s3_required, exists, logical(1), inherits = TRUE)]
if (length(s3_missing)) stop("Run Part C first. Missing: ", paste(s3_missing, collapse = ", "))

# 3A: actual tuning results for each penalised candidate.
s3_cv_row <- function(cv, label, alpha) {
  idx <- which.min(abs(cv$lambda - cv$lambda.1se))
  data.frame(Model = label, Alpha = alpha,
             LambdaMin = cv$lambda.min, Lambda1SE = cv$lambda.1se,
             CVDevianceAtMin = min(cv$cvm), CVDevianceAt1SE = cv$cvm[idx],
             CVStandardErrorAt1SE = cv$cvsd[idx])
}
s3_tuning <- rbind(
  s3_cv_row(sev_ridge_cv, "Ridge", 0),
  s3_cv_row(sev_lasso_cv, "Lasso", 1),
  s3_cv_row(sev_enet_cv, "ElasticNet", sev_alpha)
)
cat("\n--- 3A. PENALTY TUNING RESULTS ---\n")
print(s3_tuning, row.names = FALSE, digits = 7)

# 3B: show why the chosen elastic-net alpha was used.
s3_alpha_results <- do.call(rbind, lapply(seq_along(sev_alpha_grid), function(i) {
  z <- s3_cv_row(sev_enet_cvs[[i]], "ElasticNet", sev_alpha_grid[i])
  z$ChosenAlpha <- sev_alpha_grid[i] == sev_alpha
  z
}))
cat("\n--- 3B. ELASTIC-NET ALPHA COMPARISON ---\n")
print(s3_alpha_results[, c("Alpha", "Lambda1SE", "CVDevianceAt1SE", "ChosenAlpha")],
      row.names = FALSE, digits = 7)

# 3C: outer validation metrics; selection uses unrounded deviance.
s3_ranking <- as.data.frame(sev_comparison)
s3_ranking <- s3_ranking[order(s3_ranking$WeightedGammaDeviance,
                               s3_ranking$NonzeroCoefficients), ]
s3_ranking$Rank <- seq_len(nrow(s3_ranking))
s3_ranking$Chosen <- s3_ranking$Model == sev_selected_name
s3_columns <- c("Rank", "Model", "NonzeroCoefficients",
                "WeightedGammaDeviance", "WeightedMAE", "ActualSeverity",
                "PredictedSeverity", "AE_Ratio", "Chosen")
cat("\n--- 3C. VALIDATION MODEL COMPARISON ---\n")
print(s3_ranking[, s3_columns], row.names = FALSE, digits = 7)
cat("Nonzero coefficient counts include the intercept and are not ridge effective degrees of freedom.\n")

# 3D: quantify the selected model's advantage without assuming it is large.
s3_best <- s3_ranking[s3_ranking$Chosen, , drop = FALSE]
s3_base <- s3_ranking[s3_ranking$Model == "Base", , drop = FALSE]
s3_runner <- s3_ranking[2, , drop = FALSE]
s3_summary <- data.frame(
  SelectedModel = sev_selected_name,
  ValidationDeviance = s3_best$WeightedGammaDeviance,
  ValidationAE = s3_best$AE_Ratio,
  ImprovementVsBasePct = 100 * (1 - s3_best$WeightedGammaDeviance /
                                  s3_base$WeightedGammaDeviance),
  RunnerUp = s3_runner$Model,
  ImprovementVsRunnerUpPct = 100 * (1 - s3_best$WeightedGammaDeviance /
                                      s3_runner$WeightedGammaDeviance)
)
cat("\n--- 3D. CHOSEN MODEL AND VALIDATION ADVANTAGE ---\n")
print(s3_summary, row.names = FALSE, digits = 7)
cat("Selection rule: lowest unrounded validation weighted Gamma deviance;\n")
cat("fewer nonzero coefficients breaks exact ties.\n")
cat("A/E = actual cost / sum(observed ClaimNb * predicted severity).\n")
cat("A/E > 1 indicates underprediction; A/E < 1 indicates overprediction.\n")
cat("These are validation results; 2026 test results are reported separately.\n")
if (sev_selected_tuning$kind == "glm") {
  cat("\nSelected candidate formula:\n")
  print(formula(sev_selected_tuning$fit))
} else {
  cat("\nSelected candidate alpha:", sev_selected_tuning$alpha,
      "| lambda:", sev_selected_tuning$lambda, "\n")
}

# REPORT SECTION 4: ------------------------------

# Restore the format expected by Part C's calibration function
sev_test_claims <- data.table::as.data.table(sev_test_claims)

# Retry
s4_deciles <- sev_calibrate(sev_test_claims, "Decile")
s4_deciles <- s4_deciles[order(Decile)]

print(as.data.frame(s4_deciles), row.names = FALSE, digits = 7)

s4_required <- c("sev_test_model", "sev_test_scores", "sev_test_claims",
                 "sev_selected_name", "sev_comparison", "sev_calibrate",
                 "sev_res_plot", "sev_cal_plot", "sev_dev_claims")
s4_missing <- s4_required[!vapply(s4_required, exists, logical(1), inherits = TRUE)]
if (length(s4_missing)) stop("Run Part C first. Missing: ", paste(s4_missing, collapse = ", "))

cat("\n--- 4A. 2026 TEST RESULTS AGAINST YEAR-ONLY BENCHMARK ---\n")
s4_scores <- as.data.frame(sev_test_scores)
print(s4_scores, row.names = FALSE, digits = 7)
s4_chosen <- s4_scores[s4_scores$Model == sev_selected_name, , drop = FALSE]
s4_benchmark <- s4_scores[s4_scores$Model == "YearOnlyBenchmark", , drop = FALSE]
cat("Development years:", paste(sort(unique(sev_dev_claims$Year)), collapse = ", "), "\n")
cat("Test years:", paste(sort(unique(sev_test_claims$Year)), collapse = ", "), "\n")
cat("Test claiming policy-years:", nrow(sev_test_claims), "\n")
cat("Test claims:", sum(sev_test_claims$ClaimNb), "\n")
cat("Deviance improvement vs benchmark (%):",
    100 * (1 - s4_chosen$WeightedGammaDeviance / s4_benchmark$WeightedGammaDeviance), "\n")
cat("MAE improvement vs benchmark (%):",
    100 * (1 - s4_chosen$WeightedMAE / s4_benchmark$WeightedMAE), "\n")

cat("\n--- 4B. VALIDATION AND FUTURE-YEAR PERFORMANCE ---\n")
s4_validation <- as.data.frame(sev_comparison)
s4_validation <- s4_validation[s4_validation$Model == sev_selected_name, , drop = FALSE]
s4_metrics <- c("WeightedGammaDeviance", "WeightedMAE", "ActualSeverity",
                "PredictedSeverity", "AE_Ratio")
s4_comparison <- rbind(
  data.frame(Sample = "2023-2025 validation", s4_validation[, s4_metrics, drop = FALSE]),
  data.frame(Sample = "2026 test", s4_chosen[, s4_metrics, drop = FALSE])
)
print(s4_comparison, row.names = FALSE, digits = 7)
cat("Same selected specification; validation and test use different fitted coefficients.\n")

cat("\n--- 4C. 2026 CALIBRATION BY PREDICTED-SEVERITY DECILE ---\n")
s4_deciles <- sev_calibrate(sev_test_claims, "Decile")
s4_deciles <- s4_deciles[order(Decile)]
print(as.data.frame(s4_deciles), row.names = FALSE, digits = 7)
cat("Deciles run from lower to higher predicted severity; ties can affect sizes.\n")
cat("Actual total cost:", sum(s4_deciles$ActualCost), "\n")
cat("Expected conditional cost:", sum(s4_deciles$ExpectedConditionalCost), "\n")

cat("\n--- 4D. 2026 CALIBRATION BY VEHICLE TYPE AND RISK ZONE ---\n")
for (g in c("VehicleType", "RiskZone")) {
  cat("\n", g, "\n", sep = "")
  print(as.data.frame(sev_calibrate(sev_test_claims, g)), row.names = FALSE, digits = 7)
}
cat("A/E uses observed claim counts: this checks severity conditional on claiming.\n")
cat("Future technical prices require predicted frequency multiplied by predicted severity.\n")

cat("\n--- 4E. TEST DIAGNOSTIC PLOTS ---\n")
print(sev_cal_plot)
print(sev_res_plot)
# Named files let you retrieve both plots even if RStudio shows only the last.
s4_plot_dir <- "severity_report_section4_outputs"
dir.create(s4_plot_dir, showWarnings = FALSE)
ggplot2::ggsave(file.path(s4_plot_dir, "2026_decile_calibration.png"),
                plot = sev_cal_plot, width = 8, height = 5, dpi = 160)
ggplot2::ggsave(file.path(s4_plot_dir, "2026_deviance_residuals.png"),
                plot = sev_res_plot, width = 8, height = 5, dpi = 160)
cat("Both plots saved in:", normalizePath(s4_plot_dir), "\n")
cat("Gamma deviance residuals need not be normally distributed.\n")

# REPORT SECTION 5: ------------------------------

s5_required <- c("sev_final_model", "sev_final_claims", "sev_coef",
                 "sev_selected_name", "sev_year_actual", "sev_trend_table",
                 "sev_projection", "severity_2027", "severity_2028")
s5_missing <- s5_required[!vapply(s5_required, exists, logical(1), inherits = TRUE)]
if (length(s5_missing)) stop("Run Part C first. Missing: ", paste(s5_missing, collapse = ", "))

cat("\n--- 5A. FINAL MODEL AND RATING RELATIVITIES ---\n")
cat("Selected specification:", sev_selected_name, "\n")
cat("Final fitting years:", paste(sort(unique(sev_final_claims$Year)), collapse = ", "), "\n")
cat("Final fitting claim-years:", nrow(sev_final_claims), "\n")
cat("Claims represented:", sum(sev_final_claims$ClaimNb), "\n")
s5_b <- sev_coef(sev_final_model)
s5_coefficients <- data.frame(
  Term = names(s5_b), LogCoefficient = as.numeric(s5_b),
  Multiplier = exp(as.numeric(s5_b)),
  ChangePct = 100 * expm1(as.numeric(s5_b))
)
print(s5_coefficients, row.names = FALSE, digits = 7)
cat("For category coefficients, multipliers are relative to the reference level, holding other inputs fixed.\n")
cat("exp(intercept) is the reference-profile severity at YearIndex = 0;\n")
cat("its ChangePct entry is not a meaningful percentage comparison.\n")
if (sev_final_model$kind == "glm") {
  print(formula(sev_final_model$fit))
  s5_levels <- sev_final_model$fit$xlevels
  if (length(s5_levels)) {
    cat("\nReference categories (Part C uses treatment coding):\n")
    print(data.frame(Variable = names(s5_levels),
                     Reference = vapply(s5_levels, function(x) x[1], character(1))),
          row.names = FALSE)
  }
  cat("Final fit converged:", sev_final_model$fit$converged, "\n")
}

cat("\n--- 5B. OBSERVED AND MIX-ADJUSTED YEAR TRENDS ---\n")
print(as.data.frame(sev_year_actual), row.names = FALSE, digits = 7)
print(as.data.frame(sev_trend_table), row.names = FALSE, digits = 7)
cat("Trend confidence intervals cluster by PolicyID.\n")
cat("Raw and MixAdjusted are diagnostic models; MixAdjusted uses the full rating formula.\n")
cat("The deployed trend below comes from the selected final model and can differ.\n")

cat("\n--- 5C. APPLIED 2027 / 2028 TREND ASSUMPTION ---\n")
s5_year_b <- unname(s5_b["YearIndex"])
stopifnot(length(s5_year_b) == 1L, is.finite(s5_year_b))
s5_projection <- data.frame(SelectedModel = sev_selected_name,
                            YearCoefficient = s5_year_b,
                            AnnualSeverityChangePct = 100 * expm1(s5_year_b),
                            Factor2027Vs2026 = exp(s5_year_b),
                            Factor2028Vs2026 = exp(2 * s5_year_b))
print(s5_projection, row.names = FALSE, digits = 7)
cat("Assumption: the fitted proportional annual trend continues through 2028.\n")
cat("YearIndex already includes the trend; no additional inflation multiplier is applied.\n")

cat("\n--- 5D. RENEWAL SEVERITY PREDICTIONS AND CHECKS ---\n")
s5_forecast_summary <- function(d, year) {
  p <- d$PredictedSeverity
  base <- d$Severity_2026Level
  valid <- is.finite(p) & p > 0 & is.finite(base) & base > 0
  ratio <- p[valid] / base[valid]
  expected <- exp((year - 2026) * s5_year_b)
  data.frame(Year = year, Policies = nrow(d),
             DuplicatePolicyIDs = sum(duplicated(d$PolicyID)),
             InvalidPredictionOrBase = sum(!valid),
             MeanAcrossPolicies = mean(p), MedianAcrossPolicies = median(p),
             P95AcrossPolicies = unname(quantile(p, 0.95)),
             MaxTrendRatioError = if (length(ratio)) max(abs(ratio - expected)) else NA_real_)
}
print(rbind(s5_forecast_summary(severity_2027, 2027),
            s5_forecast_summary(severity_2028, 2028)), row.names = FALSE, digits = 7)
cat("Policy means are not claim-frequency-weighted portfolio severity estimates.\n")
cat("2026-level predictions hold future-book risk characteristics fixed and remove only the year effect.\n")
cat("TechnicalPrice = PredictedAnnualFrequency * PredictedSeverity.\n")
cat("Commercial loadings, discounts and premium constraints are applied separately.\n")


## COMBINING SEVERITY AND FREQUENCY ------------------------------------


# Create the input datasets
jc_fd    <- as.data.frame(freq_test_data)
jc_sd    <- as.data.frame(sev_test_all)
jc_train <- as.data.frame(freq_dev_all)

# Create the output folder
jc_out <- "combined_model_outputs"
dir.create(jc_out, showWarnings = FALSE)

# Create the model summary
jc_models <- data.frame(
  Component = c("Frequency", "Severity"),
  SelectedModel = c(freq_selected_name, sev_selected_name),
  Family = c("Poisson, log link", "Gamma, log link"),
  Response = c("ClaimNb", "ClaimAmount / ClaimNb"),
  Weights = c("One per policy-year", "ClaimNb"),
  TestTrainingYears = rep("2023-2025", 2)
)

print(jc_models, row.names = FALSE)

for (nm in c("freq_test_model", "sev_test_model")) {
  m <- get(nm)
  cat("\n", nm, "\n", sep = "")
  if (m$kind == "glm") print(formula(m$fit)) else
    cat("Penalised fit: alpha =", m$alpha, "; lambda =", m$lambda, "\n")
}
cat("2026 policies tested, INCLUDING nonclaimants:", nrow(jc_fd), "\n")

# Regenerate holdout predictions explicitly from PRE-2026 fitted models.
jc_fpred <- freq_predict_selected(freq_test_model, jc_fd)
jc_spred <- sev_predict(sev_test_model, jc_sd)
jc_sm <- match(jc_fd$PolicyID, jc_sd$PolicyID)
stopifnot(isTRUE(all.equal(as.numeric(jc_fd$ClaimNb), as.numeric(jc_sd$ClaimNb[jc_sm]))),
          isTRUE(all.equal(as.numeric(jc_fd$ClaimAmount), as.numeric(jc_sd$ClaimAmount[jc_sm]))))
jc_test <- data.frame(PolicyID = jc_fd$PolicyID, Year = 2026,
                      ClaimNb = jc_fd$ClaimNb, ClaimAmount = jc_fd$ClaimAmount,
                      VehicleType = jc_fd$VehicleType, RiskZone = jc_fd$RiskZone,
                      PredictedAnnualFrequency = jc_fpred, PredictedSeverity = jc_spred[jc_sm])
jc_test$TechnicalPrice <- jc_test$PredictedAnnualFrequency * jc_test$PredictedSeverity
stopifnot(all(is.finite(jc_test$TechnicalPrice) & jc_test$TechnicalPrice > 0),
          all(is.finite(jc_test$ClaimAmount) & jc_test$ClaimAmount >= 0))

# Transparent benchmark: flat frequency and year-only severity, trained pre-2026.
jc_flat_frequency <- mean(jc_train$ClaimNb)
jc_bseverity <- sev_predict(sev_test_baseline, jc_sd)[jc_sm]
jc_test$BenchmarkCost <- jc_flat_frequency * jc_bseverity
jc_cost_score <- function(actual, predicted, label) {
  data.frame(Model = label, Policies = length(actual),
             ActualCost = sum(actual), PredictedCost = sum(predicted),
             ActualCostPerPolicy = mean(actual), PredictedCostPerPolicy = mean(predicted),
             CostAE = sum(actual) / sum(predicted),
             MAE = mean(abs(actual - predicted)), RMSE = sqrt(mean((actual - predicted)^2)))
}
jc_scores <- rbind(
  jc_cost_score(jc_test$ClaimAmount, jc_test$TechnicalPrice, "Selected frequency x severity"),
  jc_cost_score(jc_test$ClaimAmount, jc_test$BenchmarkCost, "Flat frequency x year-only severity"))
cat("\n--- C2. JOINT 2026 ANNUAL CLAIM-COST TEST ---\n")
print(jc_scores, row.names = FALSE, digits = 7)
cat("MAE improvement vs benchmark (%):", 100 * (1 - jc_scores$MAE[1]/jc_scores$MAE[2]), "\n")
cat("RMSE improvement vs benchmark (%):", 100 * (1 - jc_scores$RMSE[1]/jc_scores$RMSE[2]), "\n")
cat("Claim-count A/E:", sum(jc_test$ClaimNb)/sum(jc_test$PredictedAnnualFrequency), "\n")
cat("Cost A/E > 1 means aggregate claim cost is underpredicted.\n")
cat("MAE and RMSE assess all policy-years; zero claims and large losses affect these differently.\n")

# Rank-based deciles avoid errors from duplicate prediction quantiles.
jc_test$RiskDecile <- pmin(10L, ceiling(rank(jc_test$TechnicalPrice,
                                             ties.method = "average") / nrow(jc_test) * 10))
jc_calibrate <- function(d, group) {
  groups <- split(seq_len(nrow(d)), as.character(d[[group]]))
  z <- do.call(rbind, lapply(names(groups), function(g) {
    i <- groups[[g]]
    data.frame(Group = g, Policies = length(i), Claims = sum(d$ClaimNb[i]),
               ActualCost = sum(d$ClaimAmount[i]), PredictedCost = sum(d$TechnicalPrice[i]),
               ActualCostPerPolicy = mean(d$ClaimAmount[i]),
               PredictedCostPerPolicy = mean(d$TechnicalPrice[i]),
               CostAE = sum(d$ClaimAmount[i])/sum(d$TechnicalPrice[i]))
  }))
  names(z)[1] <- group
  rownames(z) <- NULL
  z
}
jc_deciles <- jc_calibrate(jc_test, "RiskDecile")
jc_deciles$RiskDecile <- as.integer(jc_deciles$RiskDecile)
jc_deciles <- jc_deciles[order(jc_deciles$RiskDecile), ]
cat("\n--- C3. JOINT CALIBRATION BY PREDICTED CLAIM-COST DECILE ---\n")
print(jc_deciles, row.names = FALSE, digits = 7)
cat("\nSegment checks (supporting appendix):\n")
jc_vehicle <- jc_calibrate(jc_test, "VehicleType")
jc_zone <- jc_calibrate(jc_test, "RiskZone")
print(jc_vehicle, row.names = FALSE, digits = 7)
print(jc_zone, row.names = FALSE, digits = 7)

# Paired bootstrap: same policy sample for selected model and benchmark.
# One row per policy in 2026, so resampling rows resamples whole test policies.
set.seed(4305)
jc_boot <- replicate(500, {
  i <- sample.int(nrow(jc_test), replace = TRUE)
  a <- jc_test$ClaimAmount[i]; p <- jc_test$TechnicalPrice[i]; b <- jc_test$BenchmarkCost[i]
  c(CostAE = sum(a)/sum(p), MAEImprovementDollars = mean(abs(a-b))-mean(abs(a-p)))
})
jc_intervals <- data.frame(Metric = rownames(jc_boot),
                           Estimate = c(jc_scores$CostAE[1], jc_scores$MAE[2]-jc_scores$MAE[1]),
                           Lower95 = apply(jc_boot, 1, quantile, probs = .025),
                           Upper95 = apply(jc_boot, 1, quantile, probs = .975), row.names = NULL)
cat("\n--- C4. CONDITIONAL TEST UNCERTAINTY (500 BOOTSTRAPS) ---\n")
print(jc_intervals, row.names = FALSE, digits = 7)
cat("Positive MAEImprovementDollars favours the selected model.\n")
cat("Intervals condition on fitted models; they exclude refitting and future trend uncertainty.\n")

# Final all-history component forecasts, joined safely by ID and year.
jc_join <- function(f, s, yr) {
  f <- as.data.frame(f); s <- as.data.frame(s)
  stopifnot(!anyNA(f$PolicyID), !anyNA(s$PolicyID),
            !anyDuplicated(f$PolicyID), !anyDuplicated(s$PolicyID),
            setequal(f$PolicyID, s$PolicyID), all(f$PricingYear == yr), all(s$PricingYear == yr))
  z <- merge(f[, c("PolicyID", "PricingYear", "PredictedAnnualFrequency")],
             s[, c("PolicyID", "PricingYear", "PredictedSeverity")],
             by = c("PolicyID", "PricingYear"), all = TRUE)
  stopifnot(nrow(z) == nrow(f), all(is.finite(z$PredictedAnnualFrequency) & z$PredictedAnnualFrequency > 0),
            all(is.finite(z$PredictedSeverity) & z$PredictedSeverity > 0))
  z$TechnicalPrice <- z$PredictedAnnualFrequency * z$PredictedSeverity
  z
}
jc_technical_2027 <- jc_join(frequency_2027, severity_2027, 2027)
jc_technical_2028 <- jc_join(frequency_2028, severity_2028, 2028)
jc_forecast_summary <- function(d) data.frame(Year = unique(d$PricingYear), Policies = nrow(d),
                                              ExpectedClaims = sum(d$PredictedAnnualFrequency), ExpectedClaimCost = sum(d$TechnicalPrice),
                                              MeanTechnicalPrice = mean(d$TechnicalPrice), MedianTechnicalPrice = median(d$TechnicalPrice),
                                              P95TechnicalPrice = unname(quantile(d$TechnicalPrice, .95)),
                                              FrequencyWeightedSeverity = sum(d$TechnicalPrice)/sum(d$PredictedAnnualFrequency))
jc_forecasts <- rbind(jc_forecast_summary(jc_technical_2027), jc_forecast_summary(jc_technical_2028))
cat("\n--- C5. COMBINED 2027 / 2028 TECHNICAL CLAIM COST ---\n")
print(jc_forecasts, row.names = FALSE, digits = 7)
cat("TechnicalPrice here is expected annual CLAIM COST before commercial adjustments.\n")

jc_plot <- ggplot2::ggplot(jc_deciles, ggplot2::aes(x = RiskDecile)) +
  ggplot2::geom_line(ggplot2::aes(y = ActualCostPerPolicy, colour = "Actual")) +
  ggplot2::geom_point(ggplot2::aes(y = ActualCostPerPolicy, colour = "Actual")) +
  ggplot2::geom_line(ggplot2::aes(y = PredictedCostPerPolicy, colour = "Predicted")) +
  ggplot2::geom_point(ggplot2::aes(y = PredictedCostPerPolicy, colour = "Predicted")) +
  ggplot2::theme_minimal() + ggplot2::labs(title = "2026 combined frequency x severity calibration",
                                           x = "Predicted annual claim-cost decile", y = "Annual claim cost per policy ($)", colour = NULL)
print(jc_plot)
