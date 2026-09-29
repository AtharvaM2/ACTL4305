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

# 3. Data cleaning ----------------------------------------------------------
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
  DensityBand = cut(Density, c(0, 10, 100, 500, 2000, Inf), labels = c("<10", "10-100", "100-500", "500-2000", "2000+")),
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
