library(data.table)

# ============================================================================
# TASK 3: 2028 RENEWAL PRICING (ORDER AS IN PREVIOUS CODE)
# ============================================================================
# Order of operations:
#   1. Technical price (freq x sev), refitted WITHOUT NCDLevel and PaymentFrequency
#   2. Expense loading: (Technical + $85) / (1 - 10% - 8%)
#   3. Cap (+15%) and collar (-20%) applied to the LOADED premium vs 2027 final
#      New business: no guardrail in year one
#   4. NCD discount, then loyalty discount, then multi-policy discount
#   5. Instalment loading (3%, monthly payers)
#
# Note: in this order the discounts and instalment act AFTER the guardrails,
# so the final price can sit outside the -20% / +15% band. Diagnostics report this.
# ============================================================================

source("Data Cleaning and EDA.R")   # provides `data` (2023-2026, cleaned)

t3_out <- "task3_pricing_outputs"
dir.create(t3_out, showWarnings = FALSE)

# ---- A. Commercial parameters (from brief) ----------------------------------
FIXED_EXPENSE <- 85
LOADING_DENOM <- 1 - 0.10 - 0.08
NCD_DISCOUNT  <- c("0" = 0, "1" = 0.10, "2" = 0.20, "3" = 0.30,
                   "4" = 0.40, "5" = 0.50, "6" = 0.60)
LOYALTY_BANDS <- c("0-2yr", "3-5yr", "6-9yr", "10+yr")
LOYALTY_RATE  <- c(0, 0.03, 0.06, 0.10)
MULTI_RATE    <- 0.10
INSTAL_RATE   <- 0.03
CAP_PCT       <- 0.15
COLLAR_PCT    <- 0.20

# Rating factors for the technical model (NO NCDLevel, NO PaymentFrequency)
rating_vars <- c("DriverAgeBand", "VehAgeBand", "VehValueBand", "VehicleType",
                 "RiskZone", "DensityBand", "AnnualKms", "GaragingLocation",
                 "VehicleUse", "ExcessChosen")

# ---- B. Band and factor preparation -----------------------------------------
add_bands3 <- function(d) {
  d <- copy(as.data.table(d))
  d[, `:=`(
    DriverAgeBand = cut(DriverAge, c(-Inf, 24, 34, 44, 54, 64, 74, Inf),
                        labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")),
    VehAgeBand    = cut(VehAge, c(-Inf, 2, 5, 9, 14, 19, Inf),
                        labels = c("0-2", "3-5", "6-9", "10-14", "15-19", "20+")),
    VehValueBand  = cut(VehValue, c(-Inf, 10000, 15000, 20000, 30000, 40000, 60000, Inf),
                        labels = c("<10k", "10-15k", "15-20k", "20-30k", "30-40k", "40-60k", "60k+")),
    DensityBand   = cut(Density, c(-Inf, 10, 100, 500, 2000, Inf),
                        labels = c("<10", "10-100", "100-500", "500-2000", "2000+"))
  )]
  d
}

data <- add_bands3(data)
data[, YearIndex := Year - 2026]
for (v in rating_vars) if (!is.factor(data[[v]])) data[, (v) := factor(get(v))]

prep_book <- function(b, yr) {
  b <- add_bands3(b)
  b[, YearIndex := yr - 2026]
  stopifnot(!anyDuplicated(b$PolicyID))
  for (v in rating_vars) {
    lv <- levels(data[[v]])
    b[, (v) := factor(as.character(get(v)), levels = lv)]
    if (anyNA(b[[v]])) stop("Unmapped level or missing value in ", v, " (book ", yr, ")")
  }
  b
}

# ---- C. Refit technical models WITHOUT NCD and PaymentFrequency --------------
freq_f   <- as.formula(paste("ClaimNb ~", paste(rating_vars, collapse = " + ")))
fit_freq <- glm(freq_f, family = poisson(link = "log"), data = data)
stopifnot(fit_freq$converged)

sev_train <- copy(data[ClaimNb > 0])
sev_train[, AverageClaim := ClaimAmount / ClaimNb]
sev_f   <- as.formula(paste("AverageClaim ~", paste(c(rating_vars, "YearIndex"), collapse = " + ")))
fit_sev <- glm(sev_f, family = Gamma(link = "log"), weights = ClaimNb, data = sev_train)
stopifnot(fit_sev$converged)

# ---- D. Load books and prior-year final premium -------------------------------
renewal_book_2027 <- fread("renewal_book_2027.csv")
renewal_book_2028 <- fread("renewal_book_2028.csv")
pricing_2027      <- fread(file.path("task2_pricing_outputs", "pricing_2027_renewal_book.csv"))

prior <- pricing_2027[, .(PolicyID, Final_2027 = Final,
                          Collared_2027 = CollaredFlag, Capped_2027 = CappedFlag)]
stopifnot(!anyDuplicated(prior$PolicyID))

book28 <- prep_book(renewal_book_2028, 2028)
book28[, IsNB := as.character(NewBusinessFlag) %in% c("1", "TRUE", "Yes")]
book28[, MultiFlag := as.integer(as.character(MultiPolicyFlag) == "1")]
book28[, TechPrice := predict(fit_freq, newdata = book28, type = "response") *
         predict(fit_sev,  newdata = book28, type = "response")]
book28 <- merge(book28, prior, by = "PolicyID", all.x = TRUE)

stopifnot(all(!is.na(book28$Final_2027[!book28$IsNB])))
stopifnot(all(is.na(book28$Final_2027[book28$IsNB])))
cat("Renewals:", sum(!book28$IsNB), " New business:", sum(book28$IsNB), "\n")

# ---- E. Waterfall in the previous-code order ----------------------------------
price_waterfall_3 <- function(d) {
  w <- copy(d)
  
  # Step 2: expense loading
  w[, Loaded := (TechPrice + FIXED_EXPENSE) / LOADING_DENOM]
  
  # Step 3: cap and collar on the loaded premium vs 2027 final premium
  w[, Ceiling := fifelse(IsNB, Inf, Final_2027 * (1 + CAP_PCT))]
  w[, Floor   := fifelse(IsNB, 0,   Final_2027 * (1 - COLLAR_PCT))]
  w[, CappedFlag   := as.integer(Loaded > Ceiling)]
  w[, CollaredFlag := as.integer(Loaded < Floor)]
  w[, GuardLoaded  := pmin(pmax(Loaded, Floor), Ceiling)]
  
  # Step 4: discounts applied to the guarded loaded premium
  w[, NCD_Rate := NCD_DISCOUNT[as.character(NCDLevel)]]
  w[, AfterNCD := GuardLoaded * (1 - NCD_Rate)]
  
  w[, TenureBand   := as.character(cut(TenureYears, c(-Inf, 2, 5, 9, Inf),
                                       labels = LOYALTY_BANDS))]
  w[, Loyalty_Rate := LOYALTY_RATE[match(TenureBand, LOYALTY_BANDS)]]
  w[, AfterLoyalty := AfterNCD * (1 - Loyalty_Rate)]
  
  w[, Multi_Rate := MULTI_RATE * MultiFlag]
  w[, AfterMulti := AfterLoyalty * (1 - Multi_Rate)]
  
  # Step 5: instalment loading
  w[, Instal_Rate := INSTAL_RATE * (as.character(PaymentFrequency) == "Monthly")]
  w[, Final := AfterMulti * (1 + Instal_Rate)]
  
  # Diagnostic: is the final price inside the promised band?
  w[, InBand := IsNB | (Final >= Final_2027 * (1 - COLLAR_PCT) - 1e-9 &
                          Final <= Final_2027 * (1 + CAP_PCT) + 1e-9)]
  
  w[, Position := fcase(IsNB,               "New business (no guardrail)",
                        CollaredFlag == 1L, "Collared on loaded premium",
                        CappedFlag == 1L,   "Capped on loaded premium",
                        default =           "Free (no guardrail)")]
  w
}

# ============================================================================
# PART 1: PROMISE TESTS (hand-calculated, independent of the function)
# ============================================================================
cat("\n=== PROMISE TESTS ===\n")

hand_price_3 <- function(tech, ncd, ten, multi, freq, prior, nb) {
  loaded <- (tech + FIXED_EXPENSE) / LOADING_DENOM
  lo <- if (nb) 0   else prior * (1 - COLLAR_PCT)
  hi <- if (nb) Inf else prior * (1 + CAP_PCT)
  guarded <- min(max(loaded, lo), hi)
  band <- if (ten <= 2) 1 else if (ten <= 5) 2 else if (ten <= 9) 3 else 4
  final <- guarded * (1 - NCD_DISCOUNT[[as.character(ncd)]]) *
    (1 - LOYALTY_RATE[band]) * (1 - MULTI_RATE * multi) *
    (1 + INSTAL_RATE * (freq == "Monthly"))
  list(loaded = loaded, guarded = guarded, final = final)
}

test_cases <- data.table(
  TestID           = c("NewBiz_NoGuardrail", "Renewal_Free", "Renewal_Cap",
                       "Renewal_Collar", "Monthly_Instalment"),
  TechPrice        = c(1200, 1200, 2000, 500, 1200),
  NCDLevel         = c(0, 1, 0, 6, 1),
  TenureYears      = c(0, 1, 1, 15, 1),
  MultiPolicyFlag  = c(1, 0, 0, 1, 0),
  PaymentFrequency = c("Annual", "Annual", "Annual", "Annual", "Monthly"),
  Final_2027       = c(NA, 1500, 1000, 1000, 1500),
  IsNB             = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  MultiFlag        = c(1L, 0L, 0L, 1L, 0L)
)

test_out <- price_waterfall_3(test_cases)
exp_rows <- rbindlist(lapply(seq_len(nrow(test_cases)), function(i) {
  r <- test_cases[i]
  h <- hand_price_3(r$TechPrice, r$NCDLevel, r$TenureYears, r$MultiPolicyFlag,
                    r$PaymentFrequency, r$Final_2027, r$IsNB)
  data.table(Exp_Loaded = h$loaded, Exp_Guarded = h$guarded, Exp_Final = h$final)
}))
test_out <- cbind(test_out, exp_rows)
test_out[, Pass := abs(Loaded - Exp_Loaded) < 1e-6 &
           abs(GuardLoaded - Exp_Guarded) < 1e-6 &
           abs(Final - Exp_Final) < 1e-6]

test_report <- test_out[, .(TestID, PaymentFrequency, NCDLevel, TenureYears, MultiPolicyFlag,
                            Final_2027, Loaded = round(Loaded, 2),
                            GuardLoaded = round(GuardLoaded, 2),
                            Final = round(Final, 2), InBand, Position, Pass)]
fwrite(test_report, file.path(t3_out, "01_promise_tests.csv"))
print(test_report, row.names = FALSE)
stopifnot(all(test_out$Pass))
cat("All", nrow(test_out), "promise tests passed.\n")

# ============================================================================
# PART 2: 2028 PORTFOLIO PRICING
# ============================================================================
cat("\n=== 2028 PORTFOLIO PRICING ===\n")

wf28 <- price_waterfall_3(book28)

# ---- Bridge: technical price to final premium -------------------------------
bridge28 <- wf28[, .(
  Policies     = .N,
  Technical    = sum(TechPrice),
  Expense      = sum(Loaded - TechPrice),
  GuardLoaded  = sum(GuardLoaded - Loaded),
  NCD          = -sum(GuardLoaded * NCD_Rate),
  Loyalty      = -sum(AfterNCD * Loyalty_Rate),
  MultiPolicy  = -sum(AfterLoyalty * Multi_Rate),
  Instalment   = sum(Final - AfterMulti),
  Final        = sum(Final)
)]

stopifnot(abs(bridge28$Technical + bridge28$Expense + bridge28$GuardLoaded +
                bridge28$NCD + bridge28$Loyalty + bridge28$MultiPolicy +
                bridge28$Instalment - bridge28$Final) < 1e-4)
stopifnot(bridge28$Policies == nrow(renewal_book_2028))

bridge28_long <- data.table(
  Step   = c("Technical price", "Expense loading", "Cap/collar on loaded premium",
             "NCD discount", "Loyalty discount", "Multi-policy discount",
             "Instalment loading", "Final 2028 premium"),
  Amount = c(bridge28$Technical, bridge28$Expense, bridge28$GuardLoaded,
             bridge28$NCD, bridge28$Loyalty, bridge28$MultiPolicy,
             bridge28$Instalment, bridge28$Final)
)
fwrite(bridge28_long, file.path(t3_out, "02_waterfall_bridge_2028.csv"))
print(bridge28_long, row.names = FALSE)

# ---- Guardrail and band diagnostics -----------------------------------------
guard28 <- wf28[, .(Policies = .N, Share = round(.N / nrow(wf28), 4),
                    TotalFinal = sum(Final)), by = Position]
fwrite(guard28, file.path(t3_out, "03_guardrail_summary_2028.csv"))
print(guard28, row.names = FALSE)

band28 <- wf28[IsNB == FALSE, .(Renewals = .N,
                                OutsideBand = sum(!InBand),
                                ShareOutside = round(mean(!InBand), 4))]
fwrite(band28, file.path(t3_out, "04_final_outside_band_2028.csv"))
cat("Renewals with final price outside the 2027 band:", band28$OutsideBand,
    "of", band28$Renewals, "\n")

# ============================================================================
# PART 3: 2027 -> 2028 MOVEMENT AND COMPOUNDING
# ============================================================================
cat("\n=== 2027 -> 2028 MOVEMENT ===\n")

renewals <- wf28[IsNB == FALSE]
renewals[, `:=`(
  Chg_2028_vs_2027 = Final / Final_2027 - 1,
  CollaredBoth     = as.integer(Collared_2027 == 1 & CollaredFlag == 1),
  CappedBoth       = as.integer(Capped_2027 == 1 & CappedFlag == 1)
)]

compound_summary <- data.table(
  Renewals              = nrow(renewals),
  CollaredIn2027        = sum(renewals$Collared_2027),
  CollaredIn2028        = sum(renewals$CollaredFlag),
  CollaredInBothYears   = sum(renewals$CollaredBoth),
  CappedIn2027          = sum(renewals$Capped_2027),
  CappedIn2028          = sum(renewals$CappedFlag),
  CappedInBothYears     = sum(renewals$CappedBoth),
  Mean_Chg_2028_vs_2027 = round(100 * mean(renewals$Chg_2028_vs_2027), 2),
  Min_Chg_2028_vs_2027  = round(100 * min(renewals$Chg_2028_vs_2027), 2),
  Max_Chg_2028_vs_2027  = round(100 * max(renewals$Chg_2028_vs_2027), 2)
)
fwrite(compound_summary, file.path(t3_out, "05_compounding_summary.csv"))
print(compound_summary, row.names = FALSE)

# ============================================================================
# PART 4: EXPORT
# ============================================================================
export28 <- wf28[, .(PolicyID, IsNB, Final_2027, TechPrice, Loaded, GuardLoaded,
                     Floor, Ceiling, CappedFlag, CollaredFlag, Position,
                     NCDLevel, NCD_Rate, TenureYears, Loyalty_Rate,
                     MultiFlag, Multi_Rate, PaymentFrequency, Instal_Rate,
                     Final, InBand)]
money <- c("TechPrice", "Loaded", "GuardLoaded", "Floor", "Ceiling", "Final")
export28[, (money) := lapply(.SD, round, 2), .SDcols = money]
fwrite(export28, file.path(t3_out, "pricing_2028_renewal_book.csv"))

cat("\n2028 final premium: $", format(round(bridge28$Final), big.mark = ","), "\n")
cat("Files written to", t3_out, "\n")