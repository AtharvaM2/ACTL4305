# ACTL4305/5305 Datathon 2026 – Motor Pricing (Saltbush Direct)

R code and outputs for the technical pricing model, the 2027 pricing waterfall (Task 2) and the 2028 pricing waterfall (Task 3).

Final deliverables:

| File | Rows | Description |
|---|---|---|
| `priced_2027.csv` | 17,244 | Priced 2027 renewal book (one row per policy) |
| `priced_2028.csv` | 16,735 | Priced 2028 book: 15,214 renewals + 1,521 new business (`NEW…` IDs) |

Both files have the same 13 columns in the same order.

---

## 1. Contents

| Script | Purpose | Main outputs |
|---|---|---|
| `Data_Cleaning_and_EDA.R` | Task 1a: cleans the claims history and both renewal books, then runs EDA | Objects in memory: `data`, `renewal_book_2027`, `renewal_book_2028` (plus plots and tables) |
| `Frequency_Modelling.R` | Early frequency work: six Poisson candidates (base GLM, stepwise AIC/BIC, ridge, lasso, elastic net) and a pure-premium exploration (Tweedie GLM, frequency × severity benchmark, optional XGBoost) | Console tables and plots only |
| `ACTL4305_EDA_frequency_severity.R` | Single combined script: cleaning/EDA → frequency (6 models) → severity (6 Gamma models) → technical price → 2026 holdout calibration | `frequency_outputs/`, `severity_grouped_outputs/`, `technical_price_outputs/` (`technical_prices_2027/2028.csv`, `2026_joint_calibration.csv`, `2026_joint_test_score.csv`), `combined_model_outputs/`, `severity_report_section4_outputs/` |
| `Task_2_Pricing_Waterfall_Cap___Cup_First_Cleaned.R` | Task 2: prices the 2027 book through the full waterfall, runs the tests and loyalty-tax analysis, exports `priced_2027.csv` | `priced_2027.csv`, slide 2 and slide 3 charts (`.svg`/`.png`) |
| `Task_3_pricing.R` | Task 3: prices the 2028 book, runs promise and PaymentFrequency tests, builds diagnostics and graphs, exports `priced_2028.csv` | `priced_2028.csv`, `task3_outputs/` (7 CSVs) and `task3_outputs/graphs/` (10 PNGs) |

---

## 2. How to run

**Packages:** `data.table`, `ggplot2`, `scales`, `svglite` (Tasks 2 and 3); `glmnet`, `statmod`, `tweedie`, `sandwich` (modelling scripts); `xgboost` (optional challenger).

**Setup**

1. Put the three input files in one folder: `claims_history_2023_2026.csv`, `renewal_book_2027.csv`, `renewal_book_2028.csv`.
2. The scripts source the cleaning script by the name **`Data Cleaning and EDA.R`** (with spaces). If your file is named `Data_Cleaning_and_EDA.R`, rename it or edit the `source()` calls.
3. `Frequency_Modelling.R` and the Task 2 script contain a hard-coded `setwd("C:/Users/Ray/Documents/ACTL4305")`. Change it to your own folder.

**Run order for the pricing deliverables**

1. `Data Cleaning and EDA.R` (sourced automatically by Task 2)
2. Task 2 script. This produces `priced_2027.csv`.
3. Task 3 script, **in the same R session**. It needs `data`, `renewal_book_2028` and `book_2027` in memory. If `book_2027` is missing it falls back to reading `priced_2027.csv`.

Task 3 re-writes `priced_2027.csv` using the shared 13-column spec, so the 2027 file keeps `BaseTechnicalPrice`. At the end it checks that the `priced_2028.csv` header is identical to the `priced_2027.csv` header.

**Modelling and validation (separate path)**

Run `ACTL4305_EDA_frequency_severity.R` from the data folder. It is self-contained and takes several minutes because of the penalised-model cross-validation. Seeds used: 1, 9, 10 and 4305.

---

## 3. Data cleaning (Task 1a)

| Issue | Rows | Treatment |
|---|---|---|
| Duplicate (PolicyID, Year) rows | 371 | Removed, first kept (74,567 → 74,196 rows; 26,000 policies unchanged) |
| Inconsistent labels (VehicleType, GaragingLocation, VehicleUse) | ~2,200 each | Standardised (for example `suv`/`Suv` → `SUV`, `Sport` → `Sports`) |
| DriverAge missing or impossible (−1, 150) | 814 | 761 recovered from the same policy's other years; 53 filled with YearsLicensed + 20 |
| Negative VehAge | 223 | 205 recovered; 18 filled with the median (6) |
| VehValue, AnnualKms, GaragingLocation missing | 1,113 / 742 / 1,484 | Filled from other years of the same policy, else median by VehicleType or portfolio mode |
| Excess code 999 | 297 | Treated as missing: 272 recovered, 25 set to the mode ($500) |
| NCD above tenure, vehicles over 25 years, claims over $50k, top-coded Density | various | Kept (plausible values); logged |

After cleaning there are 10,601 claims and $75.9M incurred. All post-cleaning validation checks pass. The renewal books (17,244 and 16,735 policies) pass their checks.

---

## 4. Technical price models

**Pricing models used in Tasks 2 and 3** (fitted inside the Task 2 and Task 3 scripts on 2023–2026 data):

- **Frequency:** Poisson GLM, log link. Terms are `RiskZone + AnnualKms + GaragingLocation + VehicleUse + PaymentFrequency + factor(ExcessChosen) + factor(NCDLevel)`.
- **Severity:** Gamma GLM, log link, weighted by `ClaimNb`. Terms are `VehValueBand + VehicleType + RiskZone + factor(ExcessChosen) + YearIndex`, where `YearIndex = Year − 2026`. The year term carries severity inflation (about +5% a year) forward: 1 year to 2027 and 2 years to 2028.
- `TechnicalPrice = predicted frequency × predicted severity` (expected annual claims cost).
- `TenureYears` and `MultiPolicyFlag` are not rating factors. They are handled as commercial discounts.
- **Task 3 only:** `PaymentFrequency` is removed from the frequency model for 2028. Flipping a policy between Annual and Monthly then changes only the 3% instalment loading, which is checked in the code.

**Base price for the waterfall.** `BaseTechnicalPrice` is the technical price re-evaluated at `NCDLevel = 0` (and Annual payment in 2027). The NCD discount is then applied as a commercial discount, so NCD is not counted twice.

**EDA findings:** frequency 0.143 per policy-year, mean severity $7,161, loss cost $1,023. Mix-adjusted trends are about +2.0% a year for frequency and +5.2% a year for severity. Frequency is much higher in A_Metro (0.198) than in rural zones (about 0.09). `NCDLevel` and `TenureYears` are highly correlated (Cramér's V 0.54).

---

## 5. Pricing waterfall (Tasks 2 and 3)

Applied in this order for every policy:

| Step | Rule |
|---|---|
| 1. Base technical price | `BaseTechnicalPrice` (NCD 0) |
| 2. Loaded premium | `(BaseTechnicalPrice + $85) / (1 − 10% variable expense − 8% profit)`, that is `/ 0.82` |
| 3. Cap / cup | Applied to the loaded premium against the prior baseline (below) |
| 4. Discounts | NCD, then multi-policy, then loyalty, multiplicative (see below) |
| 5. Instalment loading | +3% for monthly payers, applied last |

**Cap / cup tiers**

| Raw rate change | Maximum increase (cap) |
|---|---|
| ≤ 5% | 5% |
| 5–15% | 10% |
| 15–22.5% | 18% |
| 22.5–30% | 25% |
| 30–60% | 35% |
| > 60% | 50% |

Minimum change (cup, the largest decrease allowed): −10% if the raw change is ≤ −20%, otherwise −5%.

**Discounts**

| Discount | Rate |
|---|---|
| NCD | 10% per level, 0–6 (up to 60%) |
| Multi-policy | 10% |
| Loyalty | 3% at tenure 3+ years, 6% at 6+, 10% at 10+ |

Final price = capped premium × (1 − NCD) × (1 − multi-policy) × (1 − loyalty) × (1 + instalment).

**Baselines**

- **2027:** the 2026 undiscounted gross base is reconstructed from the legacy rating formula (zone, vehicle, age and value factors, NCD 0, 35% loading, $350 floor). Driver age is the 2027 age minus 1. This is the `LegacyGrossBase_2026` column.
- **2028:** each renewal's baseline is its own 2027 `CapCupLoadedPremium`, which is pre-discount and pre-instalment. Carrying forward the 2027 final price would apply NCD, loyalty and multi-policy discounts a second time. New business has no prior premium, so it is priced at the full loaded premium with no cap/cup and no loyalty discount.

**Order of operations.** The cap/cup is applied before the discounts, so a floor can never swallow a promised discount (the ASIC REP 765 worked example is reproduced in the Task 2 script).

---

## 6. Output file columns

Columns in `priced_2027.csv` and `priced_2028.csv`:

| Column | Meaning |
|---|---|
| `PolicyID` | Policy identifier (`POL…`; 2028 also has `NEW…`) |
| `LegacyGrossBase_2026` | Reconstructed 2026 pre-discount legacy price (blank for 2028 new business) |
| `TechnicalPrice` | Expected annual claims cost, including the policy's NCD |
| `BaseTechnicalPrice` | Expected annual claims cost at NCD 0 |
| `LoadedPremium` | Base price plus expenses and profit loading |
| `RawRateChange` | `LoadedPremium` ÷ baseline − 1 (baseline: 2026 legacy base in 2027; 2027 `CapCupLoadedPremium` in 2028; blank for new business) |
| `EffectiveRateChange` | Rate change after cap/cup (blank for new business) |
| `CapCupLoadedPremium` | Pre-discount premium after cap/cup |
| `FinalPrice` | Annual price billed, after discounts and instalment loading |
| `NCDDiscountDollar` | NCD discount in dollars (applied first) |
| `MultiPolicyDiscountDollar` | Multi-policy discount in dollars |
| `LoyaltyDiscountDollar` | Loyalty discount in dollars |
| `TotalDiscountDollar` | Sum of the three discounts |

**Note:** instalment dollars are not a column. `FinalPrice = CapCupLoadedPremium − TotalDiscountDollar` for annual payers, and ×1.03 on top for monthly payers.

---

## 7. Headline results

| | 2027 | 2028 |
|---|---|---|
| Mean technical price | $1,156.50 | $1,205.38 |
| Mean loaded premium (NCD 0 base) | $1,948.81 | $2,076.13 |
| Mean final price | $1,086.55 | $1,231.14 |
| Total billed premium | $18.74M | $20.60M |
| Policies hitting the cap / cup | 47.5% / 29.7% | 26.6% / 20.3% (renewals) |
| Portfolio loss ratio | 106.4% | 97.9% (renewals 100.8%, new business 79.0%) |
| Combined ratio | 124.3% | 114.8% |

Other outputs:

- **Waterfall (2027, $M):** Gross loaded 33.61 → cap/cup −2.80 → NCD −10.95 → multi-policy −0.67 → loyalty −0.64 → instalment +0.19 → billed 18.74.
- **Loyalty:** longer-tenure customers are cheaper relative to risk, so there is no loyalty tax in the 2027 price-to-risk analysis. A regression controlling for rating factors shows price-to-risk falling by about 0.17 for 10+ year customers.
- **2028 compounding:** the same 2027-capped customers stay capped or cupped (no policy flips direction). Policies cupped in both years pay about 8.9% above an equivalent new customer on average.

---

## 8. Tests built into the scripts

- **Dollar conservation:** capped premium − discounts = discounted premium, and + instalment = final price, for every policy.
- **Promise tests:** all 56 combinations of NCD × multi-policy × loyalty deliver exactly the promised discount in 2027. In 2028, an end-to-end counterfactual (every policy set to each status and re-priced through the whole waterfall) delivers exactly the promised multiplier. This includes the combined cases and the new-business rule (no loyalty).
- **Boundary invariance:** delivered discounts equal promised discounts whether a policy is capped, cupped or unconstrained.
- **PaymentFrequency flip test (2028):** flipping payment frequency changes only the 3% instalment loading.
- **Export checks:** unique IDs, finite positive prices, and an identical header between `priced_2027.csv` and `priced_2028.csv`.

---

## 9. Model validation (modelling script)

The combined script holds out 2026 and fits only on 2023–2025.

- Candidate models (six frequency, six severity) are compared on a policy-grouped validation split and the validation winner is selected. Results are in `frequency_outputs/` and `severity_grouped_outputs/`.
- `2026_joint_test_score.csv`: cost A/E = **1.0025**.
- `2026_joint_calibration.csv`: decile A/E ranges from 0.91 to 1.15 across 10 risk deciles of about 1,950 policies each.

---