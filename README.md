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

1. Put the three input files in one folder: `claims_history_2023_2026.csv`, `renewal_book_2027.csv`, `renewal_book_2028.csv` along with all the R scripts.
2. Note: `Frequency_Modelling.R` and `Task_2_Pricing_Waterfall_Cap___Cup_First_Cleaned.R` scripts require `Data Cleaning and EDA.R` to be ran first.
3. Note: Run `ACTL4305_EDA_frequency_severity.R` from the data folder. It is self-contained and takes several minutes because of the penalised-model cross-validation. Seeds used: 1, 9, 10 and 4305.

**Run Order**

1. Run `Data Cleaning and EDA.R` for Task 1(sourced automatically by Task 2)
2. Run `Frequency_Modelling.R` and `ACTL4305_EDA_frequency_severity.R` for Task 1
3. Run `Task_2_Pricing_Waterfall_Cap___Cup_First_Cleaned.R` for Task 2. This produces `priced_2027.csv`.
4. Run `Task_3_pricing.R` **in the same R session** for Task 3. It needs `data`, `renewal_book_2028` and `book_2027` in memory. If `book_2027` is missing it falls back to reading `priced_2027.csv`.

Note: Task 3 re-writes `priced_2027.csv` using the shared 13-column spec, so the 2027 file keeps `BaseTechnicalPrice`. At the end it checks that the `priced_2028.csv` header is identical to the `priced_2027.csv` header.



---
