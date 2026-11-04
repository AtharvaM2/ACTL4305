# ACTL4305/5305 Datathon 2026 – Motor Pricing (Saltbush Direct)

R code and outputs for the technical pricing model (Task 1), the 2027 pricing waterfall (Task 2) and the 2028 pricing waterfall (Task 3).

Final deliverables:

| File | Rows | Columns | Description |
|---|---|---|---|
| `priced_2027.csv` | 17,244 | 13 | Priced 2027 renewal book (one row per policy) |
| `priced_2028.csv` | 16,735 | 13 | Priced 2028 book: 15,214 renewals + 1,521 new business (`NEW…` IDs) |

Both files share the exact same 13 columns in the same order. All exported files and output folders are saved directly into the folder where the scripts are run.

---

## 1. How to Run

### Package Installation
Run this once in the R console to install all required libraries:

```r
install.packages(c(
  "data.table", "ggplot2", "scales", "svglite", 
  "glmnet", "statmod", "tweedie", "sandwich", "xgboost"
))
```

### Setup
1. Keep all R scripts and the three input data files in the same folder:
   - `claims_history_2023_2026.csv`
   - `renewal_book_2027.csv`
   - `renewal_book_2028.csv`

### Run Order
To produce the final pricing deliverables, run these scripts **sequentially in the same active R session**:

1. **`Task 1 Data Cleaning and EDA Final.R`**  
   Cleans the claims history and both renewal books. Leaves `data`, `renewal_book_2027`, and `renewal_book_2028` in memory.
2. **`Task 2 Pricing Final.R`**  
   Fits baseline GLMs, executes the 2027 pricing waterfall, audits discount promises, exports `priced_2027.csv`, and creates presentation charts. Leaves `book_2027` in memory.
3. **`Task 3 Pricing Final.R`**  
   Prices the 2028 book, runs promise tests and PaymentFrequency tests, builds diagnostics, verifies the 13-column match, and exports `priced_2028.csv`.

*Optional Task 1 Modelling Scripts:*
- **`Task 1 Severity Modelling Final.R`**: Completely self-contained script. Fits all 6 frequency and 6 severity models with regularised CV, evaluates out-of-time holdouts, and exports all Task 1 report metrics.
- **`Task 1 Frequency Modelling Final.R`**: Run after Step 1 if evaluating alternative Tweedie or XGBoost models.

---

## 2. Contents

| Script | Purpose | Main outputs |
|---|---|---|
| `Task 1 Data Cleaning and EDA Final.R` | Task 1a: cleans claims history and renewal books; handles missingness and creates rating factor bands | Objects in memory (`data`, `renewal_book_2027`, `renewal_book_2028`) and EDA summary plots |
| `Task 1 Frequency Modelling Final.R` | Task 1b: early frequency candidate testing, Tweedie pure premium models, and optional XGBoost challenger | Console diagnostic tables and lift plots |
| `Task 1 Severity Modelling Final.R` | Task 1b/c: self-contained pipeline fitting 6 frequency and 6 Gamma severity models, 2026 holdout testing, and report metrics | Output folders (`frequency_outputs/`, `severity_grouped_outputs/`, `technical_price_outputs/`, `combined_model_outputs/`) |
| `Task 2 Pricing Final.R` | Task 2: prices 2027 book through full waterfall, tests promise delivery, evaluates loyalty tax, exports `priced_2027.csv` | `priced_2027.csv`, slide 2 and slide 3 charts (`.png`/`.svg`) |
| `Task 3 Pricing Final.R` | Task 3: prices 2028 book, tests PaymentFrequency removal, audits compounding and loyalty tax, exports `priced_2028.csv` | `priced_2028.csv`, `task3_outputs/` (CSV tables and graph PNGs) |
