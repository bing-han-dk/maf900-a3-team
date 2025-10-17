# 📘Replicating Tables 1–3 of Fama and MacBeth (1973)

This project replicates the key tables in a cross-sectional asset pricing study using Fama-MacBeth regressions. It includes:

- 📄 **Table 1**: Sample periods & eligible stock counts
- 📄 **Table 2**: Portfolio-level beta statistics
- 📄 **Table 3**: Fama-MacBeth regression results (4 model variants)

---

## 🧱 Repository Structure

```text
.
├── 00_utils/
│   ├── connect_to_database.R        # WRDS connection script
│   └── global_functions.R           # Core helper/statistical functions
├── 01_extract/
│   └── data_extract_clean.R         # Pulls and cleans data from WRDS
├── 02_build/
│   ├── table_1_sec_count.R          # Generates Table 1 stock counts
│   ├── table_2_build_up.R           # Generates Table 2 beta stats
│   └── table_3_build_up.R           # Portfolio formation & FM regression logic
├── 03_viz/
│   ├── table_1_viz.R                # Visualizations for Table 1 results
│   ├── table_2_viz.R                # Visualizations for Table 2 results
│   ├── table_3_viz.R                # Visualizations for Table 3 results
├── 04_outputs/                      # Replication report and exported tables and figures
├── README.md                        # This file
```

---

## ⚙️ Setup Instructions

### 1. Install Required R Packages

```r
install.packages(c(
  "dplyr", "tidyverse", "lubridate", "broom", "RPostgres", "DBI",
  "tidyr", "purrr", "knitr", "kableExtra", "gt", "stringr", "ggplot2"
))
```

### 2. WRDS Database Login

Update `utils/connect_to_database.R` with your WRDS credentials:

```r
wrds <- DBI::dbConnect(
  RPostgres::Postgres(),
  host   = "wrds-pgdata.wharton.upenn.edu",
  port   = 9737,
  dbname = "wrds",
  sslmode = "require",
  user   = "your_wrds_username"
)
```

---

## 🔁 Reproducibility Pipeline

### 🔹 Step 1: Extract Data

```r
source("01_extract/data_extract_clean.R")
```

Pulls and merges:

* CRSP monthly stock returns
* Delisting returns
* Fama-French factors (monthly)
* Outputs:

  * `data_ret`, `data_delist`, `data_factor`

---

### 🔹 Step 2: Portfolio Construction & Estimation

Key logic:

* Constructs 9 rolling periods (P1–P9)
* Applies eligibility filters:

  * Appears in test-start month
  * ≥48 months of returns in formation
  * Full 60-month coverage in estimation
* Assigns 20 portfolios by formation-period beta
* Estimates security & portfolio betas
* Runs Fama-MacBeth regressions per month

---

## 📊 Tables Generated

### 📄 Table 1: Sample Construction

**Script:** `02_build/table_1_sec_count.R`

* Uses `count_block()` on each of 9 periods
* Counts:

  * Available stocks (exist in test-start month)
  * Eligible stocks (pass 48/60M return filter)

| Period | Formation | Estimation | Testing   | Available | Eligible |
| ------ | --------- | ---------- | --------- | --------- | -------- |
| P1     | 1926–1929 | 1930–1934  | 1935–1938 | X         | Y        |
| ...    | ...       | ...        | ...       | ...       | ...      |

---

### 📄 Table 2: Portfolio Beta Statistics

**Script:** `02_build/table_2_build_up.R`

For 4 selected estimation periods (1934–1938, ..., 1958–1962):

* 20 beta-ranked portfolios
* Per-portfolio stats:

  * `β_p` (portfolio beta)
  * `R²` from CAPM regression
  * `σ_Rp` (portfolio return SD)
  * `σ_resid` (residual SD)
  * `σ̄_εᵢ` (average stock-level residual SD)
  * `ratio = σ_resid / σ̄_εᵢ`

Output: `stat_t2_list[[i]]` for each period.

---

### 📄 Table 3: Fama-MacBeth Regressions

**Script:** part of `02_build/table_3_build_up.R`

Runs cross-sectional regressions each month over test windows:

| Model | Specification                    |
| ----- | -------------------------------- |
| A     | `ret ~ beta`                     |
| B     | `ret ~ beta + beta^2`            |
| C     | `ret ~ beta + sd_resid`          |
| D     | `ret ~ beta + beta^2 + sd_resid` |

Summary includes:

* `mean_gamma`, `t_stat`, `acf1`, `r2`, etc.
* Output stored in: `fmb_results_df`

---
Based on the code snippets you provided and the image of your project directory (`03_viz/`), here's an updated, well-organized markdown documentation section for the visualizations in your project. This version is:

* **Consistent**
* **Concise**
* **Descriptive**
* **Formatted for rendering (e.g., GitHub/Quarto)**

---

## 📊 Visualizations (`03_viz/`)

This folder contains scripts for rendering the tables and figures used in the empirical results section.

### 📋 `table_1_viz.R`

**Description**:
Generates **Table 1** and **Table 1 (Continued)** summarizing portfolio formation, estimation, and testing periods.

**Usage**:

```r
source("03_viz/table_1_viz.R")
render_gt(panel_1_5)       # Main table
render_gt_ctnd(panel_6_9)  # Continued table
```

---

### 📋 `table_2_viz.R`

**Description**:
Constructs **Table 2**, reporting sample statistics for selected estimation periods (e.g., betas, R², residual variances). Outputs are split across two pages for portfolios 1–10 and 11–20.

**Usage**:

```r
source("03_viz/table_2_viz.R")
tbl2_page1  # Portfolios 1–10
tbl2_page2  # Portfolios 11–20
```

---

### 📋 `table_3_viz.R`

**Description**:
Generates **Table 3**, summarizing Fama–MacBeth regressions across various models and subperiods. Includes statistics like average γs, t-stats, autocorrelations, and R².

**Usage**:

```r
source("03_viz/table_3_viz.R")
tbl3 <- make_table3(fmb_results_df)
```

Optional export:

```r
make_table3(fmb_results_df, save_path = "results/table3.html")
```

---

## 📂 Output Variable Summary

| Variable         | Description                                 |
| ---------------- | ------------------------------------------- |
| `data_ret`       | Cleaned monthly stock returns               |
| `data_delist`    | Delisting return info                       |
| `data_factor`    | Monthly market & risk-free rates            |
| `result_all`     | Testing panel: portfolio assignments + beta |
| `stat_t2_list`   | Portfolio-level stats (for Table 2)         |
| `fmb_results_df` | Fama-MacBeth coefficients & stats (Table 3) |

---

## 🧠 Key Functions

Defined in `utils/global_functions.R`:


| Function                 | Description                                 |
|--------------------------|---------------------------------------------|
| `estimate_beta()`        | Estimates beta, standard error, R², and residual SD via OLS |
| `assign_portfolios()`    | Assigns securities into portfolios based on sorted betas |
| `assign_20_portfolios()` | Ranks and buckets securities into 20 portfolios |
| `ols_one_x(x, y)`        | Efficient helper for simple (1-variable) OLS regression |
| `run_fmb()`              | Runs cross-sectional Fama–MacBeth regressions by month |
| `fmb_coef_stats()`       | Computes time-series summary stats for gamma estimates |
| `count_block()`          | Counts eligible stocks per period for Table 1 |
| `make_panel_df()`        | Prepares compact summary table for Table 1 |
| `mon_seq()`              | Helper to generate first-of-month sequences |

---

## 🧮 Period Setup (P1–P9)

| Period | Formation | Estimation | Testing   |
| ------ | --------- | ---------- | --------- |
| P1     | 1926–1929 | 1930–1934  | 1935–1938 |
| P2     | 1927–1933 | 1934–1938  | 1939–1942 |
| P3     | 1931–1937 | 1938–1942  | 1943–1946 |
| P4     | 1935–1941 | 1942–1946  | 1947–1950 |
| P5     | 1939–1945 | 1946–1950  | 1951–1954 |
| P6     | 1943–1949 | 1950–1954  | 1955–1958 |
| P7     | 1947–1953 | 1954–1958  | 1959–1962 |
| P8     | 1951–1957 | 1958–1962  | 1963–1966 |
| P9     | 1955–1961 | 1962–1966  | 1967–1968 |

---

## 🧾 License

This project is for academic, educational, and replication use only. No guarantees. Not affiliated with any financial institution.


---

## 📬 Contact

Open a GitHub issue or email if you need help reproducing results or adapting the methodology.


