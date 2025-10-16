# 📘Replicating Tables 1–3 of Fama and MacBeth (1973)

This project replicates the key tables in a cross-sectional asset pricing study using Fama-MacBeth regressions. It includes:

- 📄 **Table 1**: Sample periods & eligible stock counts
- 📄 **Table 2**: Portfolio-level beta statistics
- 📄 **Table 3**: Fama-MacBeth regression results (4 model variants)

---

## 🧱 Repository Structure

```text
.
├── 01_extract/
│   └── data_extract_clean.R         # Pulls and cleans data from WRDS
├── 02_build/
│   ├── table_1_sec_count.R          # Generates Table 1 stock counts
│   ├── table_2_build_up.R           # Generates Table 2 beta stats
│   └── stage_builder.R              # Portfolio formation & FM regression logic
├── 03_models/						 # Reserved
├── outputs/						 # Replication report 
├── utils/
│   ├── connect_to_database.R        # WRDS connection script
│   └── global_functions.R           # Core helper/statistical functions
├── viz/
│   ├── plot_table2_betas.R          # Visualizations for Table 2 results
│   └── plot_fmb_gammas.R            # Visualizations for FMB γ estimates
├── outputs/                         # Exported tables and figures
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

```r
source("02_build/stage_builder.R")
```

Key steps:

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

**Script:** part of `02_build/stage_builder.R`

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

## 📈 Visualizations (`viz/`)

### 📉 `plot_table2_betas.R`

* Line plot of estimated portfolio betas across 20 portfolios
* Can highlight dispersion, linearity, noise

### 📈 `plot_fmb_gammas.R`

* Time-series plots of monthly γ₁ estimates (from `ret ~ beta`)
* Includes shaded confidence bands
* Optional: Add γ₂ and γ₃ if using Models B/C/D

Usage:

```r
source("viz/plot_table2_betas.R")
source("viz/plot_fmb_gammas.R")
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

| Function                 | Description                                |
| ------------------------ | ------------------------------------------ |
| `estimate_beta()`        | OLS for estimating beta, residual SD, etc. |
| `assign_20_portfolios()` | Ranks securities into 20 portfolios        |
| `ols_one_x(x, y)`        | Efficient 1-variable OLS (used everywhere) |
| `run_fmb()`              | Fama-MacBeth regression logic              |
| `fmb_coef_stats()`       | Computes gamma summary stats               |
| `count_block()`          | Counts eligible stocks for Table 1         |

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

## 📤 Exporting Tables

Export with `gt` or `kableExtra`:

```r
library(gt)
gt(stat_t2_list[[1]]) %>% gtsave("outputs/table2_P1.png")
```

---

## 🧾 License

This project is for academic, educational, and replication use only. No guarantees. Not affiliated with any financial institution.


---

## 👥 Authors

* **Peter YIN, Bing HAN**

---

## 📬 Contact

Open a GitHub issue or email if you need help reproducing results or adapting the methodology.

```

---

## ✅ How to Use

1. Copy the full `README.md` content above.
2. Paste it into your project root as `README.md`.
3. Optional:
   - Add screenshots of tables/plots
   - Add hyperlinks to `outputs/` folder
   - Create `.Rproj` file for reproducibility

Let me know if you'd like a PDF version or an `.Rmd` version renderable via **RMarkdown**.
```
