# Load necessary packages #

library(RPostgres)
library(dplyr)
library(lubridate)
library(tidyverse)
library(broom)
library(DBI)
library(tidyr)
library(stringr)
library(knitr)
library(kableExtra)
library(gt)
library(purrr)


# ==============================
# Helpers used in Table 1
# ==============================

# Helper to build month sequences (first-of-month convention)
mon_seq <- function(start, end) seq(from = start, to = end, by = "month")

count_block <- function(p_row, msf_df) {
  # Period windows
  test_m1 <- p_row$test_start
  est_months  <- mon_seq(p_row$est_start,  p_row$est_end)
  form_months <- mon_seq(p_row$form_start, p_row$form_end)
  
  # Securities available in the **first month** of test period
  avail <- msf_df %>%
    filter(month == test_m1) %>%
    filter(!is.na(ret)) %>%        # must have a return that month
    distinct(permno)
  
  n_avail <- nrow(avail)
  
  # Coverage in estimation window (must be complete 60 months). ', !is.na(ret)' might be needed in line 102
  est_cov <- msf_df %>%
    filter(month %in% est_months) %>%
    count(permno, name = "n_est") %>%
    filter(n_est == length(est_months)) %>%
    select(permno)
  
  # Coverage in formation window (>= 48 months). ', !is.na(ret)' might be needed in line 109
  form_cov <- msf_df %>%
    filter(month %in% form_months) %>%
    count(permno, name = "n_form") %>%
    filter(n_form >= 48) %>%
    select(permno)
  
  eligible <- avail %>% 
    inner_join(est_cov, by = "permno") %>%
    inner_join(form_cov, by = "permno")
  
  n_eligible <- nrow(eligible)
  
  tibble(
    Period = p_row$period,
    `Portfolio formation period` = paste(format(p_row$form_start, "%Y"), format(p_row$form_end, "%Y"), sep = "–"),
    `Initial estimation period`  = paste(format(p_row$est_start,  "%Y"), format(p_row$est_end,  "%Y"), sep = "–"),
    `Testing period`             = paste(format(p_row$test_start, "%Y"), format(p_row$test_end, "%Y"), sep = "–"),
    `No. of securities available` = n_avail,
    `No. of securities meeting data requirement` = n_eligible
  )
}

# Compact "YYYY–YYYY" to "YYYY–YY"
yy2 <- function(s, e) sprintf("%s–%s", format(s, "%Y"), stringr::str_sub(format(e, "%Y"), -2))

make_panel_df <- function(which_periods) {
  t_counts <- tbl1    |> dplyr::filter(Period %in% which_periods) |> dplyr::arrange(Period)
  t_dates  <- periods |> dplyr::filter(period %in% which_periods) |> dplyr::arrange(period)
  
  row1 <- c("Portfolio formation period ...",
            purrr::map2_chr(t_dates$form_start, t_dates$form_end, yy2))
  row2 <- c("Initial estimation period ......",
            purrr::map2_chr(t_dates$est_start,  t_dates$est_end,  yy2))
  row3 <- c("Testing period .................",
            purrr::map2_chr(t_dates$test_start, t_dates$test_end, yy2))
  row4 <- c("No. of securities available ....",
            formatC(t_counts$`No. of securities available`, format = "d", big.mark = ","))
  row5 <- c("No. of securities meeting<br>&nbsp;&nbsp;&nbsp;&nbsp;data requirement ........",
            formatC(t_counts$`No. of securities meeting data requirement`, format = "d", big.mark = ","))
  
  out <- rbind(row1, row2, row3, row4, row5) |>
    as.data.frame(check.names = FALSE, stringsAsFactors = FALSE)
  
  # <-- KEY CHANGE: give the first column a proper name (was "")
  names(out) <- c("row_label", as.character(which_periods))
  out
}


# ==============================
# Helpers used in Table 2
# ==============================
mon_seq <- function(start_date, end_date) {
  start <- as.Date(paste0(year(start_date), "-", month(start_date), "-01"))
  end   <- as.Date(paste0(year(end_date),   "-", month(end_date),   "-01"))
  as.Date((seq(start, end, by = "month") + months(1) - days(1)))
}

ols_one_x <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(y)
  if (n < 3 || sd(x) == 0) return(list(beta = NA_real_, r2 = NA_real_, s_res = NA_real_, s_y = sd(y), n = n))
  bx <- cov(x, y) / var(x)
  a  <- mean(y) - bx * mean(x)
  yhat <- a + bx * x
  res  <- y - yhat
  r2   <- if (var(y) == 0) 0 else 1 - var(res)/var(y)
  sres <- sd(res)
  list(beta = bx, r2 = r2, s_res = sres, s_y = sd(y), n = n)
}

assign_20_portfolios <- function(df_ranked) {
  N <- nrow(df_ranked)
  base <- floor(N / 20)
  L <- N - base * 20
  counts <- rep(base, 20)
  counts[1]  <- base + floor(L/2)
  counts[20] <- base + ceiling(L/2)
  cuts <- cumsum(counts)
  df_ranked %>%
    mutate(port = findInterval(row_number(), cuts) + 1L) %>%
    mutate(port = pmin(port, 20L))
}


