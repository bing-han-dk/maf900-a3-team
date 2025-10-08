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


# Log into WRDS database: Change the user name into your own #

wrds <- DBI::dbConnect(
  RPostgres::Postgres(),
  host   = "wrds-pgdata.wharton.upenn.edu",
  port   = 9737,
  dbname = "wrds",
  sslmode = "require",
  user   = "your_own_user_name"
)

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

