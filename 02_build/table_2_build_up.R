
# ==============================
# Period setup (four selected estimation windows → Table 2)
# ==============================
periods_tbl2 <- tribble(
  ~label,                  ~form_start, ~form_end, ~est_start, ~est_end, ~test_start,
  "1934–38", as.Date("1927-01-31"), as.Date("1933-12-31"), as.Date("1934-01-31"), as.Date("1938-12-31"), as.Date("1939-01-31"),
  "1942–46", as.Date("1935-01-31"), as.Date("1941-12-31"), as.Date("1942-01-31"), as.Date("1946-12-31"), as.Date("1947-01-31"),
  "1950–54", as.Date("1943-01-31"), as.Date("1949-12-31"), as.Date("1950-01-31"), as.Date("1954-12-31"), as.Date("1955-01-31"),
  "1958–62", as.Date("1951-01-31"), as.Date("1957-12-31"), as.Date("1958-01-31"), as.Date("1962-12-31"), as.Date("1963-01-31")
)

# ==============================
# Core routine for one estimation period
# ==============================
compute_table2_one <- function(form_start, form_end, est_start, est_end, test_start) {
  form_months <- mon_seq(form_start, form_end)
  est_months  <- mon_seq(est_start,  est_end)
  
  # --- Eligibility (exists at first test month; full 60m in est; >=48m in formation)
  avail <- msf_2 %>%
    filter(month == test_start, !is.na(ret)) %>%
    distinct(permno)
  
  est_ok <- msf_2 %>%
    filter(month %in% est_months, !is.na(ret)) %>%
    count(permno, name = "n_est") %>%
    filter(n_est == length(est_months)) %>%
    select(permno)
  
  form_ok <- msf_2 %>%
    filter(month %in% form_months, !is.na(ret)) %>%
    count(permno, name = "n_form") %>%
    filter(n_form >= 48) %>%
    select(permno)
  
  universe <- avail %>% inner_join(est_ok, by = "permno") %>% inner_join(form_ok, by = "permno")
  
  # --- NEW: Dynamic market series from *eligible universe* --------------------
  mkt_form <- msf_2 %>%
    semi_join(universe, by = "permno") %>%
    filter(month %in% form_months) %>%
    group_by(month) %>%
    summarise(mkt = mean(ret, na.rm = TRUE), .groups = "drop")
  
  mkt_est <- msf_2 %>%
    semi_join(universe, by = "permno") %>%
    filter(month %in% est_months) %>%
    group_by(month) %>%
    summarise(mkt = mean(ret, na.rm = TRUE), .groups = "drop")
  # ---------------------------------------------------------------------------
  
  # --- Formation-period betas → rank → 20 portfolios via FM allocation rule
  df_form <- msf_2 %>%
    semi_join(universe, by = "permno") %>%
    filter(month %in% form_months) %>%
    left_join(mkt_form, by = "month") %>%
    group_by(permno) %>%
    summarise(beta_form = { f <- ols_one_x(mkt, ret); f$beta }, .groups = "drop") %>%
    filter(is.finite(beta_form)) %>%
    arrange(beta_form) %>%
    mutate(rank = row_number()) %>%
    assign_20_portfolios() %>%
    select(permno, port)
  
  # --- Estimation window: per-security regression (uses mkt_est)
  sec_est <- msf_2 %>%
    semi_join(df_form, by = "permno") %>%
    filter(month %in% est_months) %>%
    left_join(mkt_est, by = "month") %>%
    group_by(permno) %>%
    summarise(fit = list(ols_one_x(mkt, ret)), .groups = "drop") %>%
    mutate(
      beta_i  = purrr::map_dbl(fit, "beta"),
      s_eps_i = purrr::map_dbl(fit, "s_res")
    ) %>%
    select(-fit) %>%
    inner_join(df_form, by = "permno")
  
  # --- Estimation window: monthly equal-weighted portfolio returns
  port_panel <- msf_2 %>%
    semi_join(df_form, by = "permno") %>%
    filter(month %in% est_months) %>%
    inner_join(df_form, by = "permno") %>%
    group_by(port, month) %>%
    summarise(Rp = mean(ret, na.rm = TRUE), .groups = "drop") %>%
    left_join(mkt_est, by = "month") %>%
    arrange(port, month)
  
  # --- Portfolio regressions and stats over estimation window (uses mkt_est)
  port_fit <- port_panel %>%
    group_by(port) %>%
    summarise(.fit = list(ols_one_x(mkt, Rp)), .groups = "drop") %>%
    mutate(
      beta_p  = purrr::map_dbl(.fit, "beta"),
      r2      = purrr::map_dbl(.fit, "r2"),
      s_Rp    = purrr::map_dbl(.fit, "s_y"),
      s_eps_p = purrr::map_dbl(.fit, "s_res"),
      n_obs   = purrr::map_int(.fit,  "n")
    ) %>%
    select(-.fit)
  
  # --- Avg per-stock residual SD within portfolios
  sec_agg <- sec_est %>%
    group_by(port) %>%
    summarise(sbar_eps_i = mean(s_eps_i, na.rm = TRUE), .groups = "drop")
  
  # --- Market SD & n-months in window (from mkt_est)
  rm_stats <- mkt_est %>%
    summarise(s_Rm = stats::sd(mkt, na.rm = TRUE), n_months = dplyr::n()) %>%
    as.list()
  
  tab <- port_fit %>%
    left_join(sec_agg, by = "port") %>%
    mutate(
      s_Rm     = rm_stats$s_Rm,
      n_months = rm_stats$n_months,
      se_beta_p = s_eps_p / (sqrt(n_months) * s_Rm),
      ratio     = s_eps_p / sbar_eps_i
    ) %>%
    arrange(port) %>%
    transmute(
      port, beta_p, se_beta_p, r2, s_Rp, s_eps_p, sbar_eps_i, ratio
    )
  
  tab
}

# ==============================
# Build Table 2 for the four selected estimation periods
# ==============================
tabs_tbl2 <- periods_tbl2 %>%
  mutate(
    data = pmap(
      list(form_start, form_end, est_start, est_end, test_start),
      compute_table2_one
    )
  )

