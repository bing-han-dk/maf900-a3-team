library(dplyr)


# --- functions start ----


# beta regression
estimate_beta <- function(data, min_obs = 1) {
  if (nrow(data) < min_obs) {
    return(data.frame(beta=NA,se_beta=NA,R2=NA
                      ,sd_ret=NA,sd_resid=NA)) 
  } else {
    fit <- lm(ret ~ mkt, data = data)
    beta<- as.numeric(coefficients(fit)[2])
    se_beta <- summary(fit)$coefficients["mkt", "Std. Error"]
    R2  <- summary(fit)$r.squared
    sd_ret <- sd(data$ret, na.rm = TRUE)
    sd_resid <- sd(resid(fit)) 
    return(data.frame(beta=beta,se_beta=se_beta,R2=R2
                      ,sd_ret=sd_ret,sd_resid=sd_resid))
  }
}


# portfolio formation function
assign_portfolios <- function(beta_df, n_port = 20) {
  N <- nrow(beta_df)
  n_each <- floor(N / n_port)
  remainder <- N - n_each * n_port
  extra_first <- floor(remainder / 2)
  extra_last  <- ceiling(remainder / 2)
  beta_df <- beta_df %>% arrange(beta)
  if (n_port > 2) {
    mid_portfolios <- rep(2:(n_port-1), each = n_each)
  } else {
    mid_portfolios <- c()
  }
  portfolio_vec <- c(
    rep(1, n_each + extra_first),
    mid_portfolios,
    rep(n_port, n_each + extra_last)
  )
  if (length(portfolio_vec) != N) {
    stop("Length mismatch: check input data")
  }
  beta_df$portfolio <- portfolio_vec
  return(beta_df)
}


# two parameter regression function 
run_fmb <- function(data, formula_str) {
  data %>%
    group_by(year, month) %>%
    group_modify(~ {
      model <- lm(as.formula(formula_str), data = .x)
      tidy_res <- broom::tidy(model)
      tidy_res$r_squared <- summary(model)$r.squared
      tidy_res
    }) %>%
    ungroup()
}


# summary statistics (mean, t-stat, autocorrelation, R²) 
# for Fama-MacBeth regression coefficients across time.
fmb_coef_stats <- function(fmb_model, rf_data) {
  fmb_model %>%
    left_join(rf_data %>% select(year, month, rf), by = c("year", "month")) %>%
    group_by(term) %>%
    summarise(
      mean_gamma = mean(estimate, na.rm = TRUE),
      sd_gamma = sd(estimate, na.rm = TRUE),
      t_stat = mean_gamma / (sd_gamma / sqrt(n())),
      
      acf1 = acf(estimate, plot = FALSE)$acf[2],  # ρ₀(γ)
      
      mean_gamma_rf = mean(estimate - rf, na.rm = TRUE),
      t_gamma_rf = mean_gamma_rf / (sd_gamma / sqrt(n())),
      acf1_gamma_rf = acf(estimate - rf, plot = FALSE)$acf[2],
      
      mean_r2 = mean(r_squared, na.rm = TRUE),
      sd_r2 = sd(r_squared, na.rm = TRUE),
      
      .groups = "drop"
    )
}


# --- functions end ---
