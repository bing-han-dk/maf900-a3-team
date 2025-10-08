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

# --- functions end ---