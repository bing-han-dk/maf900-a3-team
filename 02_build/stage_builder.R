library(dplyr)
library(lubridate)
library(tidyverse)
library(broom)


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



# --- build stages start ----

# define periods
periods <- list(
  list(name = "P1",
       fstart = 1926, fend = 1929,
       estart = 1930, eend = 1934,
       tstart = 1935, tend = 1938
  ),
  list(name = "P2",
       fstart = 1927, fend = 1933,
       estart = 1934, eend = 1938,
       tstart = 1939, tend = 1942
  ),
  list(name = "P3",
       fstart = 1931, fend = 1937,
       estart = 1938, eend = 1942,
       tstart = 1943, tend = 1946
  ),
  list(name = "P4",
       fstart = 1935, fend = 1941,
       estart = 1942, eend = 1946,
       tstart = 1947, tend = 1950
  ),
  list(name = "P5",
       fstart = 1939, fend = 1945,
       estart = 1946, eend = 1950,
       tstart = 1951, tend = 1954
  ),
  list(name = "P6",
       fstart = 1943, fend = 1949,
       estart = 1950, eend = 1954,
       tstart = 1955, tend = 1958
  ),
  list(name = "P7",
       fstart = 1947, fend = 1953,
       estart = 1954, eend = 1958,
       tstart = 1959, tend = 1962
  ),
  list(name = "P8",
       fstart = 1951, fend = 1957,
       estart = 1958, eend = 1962,
       tstart = 1963, tend = 1966
  ),
  list(name = "P9",
       fstart = 1955, fend = 1961,
       estart = 1962, eend = 1966,
       tstart = 1967, tend = 1968
  )
)

# define result object to save portfolio betas 
result_all <- data.frame()


# --- loop start (for P1-P9)

for (p in 1:length(periods)) {
  period <- periods[[p]]
  
  message("Processing period ", period$name, " ...")
  
  fstart <- period$fstart
  fend   <- period$fend
  estart <- period$estart
  eend   <- period$eend
  tstart <- period$tstart
  tend   <- period$tend

  N1<-length(unique((data_ret %>% filter(year == tstart & month ==1))$permno))
  message("No. of securities available: ", N1)
  
# formation stage
  
  formation_data<-data_ret %>% filter(year >= fstart & year <= fend)
  
  beta_f <- formation_data %>% 
    group_by(permno) %>%
    do(estimate_beta(data = ., min_obs = 48)) %>% 
    ungroup()%>%
    filter(!is.na(beta))
  
  # assign portfolios
  beta_f<-assign_portfolios(beta_f)
  
  # check assigned result
  beta_f %>% count(portfolio) 



# estimation stage 
  
  # portfolio beta of entire period 
  beta_p_all <- purrr::map_dfr(tstart:tend, function(i) {
    # Using purrr::map_dfr & anonymous function can effectively 
    # avoid using for loop to generate too many intermediate variables,
    # and avoid passing too many parameters when defining normal.
    
    message("Processing year ", i, " ...")
    
    n <- i - tstart
    
    # re-cumpute individual beta & s(e)
    estimatation_data<-data_ret %>% filter(year >= estart & year <= (eend+n))%>%
      filter(!is.na(ret)) # ------------> 
    
    beta_e <- estimatation_data %>% 
      group_by(permno) %>%
      do(estimate_beta(data = ., min_obs = 60)) %>%
      ungroup() %>%
      filter(!is.na(beta))
    
    # TODO: portfolio beta of year i (all months)
    
  })
  
  result_all <- rbind(result_all, beta_p_all)
  


# portfolio level estimation (table 2)
# TODO:


}
# --- end loop

