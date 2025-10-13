library(dplyr)
library(tidyverse)
library(broom)
library(purrr)
library(tibble)
library(tidyr)


source("01_extract/data_extract_clean.R")
source("utils/global_functions.R")



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

stat_t2_list <- list()


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
  
# I. formation stage
  
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

  

# II. estimation stage 
  
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
    
    # portfolio beta of year i (all months)
    beta_p_year <- purrr::map_dfr(1:12, function(m) {
      
      # merge month i's delisting return
      data_ret_m <- data_ret %>%
        filter(year == i, month == m) %>%
        left_join(
          data_delist %>% select(permno, dlstdt, dlret)
          ,by = "permno"
        ) %>%
        mutate(
          ret = ifelse(
            !is.na(dlstdt)&year(dlstdt)==i&month(dlstdt)==m,
            (1+ret)*(1+coalesce(dlret, 0))-1, ret))
      
      # make delisting set
      first_date  <- as.Date(sprintf("%d-%02d-01", i, m))
      delist_set <- data_delist %>%
        filter(!is.na(dlstdt), dlstdt < first_date) %>%
        pull(permno)
      
      # merge beta_f, beta_e and data_ret, excluding delisted 
      df_m <- beta_f %>%
        inner_join(beta_e, by = "permno") %>%
        filter(!(permno %in% delist_set)) %>%
        inner_join(
          data_ret_m %>% select(permno, ret),
          by = "permno"
        ) %>%
        select(permno, portfolio, beta = beta.y, sd_resid = sd_resid.y, ret)
      
      if (i == tstart && m == 1) {
        N2<-length(unique(df_m$permno))
        message("No. of securities meeting data requirements: ", N2)
      }
      
      # calculate portfolio beta, se and return 
      df_m %>%
        group_by(portfolio) %>%
        summarise(
          beta = mean(beta, na.rm = TRUE),
          sd_resid = mean(sd_resid,   na.rm = TRUE),
          ret  = mean(ret,  na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(year = i, month = m) %>%
        select(year, month, portfolio, beta, sd_resid, ret)
    })
    
    beta_p_year
    
  })
  
  
  result_all <- rbind(result_all, beta_p_all)
  

  
  # portfolio level estimation (table 2)
  beta_p_est <- beta_f %>%
    inner_join(
      data_ret %>%
        filter(year >= estart & year <= eend) %>%
        select(permno, ret, mkt, year, month),
      by = "permno"
    ) %>%
    group_by(year, month, portfolio) %>%
    summarise(
      ret = mean(ret, na.rm = TRUE), 
      mkt = mean(mkt, na.rm = TRUE), 
      .groups = "drop"
    )%>%
    group_by(portfolio) %>%
    do(estimate_beta(data = ., min_obs = 60)) %>%
    ungroup()
  
  # --- table 2
  stat_t2 <- beta_p_est %>%
    left_join(
      beta_p_all %>%
        filter(year == tstart, month == 1) %>% 
        select(portfolio, sd_resid_i = sd_resid),
      by = "portfolio"
    ) %>%
    mutate(sd_resid_over = sd_resid / sd_resid_i)
  
  stat_t2_t <- t(stat_t2)
  stat_t2_list[[p]] <- stat_t2

}
# --- end loop




# III. test stage(two parameter regression)


# getting all beta_p (8040 obs.)
result_all <- result_all %>%
  mutate(beta2 = beta^2)


periods_2 <- list(
  "1935-1968" = c(1935, 1968),
  "1935-1945" = c(1935, 1945),
  "1946-1955" = c(1946, 1955),
  "1956-1968" = c(1956, 1968),
  "1935-1940" = c(1935, 1940),
  "1941-1945" = c(1941, 1945),
  "1946-1950" = c(1946, 1950),
  "1951-1955" = c(1951, 1955),
  "1956-1960" = c(1956, 1960),
  "1961-1968" = c(1961, 1968)
)


model_list <- list(
  "Model A" = "ret ~ beta",
  "Model B" = "ret ~ beta + beta2",
  "Model C" = "ret ~ beta + sd_resid",
  "Model D" = "ret ~ beta + beta2 + sd_resid"
)


fmb_results_all <- list()


for (period_name in names(periods_2)) {
  message("Processing ", period_name)
  
  years <- periods_2[[period_name]]
  data_input <- result_all %>% filter(year >= years[1], year <= years[2])
  
  for (model_name in names(model_list)) {
    formula <- model_list[[model_name]]
    
    fmb_model <- run_fmb(data_input, formula)
    fmb_coef <- fmb_coef_stats(fmb_model, data_factor)
    
    # Reshape data to wide format, matching column names to terms in the data
    row <- fmb_coef %>%
      select(term, mean_gamma, sd_gamma, acf1, acf1_gamma_rf, mean_gamma_rf, t_stat, t_gamma_rf) %>%
      pivot_wider(
        names_from = term,
        values_from = c(mean_gamma, sd_gamma, acf1, acf1_gamma_rf, mean_gamma_rf, t_stat, t_gamma_rf),
        names_glue = "{.value}({term})"
      ) %>%
      mutate(
        model = model_name,
        period = period_name,
        r2 = mean(fmb_coef$mean_r2, na.rm = TRUE),
        sd_r2 = mean(fmb_coef$sd_r2, na.rm = TRUE)
      )
    
    fmb_results_all[[paste(model_name, period_name, sep = "_")]] <- row
  }
}


# Combine all
fmb_results_df <- bind_rows(fmb_results_all)
rm(fmb_results_df)


desired_order <- c(
  "model", "period",
  "mean_gamma((Intercept))", "mean_gamma(beta)", "mean_gamma(beta2)", "mean_gamma(sd_resid)",
  "mean_gamma_rf((Intercept))", "mean_gamma_rf(beta)", "mean_gamma_rf(beta2)", "mean_gamma_rf(sd_resid)",
  "sd_gamma((Intercept))", "sd_gamma(beta)", "sd_gamma(beta2)", "sd_gamma(sd_resid)",
  "acf1_gamma_rf((Intercept))", "acf1_gamma_rf(beta)", "acf1_gamma_rf(beta2)", "acf1_gamma_rf(sd_resid)",
  "acf1((Intercept))", "acf1(beta)", "acf1(beta2)", "acf1(sd_resid)",
  "t_stat((Intercept))", "t_stat(beta)", "t_stat(beta2)", "t_stat(sd_resid)",
  "t_gamma_rf((Intercept))", "t_gamma_rf(beta)", "t_gamma_rf(beta2)", "t_gamma_rf(sd_resid)",
  "r2", "sd_r2"
)


# re-order columns 
fmb_results_df <- fmb_results_df %>%
  select(all_of(desired_order))

# remove useless columns 
fmb_results_df <- fmb_results_df %>%
  select(
    -`mean_gamma_rf(beta)`,
    -`mean_gamma_rf(beta2)`,
    -`mean_gamma_rf(sd_resid)`,
    -`acf1_gamma_rf(beta)`,
    -`acf1_gamma_rf(beta2)`,
    -`acf1_gamma_rf(sd_resid)`,
    -`acf1((Intercept))`,
    -`t_gamma_rf(beta)`,
    -`t_gamma_rf(beta2)`,
    -`t_gamma_rf(sd_resid)`
  )


# order rows by model and period
period_levels <- names(periods_2)
fmb_results_df <- fmb_results_df %>%
  mutate(
    period = factor(period, levels = period_levels)
  ) %>%
  arrange(model, period)


# fmb_results_df is the result of Table 3 

# --- build stages end ----

