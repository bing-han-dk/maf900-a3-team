library(dplyr)
library(lubridate)
library(tidyverse)
library(broom)


source("02_build/stage_functions.R")


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
    
    message(length(unique(estimatation_data$permno)), " stocks in estimatation_data")
    
    beta_e <- estimatation_data %>% 
      group_by(permno) %>%
      do(estimate_beta(data = ., min_obs = 60)) %>%
      ungroup() %>%
      filter(!is.na(beta))
    
    message(length(unique(beta_e$permno)), " stocks in beta_e")
    
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
  
  
  
  # TODO: collect all stat_t2


}
# --- end loop

