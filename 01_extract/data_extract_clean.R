# crsp monthly retrun data #

res_ret <- dbSendQuery(wrds, "
                  SELECT 
                    a.permno, a.date, a.ret
                  FROM crsp.msf AS a
                  LEFT JOIN crsp.msenames AS b
                         ON a.permno = b.permno
                        AND b.namedt <= a.date
                        AND (b.nameendt IS NULL OR a.date <= b.nameendt)
                  WHERE  a.date BETWEEN '1926-01-01' AND '1968-06-30'
                    AND  b.shrcd IN (10,11)        -- common shares
                    AND  b.exchcd = 1              -- NYSE
    
      ")
data_ret <- dbFetch(res_ret)
dbClearResult(res_ret)


# crsp delist data 
res_delist <- dbSendQuery(wrds, "
                  select permno,permco,dlstdt,dlret
                  from crsp_a_stock.msedelist 
                  where dlstdt < '1968-07-01';
      ")
data_delist <- dbFetch(res_delist)
dbClearResult(res_delist)


# risk free data 
res_factor <- dbSendQuery(wrds, "
                  select dateff, rf
                  from ff_all.factors_monthly 
                  where date < '1968-07-01';
      ")
data_factor <- dbFetch(res_factor)
dbClearResult(res_factor)

data_factor<-data_factor %>% mutate(year = year(dateff), month = month(dateff))

data_ret <- data_ret %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  mutate(
    mkt = mean(ret, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  filter(!is.na(ret))

msf <- data_ret %>%
  select(permno, month, ret)
