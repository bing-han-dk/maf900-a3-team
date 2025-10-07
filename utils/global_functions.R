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