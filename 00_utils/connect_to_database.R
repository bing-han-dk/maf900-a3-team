library(RPostgres)

# Log into WRDS database: Change the user name into your own #

wrds <- DBI::dbConnect(
  RPostgres::Postgres(),
  host   = "wrds-pgdata.wharton.upenn.edu",
  port   = 9737,
  dbname = "wrds",
  sslmode = "require",
  user   = "s225233606"
)
