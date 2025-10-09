# Log into WRDS database: Change the user name into your own #

wrds <- DBI::dbConnect(
  RPostgres::Postgres(),
  host   = "wrds-pgdata.wharton.upenn.edu",
  port   = 9737,
  dbname = "wrds",
  sslmode = "require",
  user   = "your_own_user_name"
)