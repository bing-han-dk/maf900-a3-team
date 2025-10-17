

# Periods (formation, estimation, testing). Testing periods & counts appear split across two rows in FM Table 1.
periods <- tibble::tribble(
  ~period, ~form_start, ~form_end, ~est_start, ~est_end, ~test_start, ~test_end,
  1, "1926-01", "1929-12", "1930-01", "1934-12", "1935-01", "1938-12",
  2, "1927-01", "1933-12", "1934-01", "1938-12", "1939-01", "1942-12",
  3, "1931-01", "1937-12", "1938-01", "1942-12", "1943-01", "1946-12",
  4, "1935-01", "1941-12", "1942-01", "1946-12", "1947-01", "1950-12",
  5, "1939-01", "1945-12", "1946-01", "1950-12", "1951-01", "1954-12",
  6, "1943-01", "1949-12", "1950-01", "1954-12", "1955-01", "1958-12",
  7, "1947-01", "1953-12", "1954-01", "1958-12", "1959-01", "1962-12",
  8, "1951-01", "1957-12", "1958-01", "1962-12", "1963-01", "1966-12",
  9, "1955-01", "1961-12", "1962-01", "1966-12", "1967-01", "1968-06"
) %>%
  mutate(
    form_start = as.Date(paste0(form_start, "-01")),
    form_end   = as.Date(paste0(form_end, "-31")),
    est_start  = as.Date(paste0(est_start,  "-01")),
    est_end    = as.Date(paste0(est_end,    "-31")),
    test_start = as.Date(paste0(test_start, "-01")),
    test_end   = as.Date(paste0(test_end,   "-31"))
  )

periods$test_end[is.na(periods$test_end)] <- as.Date('1968-06-30')


# ---- 4) Core counting routine (exact FM eligibility rule) ----
# Rule: stock counted as "available" if it exists in the **first month of the testing period**.
#       stock counted as "meeting data requirement" if:
#         (i) it has non-missing RET in *all* months of the preceding 5-year estimation window (60 months), and
#         (ii) it has non-missing RET in >= 48 months during the formation window (4 years; if 7y window used, still need >= 48 months).
# Run for all 9 periods
tbl1 <- periods %>%
  rowwise() %>%
  do(count_block(., msf)) %>%
  ungroup()

panel_1_5 <- make_panel_df(1:5)
panel_6_9 <- make_panel_df(6:9)


