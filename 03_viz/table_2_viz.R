# ==============================
# Present Table 2 like the paper
# ==============================
fmt_three <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))

make_panel <- function(df_stats, label) {
  left  <- df_stats %>% filter(port <= 10)
  right <- df_stats %>% filter(port >= 11)
  
  stat_names <- c(
    "β̂_{p,t−1} ............",
    "s(β̂_{p,t−1}) .........",
    "r(R_p, R_m)^2 .........",
    "s(R_p) ................",
    "s(ε_p) ................",
    "s̄_{p,t−1}(ε̂_i) .......",
    "s(ε_p)/s̄_{p,t−1}(ε̂_i) .."
  )
  
  build_side <- function(x) {
    rbind(
      c("β̂_{p,t−1} ............",       fmt_three(x$beta_p)),
      c("s(β̂_{p,t−1}) .........",        fmt_three(x$se_beta_p)),
      c("r(R_p, R_m)^2 .........",        fmt_three(x$r2)),
      c("s(R_p) ................",         fmt_three(x$s_Rp)),
      c("s(ε_p) ................",         fmt_three(x$s_eps_p)),
      c("s̄_{p,t−1}(ε̂_i) .......",        fmt_three(x$sbar_eps_i)),
      c("s(ε_p)/s̄_{p,t−1}(ε̂_i) ..",      fmt_three(x$ratio))
    ) |>
      as.data.frame() |>
      setNames(c("Statistic", paste0(x$port)))
  }
  
  L <- build_side(left);  R <- build_side(right)
  
  list(
    title = paste0("TABLE 2 — Sample Statistics for Estimation Period ", label),
    left  = L, right = R
  )
}

panels <- tabs_tbl2 %>%
  mutate(panel = map2(data, label, ~ make_panel(.x, .y))) %>%
  pull(panel)

render_gt_panel <- function(panel_side, title, header_suffix = "Portfolios 1–10") {
  port_cols <- setdiff(names(panel_side), "Statistic")
  sp_id <- paste0("sp_", gsub("[^A-Za-z0-9]+", "_", header_suffix))
  
  gt::gt(panel_side) |>
    gt::tab_header(
      title    = gt::md("**TABLE 2**"),
      subtitle = gt::md(paste0(
        "**SAMPLE STATISTICS FOR FOUR SELECTED ESTIMATION PERIODS**<br>",
        header_suffix
      ))
    ) |>
    gt::cols_label(Statistic = gt::md("Statistic")) |>
    gt::tab_spanner(label = header_suffix, id = sp_id, columns = port_cols) |>
    gt::cols_align(align = "center", columns = port_cols) |>
    gt::cols_align(align = "left",   columns = "Statistic") |>
    gt::tab_options(
      table.border.top.width   = gt::px(0),
      heading.border.bottom.style = "double",
      heading.border.bottom.width = gt::px(1),
      heading.border.bottom.color = "black",
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      table.border.bottom.width = gt::px(1),
      table.border.bottom.color = "black",
      table.font.names = "Times New Roman",
      table.font.size  = gt::px(12),
      column_labels.font.weight = "bold",
      data_row.padding = gt::px(2),
      table.width      = gt::px(720),
      table_body.hlines.width = gt::px(0),
      table_body.vlines.width = gt::px(0)
    )
}

# 1) Bind the four “left” panels (ports 1–10), tagging each block
tbl2_page1_df <- bind_rows(
  panels[[1]]$left |> mutate(group = "Portfolios for Estimation Period 1934–38"),
  panels[[2]]$left |> mutate(group = "Portfolios for Estimation Period 1942–46"),
  panels[[3]]$left |> mutate(group = "Portfolios for Estimation Period 1950–54"),
  panels[[4]]$left |> mutate(group = "Portfolios for Estimation Period 1958–62")
) |> 
  relocate(group)

tbl2_page2_df <- bind_rows(
  panels[[1]]$right |> mutate(group = "Portfolios for Estimation Period 1934–38"),
  panels[[2]]$right |> mutate(group = "Portfolios for Estimation Period 1942–46"),
  panels[[3]]$right |> mutate(group = "Portfolios for Estimation Period 1950–54"),
  panels[[4]]$right |> mutate(group = "Portfolios for Estimation Period 1958–62")
) |> 
  relocate(group)

# 2) Render as one gt table with row groups (journal styling)
W <- gt::px(1)

tbl2_page1 <- gt::gt(
  data = tbl2_page1_df,
  groupname_col = "group"
) |>
  gt::tab_header(
    title    = gt::md("**TABLE 2**"),
    subtitle = gt::md("**SAMPLE STATISTICS FOR FOUR SELECTED ESTIMATION PERIODS**")
  ) |>
  gt::cols_label(Statistic = gt::md("Statistic")) |>
  gt::cols_align(align = "left",   columns = "Statistic") |>
  gt::cols_align(align = "center", columns = setdiff(names(tbl2_page1_df), c("group","Statistic"))) |>
  gt::fmt_markdown(columns = "Statistic") |>
  gt::tab_options(
    table.border.top.width   = gt::px(0),
    heading.border.bottom.style = "double",
    heading.border.bottom.width = W,
    heading.border.bottom.color = "black",
    column_labels.border.top.width    = gt::px(0),
    column_labels.border.bottom.width = W,
    column_labels.border.bottom.color = "black",
    row_group.border.top.width    = W,
    row_group.border.top.color    = "black",
    row_group.border.bottom.width = W,
    row_group.border.bottom.color = "black",
    table_body.hlines.width  = gt::px(0),
    table_body.vlines.width  = gt::px(0),
    table.border.left.width  = W,
    table.border.left.color  = "black",
    table.border.right.width = W,
    table.border.right.color = "black",
    table.border.bottom.width= W,
    table.border.bottom.color= "black",
    table.font.names = "Times New Roman",
    table.font.size  = gt::px(12),
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(2),
    table.width      = gt::px(720)
  )

tbl2_page2 <- gt::gt(
  data = tbl2_page2_df,
  groupname_col = "group"
) |>
  gt::tab_header(
    title    = gt::md("**TABLE 2 (Continued)**"),
    subtitle = gt::md("**SAMPLE STATISTICS FOR FOUR SELECTED ESTIMATION PERIODS**")
  ) |>
  gt::cols_label(Statistic = gt::md("Statistic")) |>
  gt::cols_align(align = "left",   columns = "Statistic") |>
  gt::cols_align(align = "center", columns = setdiff(names(tbl2_page2_df), c("group","Statistic"))) |>
  gt::fmt_markdown(columns = "Statistic") |>
  gt::tab_options(
    table.border.top.width   = gt::px(0),
    heading.border.bottom.style = "double",
    heading.border.bottom.width = W,
    heading.border.bottom.color = "black",
    column_labels.border.top.width    = gt::px(0),
    column_labels.border.bottom.width = W,
    column_labels.border.bottom.color = "black",
    row_group.border.top.width    = W,
    row_group.border.top.color    = "black",
    row_group.border.bottom.width = W,
    row_group.border.bottom.color = "black",
    table_body.hlines.width  = gt::px(0),
    table_body.vlines.width  = gt::px(0),
    table.border.left.width  = W,
    table.border.left.color  = "black",
    table.border.right.width = W,
    table.border.right.color = "black",
    table.border.bottom.width= W,
    table.border.bottom.color= "black",
    table.font.names = "Times New Roman",
    table.font.size  = gt::px(12),
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(2),
    table.width      = gt::px(720)
  )

# print it
tbl2_page1
tbl2_page2
