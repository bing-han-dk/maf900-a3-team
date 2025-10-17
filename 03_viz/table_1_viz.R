render_gt <- function(panel_df) {
  panel_df <- tibble::as_tibble(panel_df)
  gt::gt(panel_df) |>
    gt::tab_header(
      title    = gt::md("**TABLE 1**"),
      subtitle = gt::md("**PORTFOLIO FORMATION, ESTIMATION, AND TESTING PERIODS**")
    ) |>
    gt::tab_spanner(label = "PERIODS", columns = 2:ncol(panel_df)) |>
    gt::cols_align(align = "center", columns = 2:ncol(panel_df)) |>
    gt::cols_align(align = "left",   columns = 1) |>
    gt::fmt_markdown(columns = 1) |>
    gt::cols_label(row_label = gt::md("&nbsp;")) |>
    gt::tab_options(
      # remove the bold line above the table title
      table.border.top.width   = gt::px(0),           # or: table.border.top.style = "none"
      
      # make the line under the title a double rule
      heading.border.bottom.style = "double",
      heading.border.bottom.width = gt::px(3),
      heading.border.bottom.color = "black",
      
      # avoid a competing border between heading and column labels
      column_labels.border.top.width = gt::px(0),
      
      # keep your existing grid/border choices
      table_body.hlines.width  = gt::px(0),
      table_body.vlines.width  = gt::px(0),
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      table.border.bottom.width = gt::px(1),
      
      # typography
      table.font.names = "Times New Roman",
      table.font.size  = gt::px(12),
      column_labels.font.weight = "bold",
      data_row.padding = gt::px(2),
      table.width      = gt::px(640)
    )
}

render_gt_ctnd <- function(panel_df) {
  panel_df <- tibble::as_tibble(panel_df)
  gt::gt(panel_df) |>
    gt::tab_header(
      title    = gt::md("**TABLE 1 (Continued)**")
    ) |>
    gt::tab_spanner(label = "PERIODS", columns = 2:ncol(panel_df)) |>
    gt::cols_align(align = "center", columns = 2:ncol(panel_df)) |>
    gt::cols_align(align = "left",   columns = 1) |>
    gt::fmt_markdown(columns = 1) |>
    gt::cols_label(row_label = gt::md("&nbsp;")) |>
    gt::tab_options(
      # remove the bold line above the table title
      table.border.top.width   = gt::px(0),           # or: table.border.top.style = "none"
      
      # make the line under the title a double rule
      heading.border.bottom.style = "double",
      heading.border.bottom.width = gt::px(3),
      heading.border.bottom.color = "black",
      
      # avoid a competing border between heading and column labels
      column_labels.border.top.width = gt::px(0),
      
      # keep your existing grid/border choices
      table_body.hlines.width  = gt::px(0),
      table_body.vlines.width  = gt::px(0),
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      table.border.bottom.width = gt::px(1),
      
      # typography
      table.font.names = "Times New Roman",
      table.font.size  = gt::px(12),
      column_labels.font.weight = "bold",
      data_row.padding = gt::px(2),
      table.width      = gt::px(640)
    )
}

render_gt(panel_1_5)
render_gt_ctnd(panel_6_9)
