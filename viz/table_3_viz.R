

# =========================================================
# Fama–MacBeth (1973) Table 3 — ordered, robust gt renderer
# Input : fmb_results_df (structure per your prompt)
# Output: gt table; optional export if save_path is set
# =========================================================
make_table3 <- function(fmb_results_df, save_path = NULL) {
  
  # --- deps (quietly load without attaching) -------------------------------
  suppressPackageStartupMessages({
    requireNamespace("dplyr",    quietly = TRUE)
    requireNamespace("gt",       quietly = TRUE)
    requireNamespace("stringr",  quietly = TRUE)
  })
  `%>%` <- dplyr::`%>%`
  
  # --- canonical panel & period orders (Table 3 layout) --------------------
  panel_levels  <- c("Panel A", "Panel B", "Panel C", "Panel D")
  panel_levels_with_colon <- paste0(panel_levels, ":")
  
  period_levels <- c(
    "1935-1968",
    "1935-1945", "1946-1955", "1956-1968",
    "1935-1940", "1941-1945", "1946-1950",
    "1951-1955", "1956-1960", "1961-1968"
  )
  
  # --- columns to display in the Table 3 order -----------------------------
  cols_wanted <- c(
    "mean_gamma((Intercept))",
    "mean_gamma(beta)",
    "mean_gamma(beta2)",
    "mean_gamma(sd_resid)",
    "mean_gamma_rf((Intercept))",
    "sd_gamma((Intercept))",
    "sd_gamma(beta)",
    "sd_gamma(beta2)",
    "sd_gamma(sd_resid)",
    "acf1_gamma_rf((Intercept))",
    "acf1(beta)",
    "acf1(beta2)",
    "acf1(sd_resid)",
    "t_stat((Intercept))",
    "t_stat(beta)",
    "t_stat(beta2)",
    "t_stat(sd_resid)",
    "t_gamma_rf((Intercept))",
    "r2",
    "sd_r2"
  )
  
  # helper for no-wrap HTML labels
  nowrap <- function(x) gt::html(paste0("<span style='white-space:nowrap'>", x, "</span>"))
  
  # --- map Model -> Panel, enforce orders; create display period labels ----
  df <- fmb_results_df %>%
    dplyr::mutate(
      panel_raw = stringr::str_replace(.data$model, "^Model\\s+", "Panel "),
      period_txt_orig = as.character(.data$period),
      period_txt = dplyr::recode(
        period_txt_orig,
        "1935-1968" = "1935-6/68 . .",
        "1935-1945" = "1935-45 . . . .",
        "1946-1955" = "1946-55 . . . .",
        "1956-1968" = "1956-6/68 . .",
        "1935-1940" = "1935-40 . . . .",
        "1941-1945" = "1941-45 . . . .",
        "1946-1950" = "1946-50 . . . .",
        "1951-1955" = "1951-55 . . . .",
        "1956-1960" = "1956-60 . . . .",
        "1961-1968" = "1961-6/68 . .",
        .default = period_txt_orig
      ),
      panel  = factor(panel_raw, levels = panel_levels),
      period = factor(period_txt_orig, levels = period_levels, ordered = TRUE)
    )
  
  # ensure fixed shape
  missing_cols <- setdiff(cols_wanted, names(df))
  if (length(missing_cols)) df[missing_cols] <- NA_real_
  
  # order and keep columns
  df_show <- df %>%
    dplyr::arrange(.data$panel, .data$period) %>%
    dplyr::select(panel, period_txt, dplyr::all_of(cols_wanted))
  
  # missing-values helper
  replace_missing <- function(gt_tbl) {
    if ("sub_missing" %in% getNamespaceExports("gt")) {
      gt::sub_missing(gt_tbl, columns = gt::everything(), missing_text = ". . .")
    } else {
      gt::fmt_missing(gt_tbl, columns = gt::everything(), missing_text = ". . .")
    }
  }
  
  # build gt table
  gt_tbl <- df_show %>% gt::gt(rowname_col = "period_txt")
  
  # row groups A -> D with colons
  if ("row_group_order" %in% getNamespaceExports("gt")) {
    for (p in panel_levels) {
      idx <- which(df_show$panel == p)
      if (length(idx)) {
        gt_tbl <- gt::tab_row_group(gt_tbl, label = paste0(p, ":"), rows = idx)
      }
    }
    gt_tbl <- gt::row_group_order(gt_tbl, groups = panel_levels_with_colon)
  } else {
    for (p in rev(panel_levels)) {
      idx <- which(df_show$panel == p)
      if (length(idx)) {
        gt_tbl <- gt::tab_row_group(gt_tbl, label = paste0(p, ":"), rows = idx)
      }
    }
  }
  
  gt_tbl <- gt_tbl %>%
    gt::cols_hide(columns = "panel") %>%
    gt::tab_header(
      title = gt::md("**TABLE 3**"),
      subtitle = gt::html(
        "<b>SUMMARY RESULTS FOR THE REGRESSION</b><br/>
         <span style='font-family:Times'>
         R<sub>p</sub> = &gamma;&#770;<sub>0t</sub> + &gamma;&#770;<sub>1t</sub>&beta;<sub>p</sub> +
         &gamma;&#770;<sub>2t</sub>&beta;<sub>p</sub><sup>2</sup> +
         &gamma;&#770;<sub>3t</sub>&#772;s<sub>p</sub>(&#949;&#770;) + &eta;&#770;<sub>pt</sub>
         </span>"
      )
    ) %>%
    gt::tab_spanner(label = nowrap("STATISTIC"),
                    columns = dplyr::all_of(cols_wanted)) %>%
    gt::cols_label(
      `mean_gamma((Intercept))`    = nowrap("&gamma;&#770;<sub>0</sub>"),
      `mean_gamma(beta)`           = nowrap("&gamma;&#770;<sub>1</sub>"),
      `mean_gamma(beta2)`          = nowrap("&gamma;&#770;<sub>2</sub>"),
      `mean_gamma(sd_resid)`       = nowrap("&gamma;&#770;<sub>3</sub>"),
      `mean_gamma_rf((Intercept))` = nowrap("&gamma;&#770;<sub>0</sub> &minus; R<sub>f</sub>"),
      `sd_gamma((Intercept))`      = nowrap("s(&gamma;&#770;<sub>0</sub>)"),
      `sd_gamma(beta)`             = nowrap("s(&gamma;&#770;<sub>1</sub>)"),
      `sd_gamma(beta2)`            = nowrap("s(&gamma;&#770;<sub>2</sub>)"),
      `sd_gamma(sd_resid)`         = nowrap("s(&gamma;&#770;<sub>3</sub>)"),
      `acf1_gamma_rf((Intercept))` = nowrap("&rho;<sub>0</sub>(&gamma;&#770;<sub>0</sub> &minus; R<sub>f</sub>)"),
      `acf1(beta)`                 = nowrap("&rho;(&gamma;&#770;<sub>1</sub>)"),
      `acf1(beta2)`                = nowrap("&rho;(&gamma;&#770;<sub>2</sub>)"),
      `acf1(sd_resid)`             = nowrap("&rho;(&gamma;&#770;<sub>3</sub>)"),
      `t_stat((Intercept))`        = nowrap("t(&gamma;&#770;<sub>0</sub>)"),
      `t_stat(beta)`               = nowrap("t(&gamma;&#770;<sub>1</sub>)"),
      `t_stat(beta2)`              = nowrap("t(&gamma;&#770;<sub>2</sub>)"),
      `t_stat(sd_resid)`           = nowrap("t(&gamma;&#770;<sub>3</sub>)"),
      `t_gamma_rf((Intercept))`    = nowrap("t(&gamma;&#770;<sub>0</sub> &minus; R<sub>f</sub>)"),
      r2                           = nowrap("R<sup>2</sup>"),
      sd_r2                        = nowrap("s(R<sup>2</sup>)")
    ) %>%
    # base numeric formatting
    {
      num_cols <- names(df_show)[vapply(df_show, is.numeric, logical(1))]
      gt::fmt_number(., columns = dplyr::all_of(num_cols),
                     decimals = 4, drop_trailing_zeros = FALSE)
    } %>%
    # 3-decimal overrides
    gt::fmt_number(
      columns = dplyr::all_of(c(
        "sd_gamma((Intercept))",
        "sd_gamma(beta)",
        "sd_gamma(beta2)",
        "sd_gamma(sd_resid)"
      )),
      decimals = 3, drop_trailing_zeros = FALSE
    ) %>%
    # 2-decimal overrides
    gt::fmt_number(
      columns = dplyr::all_of(c(
        "acf1_gamma_rf((Intercept))",
        "acf1(beta)",
        "acf1(beta2)",
        "acf1(sd_resid)",
        "t_stat((Intercept))",
        "t_stat(beta)",
        "t_stat(beta2)",
        "t_stat(sd_resid)",
        "t_gamma_rf((Intercept))",
        "r2",
        "sd_r2"
      )),
      decimals = 2, drop_trailing_zeros = FALSE
    ) %>%
    gt::cols_align(align = "center", columns = gt::everything()) %>%
    replace_missing() %>%
    # >>> spacing additions (applies in every panel) <<<
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "white", weight = gt::px(12)),
      locations = gt::cells_body(
        rows = period_txt %in% c("1935-45", "1935-40"),
        columns = gt::everything()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "white", weight = gt::px(12)),
      locations = gt::cells_stub(rows = period_txt %in% c("1935-45", "1935-40"))
    ) %>%
    # ---------------------------------------------------
  gt::tab_options(
    table.font.names = c("Times New Roman", "Times", "serif"),
    table.width = gt::px(1600),
    heading.align = "center",
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    # keep only outer/title borders; remove internal grid lines
    table.border.top.width = gt::px(1),
    table.border.bottom.width = gt::px(1),
    heading.border.bottom.width = gt::px(1),
    column_labels.border.top.width = gt::px(0),
    column_labels.border.bottom.width = gt::px(0),
    table_body.hlines.width = gt::px(0),
    table_body.vlines.width = gt::px(0),
    row_group.border.top.width = gt::px(0),
    row_group.border.bottom.width = gt::px(0),
    stub.border.width = gt::px(0),
    data_row.padding = gt::px(2)
  ) %>%
    gt::tab_stubhead(label = nowrap("PERIOD")) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_stubhead()
    )
  
  if (!is.null(save_path)) gt::gtsave(gt_tbl, filename = save_path)
  gt_tbl
}

# Usage:
tbl3 <- make_table3(fmb_results_df)
tbl3


# =========================================================
# Fama–MacBeth (1973) Table 3 — ordered, robust gt renderer
# Input : fmb_results_df (structure per your prompt)
# Output: gt table; optional export if save_path is set
# =========================================================
make_table3 <- function(fmb_results_df, save_path = NULL) {
  
  # --- deps (quietly load without attaching) -------------------------------
  suppressPackageStartupMessages({
    requireNamespace("dplyr",    quietly = TRUE)
    requireNamespace("gt",       quietly = TRUE)
    requireNamespace("stringr",  quietly = TRUE)
  })
  `%>%` <- dplyr::`%>%`
  
  # --- canonical panel & period orders (Table 3 layout) --------------------
  panel_levels  <- c("Panel A", "Panel B", "Panel C", "Panel D")
  panel_levels_with_colon <- paste0(panel_levels, ":")
  
  period_levels <- c(
    "1935-1968",
    "1935-1945", "1946-1955", "1956-1968",
    "1935-1940", "1941-1945", "1946-1950",
    "1951-1955", "1956-1960", "1961-1968"
  )
  
  # --- columns to display in the Table 3 order -----------------------------
  cols_wanted <- c(
    "mean_gamma((Intercept))",
    "mean_gamma(beta)",
    "mean_gamma(beta2)",
    "mean_gamma(sd_resid)",
    "mean_gamma_rf((Intercept))",
    "sd_gamma((Intercept))",
    "sd_gamma(beta)",
    "sd_gamma(beta2)",
    "sd_gamma(sd_resid)",
    "acf1_gamma_rf((Intercept))",
    "acf1(beta)",
    "acf1(beta2)",
    "acf1(sd_resid)",
    "t_stat((Intercept))",
    "t_stat(beta)",
    "t_stat(beta2)",
    "t_stat(sd_resid)",
    "t_gamma_rf((Intercept))",
    "r2",
    "sd_r2"
  )
  
  # helper for no-wrap HTML labels
  nowrap <- function(x) gt::html(paste0("<span style='white-space:nowrap'>", x, "</span>"))
  
  # --- map Model -> Panel, enforce orders; create display period labels ----
  df <- fmb_results_df %>%
    dplyr::mutate(
      panel_raw = stringr::str_replace(.data$model, "^Model\\s+", "Panel "),
      period_txt_orig = as.character(.data$period),
      period_txt = dplyr::recode(
        period_txt_orig,
        "1935-1968" = "1935-6/68 . .",
        "1935-1945" = "1935-45 . . . .",
        "1946-1955" = "1946-55 . . . .",
        "1956-1968" = "1956-6/68 . .",
        "1935-1940" = "1935-40 . . . .",
        "1941-1945" = "1941-45 . . . .",
        "1946-1950" = "1946-50 . . . .",
        "1951-1955" = "1951-55 . . . .",
        "1956-1960" = "1956-60 . . . .",
        "1961-1968" = "1961-6/68 . .",
        .default = period_txt_orig
      ),
      panel  = factor(panel_raw, levels = panel_levels),
      period = factor(period_txt_orig, levels = period_levels, ordered = TRUE)
    )
  
  # ensure fixed shape
  missing_cols <- setdiff(cols_wanted, names(df))
  if (length(missing_cols)) df[missing_cols] <- NA_real_
  
  # order and keep columns
  df_show <- df %>%
    dplyr::arrange(.data$panel, .data$period) %>%
    dplyr::select(panel, period_txt, dplyr::all_of(cols_wanted))
  
  # missing-values helper
  replace_missing <- function(gt_tbl) {
    if ("sub_missing" %in% getNamespaceExports("gt")) {
      gt::sub_missing(gt_tbl, columns = gt::everything(), missing_text = ". . .")
    } else {
      gt::fmt_missing(gt_tbl, columns = gt::everything(), missing_text = ". . .")
    }
  }
  
  # build gt table
  gt_tbl <- df_show %>% gt::gt(rowname_col = "period_txt")
  
  # row groups A -> D with colons
  if ("row_group_order" %in% getNamespaceExports("gt")) {
    for (p in panel_levels) {
      idx <- which(df_show$panel == p)
      if (length(idx)) {
        gt_tbl <- gt::tab_row_group(gt_tbl, label = paste0(p, ":"), rows = idx)
      }
    }
    gt_tbl <- gt::row_group_order(gt_tbl, groups = panel_levels_with_colon)
  } else {
    for (p in rev(panel_levels)) {
      idx <- which(df_show$panel == p)
      if (length(idx)) {
        gt_tbl <- gt::tab_row_group(gt_tbl, label = paste0(p, ":"), rows = idx)
      }
    }
  }
  
  gt_tbl <- gt_tbl %>%
    gt::cols_hide(columns = "panel") %>%
    gt::tab_header(
      title = gt::md("**TABLE 3**"),
      subtitle = gt::html(
        "<b>SUMMARY RESULTS FOR THE REGRESSION</b><br/>
         <span style='font-family:Times'>
         R<sub>p</sub> = &gamma;&#770;<sub>0t</sub> + &gamma;&#770;<sub>1t</sub>&beta;<sub>p</sub> +
         &gamma;&#770;<sub>2t</sub>&beta;<sub>p</sub><sup>2</sup> +
         &gamma;&#770;<sub>3t</sub>&#772;s<sub>p</sub>(&#949;&#770;) + &eta;&#770;<sub>pt</sub>
         </span>"
      )
    ) %>%
    gt::tab_spanner(label = nowrap("STATISTIC"),
                    columns = dplyr::all_of(cols_wanted)) %>%
    gt::cols_label(
      `mean_gamma((Intercept))`    = nowrap("&gamma;&#770;<sub>0</sub>"),
      `mean_gamma(beta)`           = nowrap("&gamma;&#770;<sub>1</sub>"),
      `mean_gamma(beta2)`          = nowrap("&gamma;&#770;<sub>2</sub>"),
      `mean_gamma(sd_resid)`       = nowrap("&gamma;&#770;<sub>3</sub>"),
      `mean_gamma_rf((Intercept))` = nowrap("&gamma;&#770;<sub>0</sub> &minus; R<sub>f</sub>"),
      `sd_gamma((Intercept))`      = nowrap("s(&gamma;&#770;<sub>0</sub>)"),
      `sd_gamma(beta)`             = nowrap("s(&gamma;&#770;<sub>1</sub>)"),
      `sd_gamma(beta2)`            = nowrap("s(&gamma;&#770;<sub>2</sub>)"),
      `sd_gamma(sd_resid)`         = nowrap("s(&gamma;&#770;<sub>3</sub>)"),
      `acf1_gamma_rf((Intercept))` = nowrap("&rho;<sub>0</sub>(&gamma;&#770;<sub>0</sub> &minus; R<sub>f</sub>)"),
      `acf1(beta)`                 = nowrap("&rho;(&gamma;&#770;<sub>1</sub>)"),
      `acf1(beta2)`                = nowrap("&rho;(&gamma;&#770;<sub>2</sub>)"),
      `acf1(sd_resid)`             = nowrap("&rho;(&gamma;&#770;<sub>3</sub>)"),
      `t_stat((Intercept))`        = nowrap("t(&gamma;&#770;<sub>0</sub>)"),
      `t_stat(beta)`               = nowrap("t(&gamma;&#770;<sub>1</sub>)"),
      `t_stat(beta2)`              = nowrap("t(&gamma;&#770;<sub>2</sub>)"),
      `t_stat(sd_resid)`           = nowrap("t(&gamma;&#770;<sub>3</sub>)"),
      `t_gamma_rf((Intercept))`    = nowrap("t(&gamma;&#770;<sub>0</sub> &minus; R<sub>f</sub>)"),
      r2                           = nowrap("R<sup>2</sup>"),
      sd_r2                        = nowrap("s(R<sup>2</sup>)")
    ) %>%
    # base numeric formatting
    {
      num_cols <- names(df_show)[vapply(df_show, is.numeric, logical(1))]
      gt::fmt_number(., columns = dplyr::all_of(num_cols),
                     decimals = 4, drop_trailing_zeros = FALSE)
    } %>%
    # 3-decimal overrides
    gt::fmt_number(
      columns = dplyr::all_of(c(
        "sd_gamma((Intercept))",
        "sd_gamma(beta)",
        "sd_gamma(beta2)",
        "sd_gamma(sd_resid)"
      )),
      decimals = 3, drop_trailing_zeros = FALSE
    ) %>%
    # 2-decimal overrides
    gt::fmt_number(
      columns = dplyr::all_of(c(
        "acf1_gamma_rf((Intercept))",
        "acf1(beta)",
        "acf1(beta2)",
        "acf1(sd_resid)",
        "t_stat((Intercept))",
        "t_stat(beta)",
        "t_stat(beta2)",
        "t_stat(sd_resid)",
        "t_gamma_rf((Intercept))",
        "r2",
        "sd_r2"
      )),
      decimals = 2, drop_trailing_zeros = FALSE
    ) %>%
    gt::cols_align(align = "center", columns = gt::everything()) %>%
    replace_missing() %>%
    # >>> spacing additions (prefix-match to handle dotted labels) <<<
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "white", weight = gt::px(12)),
      locations = gt::cells_body(
        rows = grepl("^1935-45", period_txt) | grepl("^1935-40", period_txt),
        columns = gt::everything()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "white", weight = gt::px(12)),
      locations = gt::cells_stub(
        rows = grepl("^1935-45", period_txt) | grepl("^1935-40", period_txt)
      )
    ) %>%
    # ---------------------------------------------------
  gt::tab_options(
    table.font.names = c("Times New Roman", "Times", "serif"),
    table.width = gt::px(1600),
    heading.align = "center",
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    # keep only outer/title borders; remove internal grid lines
    table.border.top.width = gt::px(1),
    table.border.bottom.width = gt::px(1),
    heading.border.bottom.width = gt::px(1),
    column_labels.border.top.width = gt::px(0),
    column_labels.border.bottom.width = gt::px(0),
    table_body.hlines.width = gt::px(0),
    table_body.vlines.width = gt::px(0),
    row_group.border.top.width = gt::px(0),
    row_group.border.bottom.width = gt::px(0),
    stub.border.width = gt::px(0),
    data_row.padding = gt::px(2)
  ) %>%
    gt::tab_stubhead(label = nowrap("PERIOD")) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_stubhead()
    )
  
  if (!is.null(save_path)) gt::gtsave(gt_tbl, filename = save_path)
  gt_tbl
}

# Usage:
tbl3 <- make_table3(fmb_results_df)
tbl3








