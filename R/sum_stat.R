#' Descriptive Summary Table for Continuous and Categorical Variables
#'
#' This function generates descriptive summary tables for both continuous and categorical variables.
#' Continuous variables can be summarized using mean (SD) or median (IQR), and categorical variables
#' are summarized as counts and percentages. Optionally, summaries can be stratified by a grouping variable.
#'
#' @param data A data frame containing the variables to summarize.
#' @param by Optional. A grouping variable (column name as string) to stratify the summary table.
#' @param statistic Character. Summary statistic for continuous variables. Either `"mean_sd"` (default) or `"med_iqr"`.
#'
#' @return A `flextable` object displaying the summary table with appropriate formatting for publication or reporting.
#'   Continuous variables show mean (SD) or median (IQR), and categorical variables show counts and percentages.
#'   If `by` is specified, summaries are presented for each group in separate columns.
#' @importFrom dplyr bind_rows
#' @import dplyr tidyr flextable
#' @export sum.stat
#'
#' @examples
#' sum.stat(iris)
#' sum.stat(iris, by = "Species", statistic = "mean_sd")
#' sum.stat(iris, statistic = "med_iqr")
sum.stat <- function(data, by = NULL, statistic = "mean_sd") {

  data <- tibble::as_tibble(data)

  is_cat <- function(x) is.factor(x) || is.character(x)
  summary_list <- list()

  for (colname in names(data)) {

    if (!is.null(by) && colname == by) next

    var <- data[[colname]]

    if (is_cat(var)) {

      if (!is.null(by)) {

        tbl <- data %>%
          dplyr::group_by(.data[[colname]], .data[[by]]) %>%
          dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
          tidyr::complete(.data[[colname]], .data[[by]], fill = list(n = 0)) %>%
          dplyr::group_by(.data[[by]]) %>%
          dplyr::mutate(pct = round(n / sum(n) * 100, 2)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            Variable = colname,
            Characteristic = as.character(.data[[colname]]),
            Value = paste0(n, " (", pct, "%)")
          ) %>%
          dplyr::select(Variable, Characteristic, .data[[by]], Value) %>%
          tidyr::pivot_wider(
            names_from = .data[[by]],
            values_from = Value,
            values_fill = ""
          )

      } else {

        tbl <- data %>%
          dplyr::count(.data[[colname]]) %>%
          dplyr::mutate(
            pct = round(n / sum(n) * 100, 2),
            Variable = colname,
            Characteristic = as.character(.data[[colname]]),
            Value = paste0(n, " (", pct, "%)")
          ) %>%
          dplyr::select(Variable, Characteristic, Value)
      }

    } else {

      if (!is.null(by)) {

        tbl <- data %>%
          dplyr::group_by(.data[[by]]) %>%
          dplyr::summarise(
            Value = dplyr::case_when(
              statistic == "mean_sd" ~ paste0(
                format(round(mean(.data[[colname]], na.rm = TRUE), 2), nsmall = 2),
                " (",
                format(round(sd(.data[[colname]], na.rm = TRUE), 2), nsmall = 2),
                ")"
              ),
              statistic == "med_iqr" ~ paste0(
                format(round(median(.data[[colname]], na.rm = TRUE), 2), nsmall = 2),
                " (",
                format(round(quantile(.data[[colname]], 0.25, na.rm = TRUE), 2), nsmall = 2),
                ", ",
                format(round(quantile(.data[[colname]], 0.75, na.rm = TRUE), 2), nsmall = 2),
                ")"
              ),
              TRUE ~ NA_character_
            ),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            Variable = colname,
            Characteristic = ifelse(statistic == "mean_sd",
                                    "Mean (SD)", "Median (IQR)")
          ) %>%
          tidyr::pivot_wider(
            names_from = .data[[by]],
            values_from = Value,
            values_fill = ""
          )

      } else {

        tbl <- data %>%
          dplyr::summarise(
            Value = dplyr::case_when(
              statistic == "mean_sd" ~ paste0(
                format(round(mean(.data[[colname]], na.rm = TRUE), 2), nsmall = 2),
                " (",
                format(round(sd(.data[[colname]], na.rm = TRUE), 2), nsmall = 2),
                ")"
              ),
              statistic == "med_iqr" ~ paste0(
                format(round(median(.data[[colname]], na.rm = TRUE), 2), nsmall = 2),
                " (",
                format(round(
                  quantile(.data[[colname]], 0.25, na.rm = TRUE), 2), nsmall = 2),
                ", ",
                format(round(
                  quantile(.data[[colname]], 0.75, na.rm = TRUE), 2), nsmall = 2),
                ")"
              ),
              TRUE ~ NA_character_
            )
          ) %>%
          dplyr::mutate(
            Variable = colname,
            Characteristic = ifelse(statistic == "mean_sd",
                                    "Mean (SD)", "Median (IQR)")
          ) %>%
          dplyr::select(Variable, Characteristic, Value)
      }
    }

    summary_list[[colname]] <- tbl
  }

  summary_df <- dplyr::bind_rows(summary_list) %>%
    dplyr::group_by(Variable) %>%
    dplyr::mutate(Variable = ifelse(dplyr::row_number() == 1, Variable, "")) %>%
    dplyr::ungroup()

  footer_note <- ifelse(statistic == "mean_sd",
                        "Mean (SD) / n(%)",
                        "Median (IQR) / n(%)")

  flextable::flextable(summary_df) %>%
    flextable::set_header_labels(
      Variable = "Variable",
      Characteristic = "Characteristic"
    ) %>%
    flextable::autofit() %>%
    flextable::bold(part = "header") %>%
    flextable::add_footer_lines(footer_note)
}
