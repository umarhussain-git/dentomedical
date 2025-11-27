#' Create a Summary Table With P-Values for Group Comparisons
#'
#' `sum.stat.p()` generates a descriptive summary table for both categorical and
#' continuous variables stratified by a grouping variable. It automatically
#' computes appropriate statistical tests (Chi-square, Fisher's exact, t-test,
#' Wilcoxon, ANOVA, or Kruskal–Wallis) based on data type and distribution
#' characteristics. The output is formatted as a `flextable` with footnotes
#' indicating the summary statistics used and the tests applied.
#'
#' @param data A data frame or tibble containing variables to summarise.
#' @param by A string specifying the grouping variable name. Must be a column in `data`.
#' @param statistic A string specifying summary style for continuous variables:
#'   - `"mean_sd"`: Mean (SD)
#'   - `"med_iqr"`: Median (IQR)
#' @param test_type Optionally force a specific test. Choices:
#'   - `"auto"` *(default)* — automatically selects appropriate tests
#'   - `"chisq"`, `"fisher"` for categorical variables
#'   - `"t.test"`, `"wilcox"` for 2-group continuous comparisons
#'   - `"anova"`, `"kruskal"` for >2-group continuous comparisons
#'
#' @return A `flextable` object containing the summary table with p-values and
#' footer notes describing summary statistics and tests used.
#'
#' @export sum.stat.p
#'
#' @examples
#' # Load built-in dataset
#' data(CO2)
#'
#' # Example 1: Auto test selection, median/IQR summary
#' sum.stat.p(CO2, by = "Type", statistic = "med_iqr")
#'
#' # Example 2: Force Wilcoxon test for continuous variables
#' sum.stat.p(CO2, by = "Type", statistic = "med_iqr", test_type = "wilcox")
#'
#' # Example 3: Mean/SD with automatic test choice
#' sum.stat.p(CO2, by = "Treatment", statistic = "mean_sd")
sum.stat.p <- function(data, by, statistic = "mean_sd", test_type = "auto") {
  data <- as_tibble(data)
  is_cat <- function(x) is.factor(x) || is.character(x)

  summary_list <- list()
  used_tests <- c()

  for (colname in names(data)) {
    if (colname == by) next
    var <- data[[colname]]
    pval <- NA
    test_used <- ""

    # ----- P-VALUE SECTION -----
    try({
      if (is_cat(var)) {
        tbl <- table(data[[colname]], data[[by]])
        if (test_type == "fisher" || (test_type == "auto" && any(tbl < 5))) {
          pval <- fisher.test(tbl)$p.value
          test_used <- "Fisher's Exact"
        } else {
          pval <- chisq.test(tbl)$p.value
          test_used <- "Chi-square"
        }
      } else {
        g <- unique(data[[by]])
        if (length(g) == 2) {
          if (test_type == "wilcox") {
            pval <- wilcox.test(var ~ data[[by]])$p.value
            test_used <- "Wilcoxon Rank-Sum"
          } else if (test_type == "t.test" || test_type == "auto") {
            pval <- t.test(var ~ data[[by]])$p.value
            test_used <- "Student's t-test"
          }
        } else {
          if (test_type == "kruskal" || test_type == "auto") {
            pval <- kruskal.test(var ~ data[[by]])$p.value
            test_used <- "Kruskal-Wallis"
          } else if (test_type == "anova") {
            pval <- summary(aov(var ~ data[[by]]))[[1]][["Pr(>F)"]][1]
            test_used <- "ANOVA"
          }
        }
      }
    }, silent = TRUE)

    used_tests <- c(used_tests, test_used)

    # ----- SUMMARY SECTION -----
    if (is_cat(var)) {
      tbl <- data %>%
        group_by(!!sym(colname), !!sym(by)) %>%
        summarise(n = n(), .groups = "drop") %>%
        complete(!!sym(colname), !!sym(by), fill = list(n = 0)) %>%
        group_by(!!sym(by)) %>%
        mutate(pct = round(n / sum(n) * 100)) %>%
        ungroup() %>%
        mutate(
          Variable = colname,
          Characteristic = as.character(!!sym(colname)),
          label = paste0(n, " (", pct, "%)")
        ) %>%
        select(Variable, Characteristic, !!sym(by), label) %>%
        pivot_wider(names_from = !!sym(by), values_from = label) %>%
        mutate(`p-value` = if_else(row_number() == 1, format.pval(pval, digits = 3, eps = 0.001), ""))

    } else {
      tbl <- data %>%
        group_by(!!sym(by)) %>%
        summarise(
          val = case_when(
            statistic == "mean_sd" ~ paste0(
              round(mean(!!sym(colname), na.rm = TRUE), 2),
              " (",
              round(sd(!!sym(colname), na.rm = TRUE), 2),
              ")"
            ),
            TRUE ~ paste0(
              round(median(!!sym(colname), na.rm = TRUE), 2),
              " (",
              round(quantile(!!sym(colname), 0.25, na.rm = TRUE), 2),
              ", ",
              round(quantile(!!sym(colname), 0.75, na.rm = TRUE), 2),
              ")"
            )
          ),
          .groups = "drop"
        ) %>%
        mutate(Variable = colname) %>%
        pivot_wider(names_from = !!sym(by), values_from = val) %>%
        mutate(
          Characteristic = if_else(statistic == "mean_sd", "Mean (SD)", "Median (IQR)"),
          `p-value` = format.pval(pval, digits = 3, eps = 0.001)
        ) %>%
        select(Variable, Characteristic, everything())
    }

    summary_list[[colname]] <- tbl
  }

  summary_df <- bind_rows(summary_list) %>%
    group_by(Variable) %>%
    mutate(Variable = if_else(row_number() == 1, Variable, "")) %>%
    ungroup()

  # --- Build Footer ---
  stat_text <- case_when(
    statistic == "mean_sd" ~ "1 n (%); Mean (SD)",
    statistic == "med_iqr" ~ "1 n (%); Median (IQR)",
    TRUE ~ "1 n (%)"
  )

  test_text <- paste("P-values calculated using:", paste(unique(used_tests[used_tests != ""]), collapse = ", "))

  flextable(summary_df) %>%
    set_header_labels(
      Variable = "Variable",
      Characteristic = "Characteristic",
      `p-value` = "p-value"
    ) %>%
    autofit() %>%
    bold(part = "header") %>%
    add_footer_lines(stat_text) %>%
    add_footer_lines(test_text)
}


# # Auto test selection with footnotes showing test
# sum_stat_p(CO2, by = "Type", statistic = "med_iqr")
#
# # Force Fisher and Wilcoxon
# sum_stat_p(CO2, by = "Type", statistic = "med_iqr", test_type = "wilcox")


