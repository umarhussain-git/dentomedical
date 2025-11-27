#' Normality Test Summary Table for Numeric Variables
#'
#' This function performs the Shapiro-Wilk normality test on all numeric variables
#' in a dataset and returns the results in a publication-ready `flextable`.
#' Extremely small p-values are displayed as "p < 0.001". The function
#' automatically detects numeric variables and ignores non-numeric columns.
#'
#' @param data A data frame containing numeric and non-numeric variables.
#'   Only numeric variables are assessed for normality.
#' @param sample_size Integer. Maximum number of observations to use for the
#'   Shapiro-Wilk test per variable (default = 5000).
#'
#' @return A `flextable` summarizing each numeric variable with Shapiro-Wilk
#'   W statistic, formatted p-value, and distribution classification
#'   ("Normal" or "Skewed").
#'
#' @import dplyr flextable tidyr
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' norm.sum(iris)
norm.sum <- function(data, sample_size = 5000) {

  # Select only numeric variables
  num_vars <- data[sapply(data, is.numeric)]
  results <- list()

  for (var in names(num_vars)) {
    x <- num_vars[[var]]

    # Cap sample size for Shapiro-Wilk
    if (length(x) > sample_size) {
      x <- sample(x, sample_size)
    }

    # Run Shapiro-Wilk test
    test <- shapiro.test(x)
    dist_type <- ifelse(test$p.value > 0.05, "Normal", "Skewed")

    # Format p-value as character (CRAN safe)
    p_label <- ifelse(
      test$p.value < 0.001,
      "< 0.001",
      format(round(test$p.value, 4), nsmall = 4)
    )

    # Store row (all character or numeric-safe)
    results[[var]] <- data.frame(
      Variable = var,
      W = round(as.numeric(test$statistic), 3),
      `p-value` = p_label,
      Distribution = dist_type,
      stringsAsFactors = FALSE
    )
  }

  # Bind all rows safely
  results_df <- bind_rows(results)

  # Create flextable output
  flextable(results_df) %>%
    set_header_labels(
      Variable = "Variable",
      W = "W Statistic",
      `p-value` = "p-value",
      Distribution = "Distribution"
    ) %>%
    autofit() %>%
    bold(part = "header")
}
