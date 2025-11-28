#' Linear Regression Summary Table
#'
#' This function performs univariate and multivariate linear regression analyses
#' for the specified predictors and outcome variable, returning a summary table
#' with characteristics, regression coefficients (β) with 95% CI, and p-values.
#' Numeric variables show mean (SD); categorical variables show n (%).
#' Multivariate model R² and adjusted R² are included in the table footer.
#'
#' @param data A data frame or tibble containing the variables.
#' @param outcome The name of the outcome variable (numeric) as a string.
#' @param predictors A character vector of predictor variable names.
#'
#' @return A `flextable` object summarizing univariate and multivariate linear regression results.
#' @export linreg
#'
#' @import broom
#' @import flextable
#' @import tidyr
#' @import dplyr
#'
#' @examples
#' # Example using built-in iris dataset
#' linreg(iris, outcome = "Sepal.Length",
#'        predictors = c("Sepal.Width", "Petal.Length", "Species"))
linreg <- function(data, outcome, predictors) {
  suppressPackageStartupMessages({
      })
  data <- as_tibble(data)

  #-------------------- Helper Functions --------------------#
  summary_numeric <- function(var) {
    tibble(
      Predictor = var,
      Level = NA,
      Characteristics = paste0(round(mean(data[[var]], na.rm = TRUE), 2),
                               " (", round(sd(data[[var]], na.rm = TRUE), 2), ")")
    )
  }

  summary_categorical <- function(var) {
    tbl <- table(data[[var]])
    pct <- prop.table(tbl) * 100
    levs <- names(tbl)
    tibble(
      Predictor = var,
      Level = levs,
      Characteristics = paste0(tbl, " (", round(pct, 1), "%)",
                               ifelse(seq_along(tbl) == 1, " [ref]", ""))
    )
  }

  #-------------------- Univariate --------------------#
  uni_list <- list()
  for (var in predictors) {
    model_uni <- lm(as.formula(paste(outcome, "~", var)), data = data)
    t <- tidy(model_uni, conf.int = TRUE)

    if (is.numeric(data[[var]])) {
      uni_list[[var]] <- tibble(
        Predictor = var,
        Level = NA,
        `Univariate β (95% CI)` = paste0(round(t$estimate[-1],2),
                                         " (", round(t$conf.low[-1],2),
                                         ", ", round(t$conf.high[-1],2), ")"),
        `Univariate p` = format.pval(t$p.value[-1], digits=3, eps=0.001)
      )
    } else {
      levs <- t$term[-1]
      uni_list[[var]] <- tibble(
        Predictor = var,
        Level = gsub(var, "", levs),
        `Univariate β (95% CI)` = paste0(round(t$estimate[-1],2),
                                         " (", round(t$conf.low[-1],2),
                                         ", ", round(t$conf.high[-1],2), ")"),
        `Univariate p` = format.pval(t$p.value[-1], digits=3, eps=0.001)
      )
    }
  }
  uni_results <- bind_rows(uni_list)

  #-------------------- Multivariate --------------------#
  model_multi <- lm(as.formula(paste(outcome, "~", paste(predictors, collapse=" + "))),
                    data = data)
  t_multi <- tidy(model_multi, conf.int = TRUE)

  multi_list <- list()
  for (var in predictors) {
    if (is.numeric(data[[var]])) {
      row <- t_multi %>% filter(term == var)
      multi_list[[var]] <- tibble(
        Predictor = var,
        Level = NA,
        `Multivariate β (95% CI)` = paste0(round(row$estimate,2),
                                           " (", round(row$conf.low,2),
                                           ", ", round(row$conf.high,2), ")"),
        `Multivariate p` = format.pval(row$p.value, digits=3, eps=0.001)
      )
    } else {
      rows <- t_multi %>% filter(grepl(var, term))
      levels <- gsub(var, "", rows$term)
      multi_list[[var]] <- tibble(
        Predictor = var,
        Level = levels,
        `Multivariate β (95% CI)` = paste0(round(rows$estimate,2),
                                           " (", round(rows$conf.low,2),
                                           ", ", round(rows$conf.high,2), ")"),
        `Multivariate p` = format.pval(rows$p.value, digits=3, eps=0.001)
      )
    }
  }
  multi_results <- bind_rows(multi_list)

  #-------------------- Summary Table --------------------#
  summary_list <- list()
  for (var in predictors) {
    if (is.numeric(data[[var]])) {
      summary_list[[var]] <- summary_numeric(var)
    } else {
      summary_list[[var]] <- summary_categorical(var)
    }
  }
  summary_tbl <- bind_rows(summary_list)

  # Merge results
  final_tbl <- summary_tbl %>%
    left_join(uni_results, by=c("Predictor","Level")) %>%
    left_join(multi_results, by=c("Predictor","Level")) %>%
    select(Predictor, Level, Characteristics,
           `Univariate β (95% CI)`,`Univariate p`,
           `Multivariate β (95% CI)`,`Multivariate p`)

  #-------------------- Remove repeated Predictor names --------------------#
  final_tbl <- final_tbl %>%
    group_by(Predictor) %>%
    mutate(Predictor = ifelse(row_number() == 1, Predictor, "")) %>%
    ungroup()

  # R^2 footer with superscript + characteristics footnote
  footer_txt <- paste0("Characteristics: Mean (SD) for numeric; n (%) for categorical. ",
                       "Multivariate model: R", "\U00B2", " = ",
                       round(summary(model_multi)$r.squared,3),
                       "; Adjusted R", "\U00B2", " = ",
                       round(summary(model_multi)$adj.r.squared,3))

  flextable(final_tbl) %>%
    autofit() %>%
    bold(part="header") %>%
    add_footer_lines(footer_txt)
}
