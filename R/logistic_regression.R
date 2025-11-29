#' Logistic Regression Summary Table
#'
#' Performs logistic regression for a binary outcome and a set of predictor variables.
#' Computes both univariate and multivariate odds ratios (ORs) with 95% confidence intervals
#' and p-values. Categorical variables automatically include a reference level in the output.
#' Results are returned as a formatted flextable.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome A character string (factor)specifying the binary outcome variable.
#' @param predictors A character vector (factor) of predictor variables to include in the regression.
#'
#' @return A flextable displaying univariate and multivariate odds ratios (95% CI)
#' and p-values for each predictor. Reference levels for categorical variables are labeled "Reference".
#' @importFrom stats glm confint
#' @importFrom broom tidy
#' @import MASS
#' @import dplyr
#' @import flextable
#' @export logreg
#' @examples logreg(data=medical_data(), outcome="case" ,
#'    predictors= c("age" ,  "parity" ,    "induced" ))
logreg <- function(data, outcome, predictors) {

  # Convert character predictors to factor
  for (var in predictors) {
    if (is.character(data[[var]])) data[[var]] <- as.factor(data[[var]])
  }

  get_logistic <- function(formula) {
    glm(formula, data = data, family = binomial) %>%
      broom::tidy(conf.int = TRUE, exponentiate = TRUE)
  }

  add_reference <- function(var, fit_tidy) {
    if (is.factor(data[[var]])) {
      ref_level <- levels(data[[var]])[1]
      ref_row <- tibble(
        term = paste0(var, "_", ref_level, " (ref)"),
        estimate = NA,
        conf.low = NA,
        conf.high = NA,
        p.value = NA
      )
      fit_tidy <- fit_tidy %>%
        mutate(term = paste0(var, "_", sub(paste0("^", var), "", term)))
      fit_tidy <- bind_rows(ref_row, fit_tidy)
    } else {
      # Continuous variable: term = predictor name only
      fit_tidy <- fit_tidy %>%
        mutate(term = var)
    }
    fit_tidy
  }

  # Univariate
  uni_list <- lapply(predictors, function(var) {
    fit <- get_logistic(as.formula(paste(outcome, "~", var))) %>%
      filter(term != "(Intercept)")
    add_reference(var, fit)
  })

  uni_results <- bind_rows(uni_list) %>%
    mutate(
      `Univariate OR (95% CI)` = ifelse(is.na(estimate), "Reference",
                                        paste0(round(estimate,2), " (", round(conf.low,2), "-", round(conf.high,2), ")")),
      `P-value (Univariate)` = ifelse(is.na(p.value), NA,
                                      ifelse(p.value < 0.001, "<0.001", round(p.value,3)))
    ) %>%
    select(term, `Univariate OR (95% CI)`, `P-value (Univariate)`)

  # Multivariate
  formula_multi <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  multi_fit <- get_logistic(formula_multi) %>% filter(term != "(Intercept)")

  multi_list <- lapply(predictors, function(var) {
    fit <- multi_fit %>% filter(grepl(var, term))
    add_reference(var, fit)
  })

  multi_results <- bind_rows(multi_list) %>%
    mutate(
      `Multivariate OR (95% CI)` = ifelse(is.na(estimate), "Reference",
                                          paste0(round(estimate,2), " (", round(conf.low,2), "-", round(conf.high,2), ")")),
      `P-value (Multivariate)` = ifelse(is.na(p.value), NA,
                                        ifelse(p.value < 0.001, "<0.001", round(p.value,3)))
    ) %>%
    select(term, `Multivariate OR (95% CI)`, `P-value (Multivariate)`)

  # Combine
  combined <- full_join(uni_results, multi_results, by = "term") %>%
    rename(Predictor = term)

  # Output flextable with bold headers
  flextable(combined) %>%
    set_header_labels(
      Predictor = "Predictor",
      `Univariate OR (95% CI)` = "Univariate OR (95% CI)",
      `P-value (Univariate)` = "P-value (Univariate)",
      `Multivariate OR (95% CI)` = "Multivariate OR (95% CI)",
      `P-value (Multivariate)` = "P-value (Multivariate)"
    ) %>%
    bold(part = "header") %>%
    autofit()
}
