#' Diagnostic Accuracy Metrics with Optional 2x2 Table
#'
#' Calculates diagnostic accuracy measures (Sensitivity, Specificity, PPV, NPV, Accuracy, LR+, LR-, DOR)
#' from a binary test and gold standard. Provides 95% confidence intervals using Wilson method
#' for proportions and log method for ratios. Optionally, prints a descriptive 2x2 table.
#'
#' @param data A data frame containing the test results and gold standard.
#' @param test_col Character. Name of the column in `data` with test results ("positive"/"negative").
#' @param gold_col Character. Name of the column in `data` with gold standard results ("positive"/"negative").
#' @param descriptive Logical. If TRUE, prints a descriptive 2x2 table with counts (TN, TP, FP, FN). Default is FALSE.
#'
#' @return A `flextable` object summarizing diagnostic metrics with 95% CI.
#' If `descriptive = TRUE`, also prints a 2x2 table of counts.
#'
#' @export
#'
#' @examples
#' diagnostic_data <- data.frame(
#'   test = c("positive","negative","positive","negative","positive","negative","positive","negative"),
#'   goldstandard = c("positive","positive","negative","negative","positive","negative","positive","negative")
#' )
#' diag_accuracy(diagnostic_data, test_col = "test", gold_col = "goldstandard", descriptive = TRUE)
diag_accuracy <- function(data, test_col, gold_col, descriptive = FALSE) {
  # Extract columns
  test <- factor(data[[test_col]], levels = c("negative", "positive"))
  gold <- factor(data[[gold_col]], levels = c("negative", "positive"))

  # Confusion matrix
  tab <- table(test, gold)
  TP <- tab["positive","positive"]
  FP <- tab["positive","negative"]
  TN <- tab["negative","negative"]
  FN <- tab["negative","positive"]

  # Optional descriptive 2x2 table
  if(descriptive){
    desc_df <- data.frame(
      Metric = c("True Positive", "False Positive", "False Negative", "True Negative"),
      Count  = c(TP, FP, FN, TN),
      check.names = FALSE
    )
    desc_ft <- flextable(desc_df) %>%
      set_header_labels(Metric = "2x2 Metric", Count = "Count") %>%
      add_header_lines(values = "Descriptive Counts") %>%
      align(align = "center", part = "all") %>%
      autofit()
    print(desc_ft)
  }

  # Wilson CI for proportions
  wilson_ci <- function(x,n){
    p <- x/n
    z <- 1.96
    denom <- 1 + z^2/n
    centre <- p + z^2/(2*n)
    margin <- z * sqrt(p*(1-p)/n + z^2/(4*n^2))
    c(lower = (centre - margin)/denom, upper = (centre + margin)/denom)
  }

  # Metrics
  Sens <- TP/(TP+FN)
  Spec <- TN/(TN+FP)
  PPV  <- TP/(TP+FP)
  NPV  <- TN/(TN+FN)
  Acc  <- (TP+TN)/(TP+TN+FP+FN)

  # Likelihood ratios & DOR
  LR_pos <- Sens/(1-Spec)
  LR_neg <- (1-Sens)/Spec
  DOR    <- LR_pos/LR_neg

  # Log CIs for ratios
  log_ci <- function(est,se) {
    c(lower = exp(log(est) - 1.96*se), upper = exp(log(est) + 1.96*se))
  }

  se_LR_pos <- sqrt((1/TP - 1/(TP+FN)) + (1/FP - 1/(FP+TN)))
  se_LR_neg <- sqrt((1/FN - 1/(TP+FN)) + (1/TN - 1/(FP+TN)))
  LR_pos_CI <- log_ci(LR_pos, se_LR_pos)
  LR_neg_CI <- log_ci(LR_neg, se_LR_neg)
  DOR_CI    <- log_ci(DOR, sqrt(se_LR_pos^2 + se_LR_neg^2))

  # Combine metrics into a table
  metrics <- list(
    "Sensitivity (%)" = c(Sens*100, wilson_ci(TP,TP+FN)*100),
    "Specificity (%)" = c(Spec*100, wilson_ci(TN,TN+FP)*100),
    "PPV (%)"         = c(PPV*100, wilson_ci(TP,TP+FP)*100),
    "NPV (%)"         = c(NPV*100, wilson_ci(TN,TN+FN)*100),
    "Accuracy (%)"    = c(Acc*100, wilson_ci(TP+TN, TP+TN+FP+FN)*100),
    "LR+"             = c(LR_pos, LR_pos_CI),
    "LR-"             = c(LR_neg, LR_neg_CI),
    "DOR"             = c(DOR, DOR_CI)
  )

  df <- do.call(rbind, lapply(names(metrics), function(m){
    est <- round(metrics[[m]][1],2)
    lower <- round(metrics[[m]][2],2)
    upper <- round(metrics[[m]][3],2)
    data.frame(Metric = m,
               `Estimate (95% CI)` = paste0(est," (", lower,"-",upper,")"),
               check.names = FALSE)
  }))
  rownames(df) <- NULL

  # Flextable with proper header and footer
  ft <- flextable(df) %>%
    set_header_labels(Metric = "Diagnostic Metric",
                      `Estimate (95% CI)` = "Estimate (95% CI)") %>%
    add_footer_lines(values = "CI formula references: Wilson (1927) for proportions; log method for LR+/- and DOR (Altman, 1991)") %>%
    align(align = "center", part = "all") %>%
    autofit()

  ft
}


