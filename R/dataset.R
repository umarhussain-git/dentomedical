#' Load Infertility Dataset
#'
#' @return A data.frame containing infertility cases with labeled predictors suitable for logistic regression
#' @export
medical_data <- function() {
  data("infert", package = "datasets")

  # Convert categorical predictors to factors with labels
  infert$case <- factor(infert$case, levels = c(0, 1), labels = c("Control", "Infertile"))
  infert$induced <- factor(infert$induced, levels = c(0, 1), labels = c("No", "Yes"))
  infert$spontaneous <- factor(infert$spontaneous, levels = c(0, 1), labels = c("No", "Yes"))

  return(infert)
}
