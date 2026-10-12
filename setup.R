required_packages <- c(
  "Formula", "Hmisc", "MASS", "broom", "corrplot", "correlation",
  "dplyr", "ggplot2", "glmnet", "gridExtra", "kableExtra", "lattice",
  "mgcv", "pROC", "reshape2", "rms", "survival", "tableone"
)

missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
