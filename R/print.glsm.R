#' Print method for GLSM objects
#'
#' Displays the call, the number of populations in the saturated model, the estimated coefficients with standard errors
#' and odds ratios, and the log-likelihoods for the different models.
#'
#' @param x An object of class `glsm`, typically the result of a call to [glsm()].
#' @param ... Additional arguments (currently unused).
#'
#' @details This function provides a clean summary output when you type the model object in the console.
#'
#' @return Prints formatted model information to the console. Invisibly returns `NULL`.
#'
#' @seealso [glsm()], [summary.glsm()], [plot.glsm()]
#'
#' @export
print.glsm <- function(x, ...) {
  Coef <- as.vector(t(x$coef))
  StdErr <- as.vector(t(x$Std.Error))
  ExpB <- as.vector(t(x$ExpB))

  vars <- rownames(x$coef)
  cats <- colnames(x$coef)

  VarCat <- paste0(
    rep(vars, each = length(cats)),
    ":",
    rep(cats, times = length(vars))
  )

  TB <- data.frame(
    Coef,
    StdErr,
    ExpB
  )

  rownames(TB) <- VarCat
  colnames(TB) <- c("Coef(B)", "Std.Error", "Exp(B)")

  cat("\nCall:\n")
  print(x$call)

  cat("\nPopulations in Saturate Model: ", x$Populations, "\n", sep = "")
  cat("\nCoefficients:\n")
  print(TB)

  cat("\nLog Likelihood:\n")
  LL <- cbind(x$Log_Lik_Complete, x$Log_Lik_Null, x$Log_Lik_Logit, x$Log_Lik_Saturate)
  dimnames(LL) <- list("Estimation", c("Complete", "Null", "Logit", "Saturate"))
  print(t(LL))

  if (anyNA(unlist(x$data))) {
    cat("(", nrow(x$data) - nrow(na.omit(x$data)), " observations deleted due to missingness)\n", sep = "")
  }
}
