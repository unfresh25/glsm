#' Print method for summary.glsm objects
#'
#' Nicely prints the output of [summary.glsm()], including coefficients and deviance comparisons.
#'
#' @param x An object of class `summary.glsm`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Prints the summary tables to the console. Invisibly returns `NULL`.
#'
#' @seealso [summary.glsm()], [glsm()]
#'
#' @export
print.summary.glsm <- function(x, ...){
  cat("\nCall:\n")
  print(x$Call)
  cat("\nCoefficients: \n",  sep = "")
  printCoefmat(x$coef, P.values=TRUE, has.Pvalue=TRUE)
  cat("\nAnalysis of Deviance (Chi-squared): \n")
  printCoefmat(x$`comparison test`, P.values=TRUE, has.Pvalue=TRUE)
}
