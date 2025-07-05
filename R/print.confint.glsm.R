#' Print method for confidence intervals of a GLSM
#'
#' Prints the confidence intervals and odds ratios computed by [confint.glsm()].
#'
#' @param x An object of class `confint.glsm`.
#' @param ... Further arguments (unused).
#'
#' @return Prints output to the console.
#'
#' @export
print.confint.glsm <- function(x, ...) {
  cat(x$level, "% confidence intervals for coefficients\n", sep = "")
  print(x$confint)

  cat("\n", x$level, "% confidence intervals for Odds Ratios\n", sep = "")
  print(x$OR)
}
