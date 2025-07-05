#' Confidence intervals for GLSM coefficients
#'
#' Computes confidence intervals for the coefficients of a fitted `glsm` model,
#' along with the intervals for the corresponding odds ratios.
#'
#' @param object An object of class `glsm` returned by [glsm()].
#' @param parm Optional. A single coefficient name to extract its interval.
#'        If missing, intervals are computed for all coefficients.
#' @param level Confidence level. A single number between 0 and 1.
#'        Defaults to `0.95`.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' By default, the function returns the Wald-type confidence intervals for all coefficients.
#' The odds ratios (exponentiated coefficients) are also provided.
#'
#' @return A list of class `confint.glsm` with elements:
#'   \item{confint}{A matrix of lower and upper confidence limits for the coefficients.}
#'   \item{OR}{The same intervals transformed to the odds ratio scale.}
#'   \item{level}{The confidence level used (in percent).}
#'
#' @seealso [glsm()]
#'
#' @export
confint.glsm <- function(object, parm, level = 0.95, ...) {
  if (length(list(...)) > 0) {
    stop("This function does not accept additional arguments. Please review the documentation.")
  }

  if ((length(level) != 1L) || is.na(level) || (level <= 0) || (level >= 1)) {
    stop("'level' must be a single number between 0 and 1")
  }

  alpha <- 1 - level
  Z <- qnorm(1 - alpha / 2)

  vars <- rownames(object$coef)
  cats <- colnames(object$coef)
  all_parms <- paste0(rep(vars, each = length(cats)), ":", rep(cats, times = length(vars)))

  if (missing(parm)) {
    b <- as.vector(object$coef)
    se <- as.vector(object$Std.Error)
    li <- b - Z * se
    ls <- b + Z * se

    CI <- cbind(li, ls)
    rownames(CI) <- all_parms
    colnames(CI) <- c(
      paste0("lower ", round(100 * alpha / 2, 1), "%"),
      paste0("upper ", round(100 * (1 - alpha / 2), 1), "%")
    )

    OR <- exp(CI)
    rownames(OR) <- all_parms

  } else {
    if (!parm %in% all_parms) {
      stop(paste("The coefficient", parm, "is not present in the model."))
    }

    idx <- which(all_parms == parm)
    b <- as.vector(object$coef)[idx]
    se <- as.vector(object$Std.Error)[idx]

    li <- b - Z * se
    ls <- b + Z * se

    CI <- cbind(li, ls)
    rownames(CI) <- parm
    colnames(CI) <- c(
      paste0("lower ", round(100 * alpha / 2, 1), "%"),
      paste0("upper ", round(100 * (1 - alpha / 2), 1), "%")
    )

    if (grepl("\\(Intercept\\)", parm)) {
      OR <- NA
    } else {
      OR <- exp(CI)
      rownames(OR) <- parm
    }
  }

  res <- list(
    confint = CI,
    OR = OR,
    level = level * 100
  )
  class(res) <- "confint.glsm"
  return(res)
}
