#' Summary method for GLSM objects
#'
#' Produces a detailed summary of a `glsm` object, including the estimated coefficients,
#' standard errors, odds ratios, Wald statistics, degrees of freedom, p-values,
#' and an analysis of deviance comparing models.
#'
#' @param object An object of class `glsm`, returned by [glsm()].
#' @param ... Additional arguments (currently unused).
#'
#' @details The summary includes:
#' \itemize{
#'   \item A table of coefficients, standard errors, odds ratios, Wald tests, degrees of freedom, and p-values.
#'   \item A deviance table comparing the null, complete, logit, and saturated models.
#' }
#'
#' @return An object of class `summary.glsm` containing the summary tables.
#'
#' @seealso [glsm()], [print.summary.glsm()]
#'
#' @export
summary.glsm <- function(object, ...) {
  Coef <- as.vector(t(object$coef))
  StdErr <- as.vector(t(object$Std.Error))
  ExpB <- as.vector(t(object$ExpB))

  vars <- rownames(object$coef)
  cats <- colnames(object$coef)

  VarCat <- paste0(
    rep(vars, each = length(cats)),
    ":",
    rep(cats, times = length(vars))
  )

  TC <- cbind(
    `Coef(B)` = Coef,
    `Std.Error` = StdErr,
    `Exp(B)` = ExpB,
    Wald = object$Wald,
    DF = object$DF,
    P.value = object$P.value
  )

  rownames(TC) <- VarCat

  TAB <- cbind(
    Deviance = c(object$Dev_Null_vs_Logit, object$Dev_Logit_vs_Complete, object$Dev_Logit_vs_Saturate),
    DF = c(object$Df_Null_vs_Logit, object$Df_Logit_vs_Complete, object$Df_Logit_vs_Saturate),
    P.value = c(object$P.v_Null_vs_Logit, object$P.v_Logit_vs_Complete, object$P.v_Logit_vs_Saturate)
  )
  row.names(TAB) <- c("Null vs Logit", "Logit vs Complete", "Logit vs Saturate")

  res <- list(Call = object$call, `comparison test` = TAB, coef = TC)
  class(res) <- "summary.glsm"
  return(res)
}
