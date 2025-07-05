#' Plot method for GLSM objects
#'
#' Plots the estimated coefficients or odds ratios with confidence intervals
#' from a fitted `glsm` model using `ggplot2`.
#'
#' @param x An object of class `glsm` returned by [glsm()].
#' @param type Type of plot: `"coef"` for log-odds coefficients or `"OR"` for odds ratios. Defaults to `"coef"`.
#' @param level Confidence level for the intervals. Defaults to `0.95`.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' This plot shows point estimates with horizontal confidence intervals.
#' A vertical dashed line is drawn at zero (for coefficients) or one (for odds ratios).
#'
#' The function requires the `ggplot2` package.
#'
#' @return A `ggplot` object is produced and displayed.
#'
#' @seealso [glsm()]
#'
#' @export
plot.glsm <- function(x, type = c("coef", "OR"), level = 0.95, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete ggplot2 es necesario para esta función.")
  }

  type <- match.arg(type)
  alpha <- 1 - level
  Z <- qnorm(1 - alpha / 2)

  B <- as.vector(x$coef)
  SE <- as.vector(x$Std.Error)
  LI <- B - Z * SE
  LS <- B + Z * SE

  vars <- rownames(x$coef)
  cats <- colnames(x$coef)
  labels <- paste0(rep(vars, each = length(cats)), ":", rep(cats, times = length(vars)))

  if (type == "OR") {
    B <- exp(B)
    LI <- exp(LI)
    LS <- exp(LS)
  }

  df <- data.frame(
    VarCat = labels,
    Estimate = B,
    Lower = LI,
    Upper = LS
  )

  df$VarCat <- factor(df$VarCat, levels = df$VarCat[order(df$Estimate)])

  library(ggplot2)
  p <- ggplot(df, aes(x = Estimate, y = VarCat)) +
    geom_point() +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
    geom_vline(xintercept = ifelse(type == "coef", 0, 1), linetype = "dashed", color = "gray40") +
    labs(
      x = ifelse(type == "coef", "Log-Odds Coefficient", "Odds Ratio"),
      y = "",
      title = paste0(ifelse(type == "coef", "Coefficients", "Odds Ratios"), " with ", round(level*100, 1), "% CI")
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    )

  print(p)
}
