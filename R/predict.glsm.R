#' Predict method for GLSM objects
#'
#' Computes predictions from a fitted `glsm` model. Supported prediction types include
#' linear predictors (link), fitted probabilities (response), odds, and odds ratios (OR).
#'
#' @param object An object of class `glsm` returned by [glsm()].
#' @param newdata An optional data frame in which to look for variables with which to predict.
#'        Currently, passing `newdata` is not supported.
#' @param type Type of prediction to compute. One of `"link"` (linear predictor),
#'        `"response"` (probabilities), `"odd"` (odds), or `"OR"` (odds ratios). Defaults to `"response"`.
#' @param level Confidence level for the linear predictor interval. Only used if `type = "link"`.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' The function returns the requested type of prediction:
#' \itemize{
#'   \item `"link"`: Linear predictors with confidence intervals.
#'   \item `"response"`: Predicted class probabilities.
#'   \item `"odd"`: Estimated odds.
#'   \item `"OR"`: Odds ratios relative to the reference category.
#' }
#'
#' @return A list with predictions and confidence intervals if `type = "link"`, otherwise a matrix of predicted values.
#'
#' @seealso [glsm()]
#'
#' @export
predict.glsm <- function(object, newdata = NULL,
                         type = NULL,
                         level = 0.95, ...) {

  if (is.null(type)) {
    type <- "response"
  }

  type <- match.arg(type, choices = c("link", "response", "odd", "OR"))

  if ((length(level) != 1L) || is.na(level) || (level <= 0) || (level >= 1)) {
    stop("'level' must be a single number between 0 and 1")
  }

  if (!is.null(newdata)) {
    stop("Predictions for new data not implemented yet.")
  }

  beta <- object$coef

  if (is.null(newdata)) {
    data_ <- object$data
  } else {
    data_ <- newdata
  }

  mf <- model.frame(object$terms, data = data_)
  X <- model.matrix(object$terms, data = mf)

  if (colnames(X)[1] != "(Intercept)" && !all(X[,1] == 1)) {
    X <- cbind("(Intercept)" = 1, X)
  }

  eta <- X %*% beta

  if (type == "link") {
    pred <- eta
  } else if (type == "response") {
    exp_eta <- exp(eta)
    denom <- 1 + rowSums(exp_eta)
    pi <- cbind(exp_eta, 1) / denom
    pred <- pi
  } else if (type == "odd") {
    exp_eta <- exp(eta)
    pred <- exp_eta
  } else if (type == "OR") {
    exp_eta <- exp(eta)
    denom <- 1 + rowSums(exp_eta)
    pi <- cbind(exp_eta, 1) / denom
    ref_p <- pi[, ncol(pi)]
    odds <- pi[, -ncol(pi)] / ref_p
    pred <- odds
  }

  se <- sqrt(diag(object$vcov))
  z_value <- qnorm(1 - (1 - level)/2)

  if (type == "link") {
    ci_lower <- pred - z_value * se
    ci_upper <- pred + z_value * se
    return(list(predictions = pred, ci_lower = ci_lower, ci_upper = ci_upper))
  } else {
    return(pred)
  }
}
