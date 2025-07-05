#' Generalized Logistic Saturated Model (GLSM)
#'
#' Fits a multinomial logistic regression model with a saturated structure.
#' The function estimates the coefficients using an Iteratively Reweighted Least Squares (IRLS) algorithm,
#' calculates log-likelihoods for the null, complete, and saturated models, and returns detailed outputs
#' for model comparison, predicted probabilities, odds, and odds ratios.
#'
#' @param formula A formula specifying the dependent and independent variables, e.g., `y ~ x1 + x2`.
#' @param data A `data.frame` containing the variables used in the formula.
#' @param ref A character string indicating the reference category of the dependent variable. If `NaN`, the first level is used by default.
#'
#' @details
#' This implementation works for categorical outcomes with 3 or more levels.
#' For binary logistic regression, use `lsm()` instead.
#' The function automatically computes:
#' \itemize{
#'   \item Log-likelihoods for null, complete, logit, and saturated models.
#'   \item Deviance and p-values for model comparisons.
#'   \item Coefficients, standard errors, Wald statistics, and odds ratios.
#' }
#'
#' @return An object of class `glsm` containing:
#' \describe{
#'   \item{coefficients}{The estimated coefficients.}
#'   \item{Std.Error}{Standard errors of the coefficients.}
#'   \item{ExpB}{Exponentiated coefficients (odds ratios).}
#'   \item{Log.Lik}{Log-likelihoods for the different models.}
#'   \item{Deviance}{Deviances and tests for model comparisons.}
#'   \item{Odds}{Fitted odds and odds ratios.}
#'   \item{Probabilities}{Fitted category probabilities.}
#'   \item{call}{The original function call.}
#' }
#'
#' @examples
#' \dontrun{
#' data(mydata)
#' model <- glsm(prog ~ ses + write + read, data = mydata, ref = "academic")
#' summary(model)
#' confint(model, parm = "write:vocation")
#' predict(model, type = "response")
#' plot(model)
#' }
#'
#' @export
glsm <- function(formula, data, ref = NaN) {
  xdata <- data
  mf <- model.frame(formula = formula, data = xdata)

  predictors <- colnames(mf)[-1]

  n_data <- as.data.frame(mf)
  if (length(unique(n_data[[1]])) < 3) {
    stop("The dependent variable must have 3 or more levels.\n\nIf you are trying to perform a dichotomous logistic regression model,\nI recommend using the lsm() function from the package of the same name.")
  }

  if (is.ordered(n_data[1])) {
    stop("The values of the dependent variable should not be ordered.")
  }

  lvs <- levels(as.factor(n_data[[1]]))
  rw <- nrow(n_data)
  means <- list()

  for (i in lvs){
    n_data[paste0("u_", i)] <- ifelse(n_data[, 1] == i, 1, 0)
  }

  Y <- model.response(mf)

  if (!is.factor(Y)) {
    Y <- factor(Y)
  }

  if (is.nan(ref)) {
    ref <- levels(Y)[1]
  }

  if (!(ref %in% levels(Y))) {
    stop("La base de referencia no existe en los niveles de Y.")
  }

  Y <- relevel(Y, ref = ref)


  X <- model.matrix(attr(mf, "terms"), data = mf)

  # -----------------------------------------
  #                Null model
  #------------------------------------------

  means_u <- colMeans(n_data[, grepl("^u_", names(n_data))])

  p_u <- means_u

  l <- list()

  for(i in 1:length(means_u)){
    l[[i]] <- means_u[i] * log(p_u[i])
  }

  l <- rw * sum(unlist(l))
  Log_Lik_Null <- l

  # -----------------------------------------
  #             Complete model
  #------------------------------------------

  l <- list()

  for(i in 1:length(lvs)){
    u <- n_data[, grepl("^u_", names(n_data))][i]
    l[[i]] <- ifelse(u == 0, 0, u * log(u))
  }

  l <- sum(unlist(l), na.rm = T)
  Log_Lik_Complete <- l

  # -----------------------------------------
  #             Saturated model
  #------------------------------------------

  ff <- count(data, vars = c(names(mf)[-1]))
  names(ff)[ncol(ff)] <- c("n")
  J <- nrow(ff)

  aa <- split(mf,mf[,names(mf)[1]])
  bb <- lapply(aa, function(x) count(x, vars = c(colnames(x)[-1])))

  for (i in 1:length(bb)) {
    names(bb[[i]])[ncol(bb[[i]])] <- c(paste0("z_", names(bb[i]), "_j"))
  }

  for(i in 1:length(bb)) {
    bb[[i]] <- join(bb[[i]], ff, by = names(mf)[-1])
    bb[[i]][paste0("p_", names(bb[i]), "_j")] <- bb[[i]][paste0("z_", names(bb[i]), "_j")]/bb[[i]]["n"]
  }

  tb <- as.data.frame(bb[[1]])
  tb <- tb[, c(1:(ncol(tb) - 3), ncol(tb) - 1, ncol(tb) - 2, ncol(tb))]

  for(i in bb[-1]){
    tb <- join(tb, i, by = c(names(mf)[-1], "n"), type = "full")
  }

  tb[is.na(tb)] <- 0
  nc <- length(names(mf)[-1]) + 2
  pos <- 0
  l <- numeric(length(bb))

  tb <- as.data.frame(tb)

  for (i in 1:(length(bb))) {
    tb[paste0("l_", names(bb[i]))] <- ifelse(tb[, nc + pos + 1] == 0 | tb[, nc + pos] == 0, 0, tb[, nc + pos] * log(tb[, nc + pos + 1]))
    pos <- pos + 2
  }

  tb["Lp"] <- apply(tb[, grep("^l_", names(tb))], 1, function(x) {
    if(0 %in% x){
      return(0)
    } else{
      return(sum(x))
    }
  })

  tb <- tb[, -grep("^l_", names(tb))]

  l <- sum(tb$Lp)
  Log_Lik_Saturate <- l

  Populations <- J
  Saturated_Table <- tb
  Saturated_List <- bb

  z_rj <- tb[, grep("^z_", names(tb))]
  nj <- tb[, 'n']
  p_rj_tilde <- z_rj/nj

  names(p_rj_tilde) <- gsub("^z_", "p_", names(p_rj_tilde))
  names(p_rj_tilde) <- paste0(names(p_rj_tilde), "_tilde")
  # -----------------------------------------
  #           Model parameters
  #------------------------------------------

  irls <- function(X, Y, ref, intercept = TRUE, max_iter = 100, tol = 1e-6, verbose = TRUE) {
    n <- nrow(X)

    if (intercept) {
      X <- cbind(1, X)
    }

    p <- ncol(X)

    # Asegurar que el nivel de referencia sea el último
    if (!is.factor(Y)) {
      Y <- factor(Y)
    }
    if (!(ref %in% levels(Y))) {
      stop("El nivel de referencia no está en los niveles de Y.")
    }
    Y <- factor(Y, levels = c(setdiff(levels(Y), ref), ref))

    K <- length(levels(Y))

    Y_mat <- matrix(0, nrow = n, ncol = K - 1)
    for (k in 1:(K - 1)) {
      Y_mat[, k] <- as.numeric(Y == levels(Y)[k])
    }

    softmax <- function(eta) {
      exp_eta <- exp(eta)
      denom <- 1 + rowSums(exp_eta)
      pi <- cbind(exp_eta, 1) / denom
      return(pi)
    }

    beta <- matrix(0, nrow = p, ncol = K - 1)

    for (iter in 1:max_iter) {
      eta <- X %*% beta
      pi <- softmax(eta)
      pi_k <- pi[, 1:(K - 1)]

      g <- t(X) %*% (Y_mat - pi_k)

      I <- matrix(0, nrow = p * (K - 1), ncol = p * (K - 1))
      for (i in 1:n) {
        p_vec <- pi[i, ]
        Cov <- diag(p_vec[1:(K - 1)]) - outer(p_vec[1:(K - 1)], p_vec[1:(K - 1)])
        Xi <- matrix(X[i, ], nrow = p)
        I <- I + kronecker(Cov, Xi %*% t(Xi))
      }

      g_vec <- as.vector(g)
      delta <- solve(I, g_vec)
      beta <- beta + matrix(delta, nrow = p, ncol = K - 1)

      if (sqrt(sum(delta^2)) < tol) {
        if (verbose) cat("Converged in", iter, "iterations\n")
        break
      }
    }

    rownames(beta) <- colnames(X)
    clases <- levels(Y)[1:(K - 1)]
    base_class <- levels(Y)[K]
    colnames(beta) <- clases

    cov_beta <- solve(I)
    std_error <- sqrt(diag(cov_beta))
    std_error_mat <- matrix(std_error, nrow = p, ncol = K - 1)
    rownames(std_error_mat) <- colnames(X)
    colnames(std_error_mat) <- clases

    exp_eta <- exp(eta)
    denom <- 1 + rowSums(exp_eta)
    pi <- cbind(exp_eta, 1) / denom
    colnames(pi) <- c(clases, base_class)

    Y_mat_full <- matrix(0, nrow = n, ncol = K)
    for (k in 1:K) {
      Y_mat_full[, k] <- as.numeric(Y == levels(Y)[k])
    }
    LogLik <- sum(Y_mat_full * log(pi))

    return(list(
      coefficients = beta,
      std_error = std_error_mat,
      logLik = LogLik,
      pi = pi,
      vcov = cov_beta,
      iterations = iter
    ))
  }

  res <- irls(X, Y_fac, ref, intercept = FALSE)

  coef <- res$coefficients
  coefficients <- as.numeric(coef)

  ExpB <- exp(coef)

  Std.Error <- res$std_error

  Wald <- (coefficients/Std.Error)^2
  DF <- rep(1, length(coef))
  P.value <- pchisq(Wald, DF, lower.tail = F)

  Log_Lik_Logit <- res$logLik

  Dev_Null_vs_Logit <- 2 * (Log_Lik_Logit - Log_Lik_Null)
  Dev_Logit_vs_Complete <- -2 * Log_Lik_Logit
  Dev_Logit_vs_Saturate <- 2 * (Log_Lik_Saturate - Log_Lik_Logit)

  K <- length(lvs)
  Df_Null_vs_Logit <- 2 * (1 + K) - 2
  Df_Logit_vs_Complete <- 2 * (rw - (1 + K))
  Df_Logit_vs_Saturate <- 2 * (J - (1 + K))

  P.v_Null_vs_Logit <- pchisq(Dev_Null_vs_Logit, Df_Null_vs_Logit, lower.tail = F)
  P.v_Logit_vs_Complete <- pchisq(Dev_Logit_vs_Complete, Df_Logit_vs_Complete, lower.tail = F)
  P.v_Logit_vs_Saturate <- pchisq(Dev_Logit_vs_Saturate, Df_Logit_vs_Saturate, lower.tail = F)

  lvs_t <- lvs[-match(ifelse(is.na(ref), lvs[1], ref), lvs)]
  ref_lvl <- match(ifelse(is.na(ref), lvs[1], ref), lvs)

  p_rj = res$pi

  p_ref <- p_rj[, which(colnames(p_rj) %in% lvs[ref_lvl])]
  odd_p <- p_rj[, setdiff(colnames(p_rj), lvs[ref_lvl])]
  odds <- odd_p / p_ref

  logit_p <- log(odds)
  or <- exp(coef)

  colnames(p_rj) <- paste0("p_", lvs, "_j")

  ltb <- tb[predictors]

  ltb$n <- tb$n
  ltb[colnames(tb[grepl("^z_", names(tb))])] <- tb[grepl("^z_", names(tb))]
  ltb[colnames(p_rj)] <- p_rj
  ltb[colnames(logit_p)] <- logit_p

  m_rj <- ltb[, 'n'] * ltb[, grepl("^p_", names(ltb))]
  colnames(m_rj) <- paste0("m_", lvs, "_j")

  v_rj <- ltb[, grepl("^p_", names(ltb))] * (1 - ltb[, grepl("^p_", names(ltb))])
  colnames(v_rj) <- paste0("v_", lvs, "_j")
  ltb[colnames(v_rj)] <- v_rj

  V_rj <- ltb$n * v_rj

  S_p <- ((tb$n * (p_rj_tilde  - p_rj)))/v_rj
  colnames(S_p) <- paste0("S_", lvs, "(p)")

  cov_m <- res$vcov

  for (i in seq_along(lvs_t)) {
    row.names(cov_m) = colnames(cov_m) <- gsub(paste0(":", i), paste0(":", lvs_t[i]), row.names(cov_m))
  }

  logi <- list(
    data = n_data,
    coefficients = coefficients,
    coef = coef,
    Std.Error = Std.Error,
    ExpB = ExpB,
    Wald = as.numeric(Wald),
    DF = DF,
    P.value = as.numeric(P.value),
    Log_Lik_Complete = Log_Lik_Complete,
    Log_Lik_Null = Log_Lik_Null,
    Log_Lik_Saturate = Log_Lik_Saturate,
    Log_Lik_Logit = Log_Lik_Logit,
    Populations = Populations,
    Dev_Null_vs_Logit = Dev_Null_vs_Logit,
    Dev_Logit_vs_Complete = Dev_Logit_vs_Complete,
    Dev_Logit_vs_Saturate = Dev_Logit_vs_Saturate,
    Df_Null_vs_Logit = Df_Null_vs_Logit,
    Df_Logit_vs_Complete = Df_Logit_vs_Complete,
    Df_Logit_vs_Saturate = Df_Logit_vs_Saturate,
    P.v_Null_vs_Logit = P.v_Null_vs_Logit,
    P.v_Logit_vs_Complete = P.v_Logit_vs_Complete,
    P.v_Logit_vs_Saturate = P.v_Logit_vs_Saturate,
    Logit_r = logit_p,
    p_logit_complete = n_data[, grepl("^u_", names(n_data))],
    p_hat_null = p_u,
    p_rj = p_rj,
    odd = odds,
    OR = or,
    z_rj = z_rj,
    nj = nj,
    p_rj_tilde = p_rj_tilde,
    v_rj = v_rj,
    m_rj = m_rj,
    V_rj = V_rj,
    V = cov(z_rj),
    S_p = S_p,
    I_p = cov(S_p),
    Zast_j = scale(z_rj),
    mcov = cov_m,
    mcor = cov2cor(cov_m),
    Esm = tb,
    Elm = ltb,
    call = match.call(),
    terms = terms(mf)
  )

  class(logi) <- "glsm"

  return(logi)
}
