#' Compute quality of an embedding
#'
#'
#'


R_nx <- function(x, ...) {
  UseMethod("R_nx2")
}

R_nx.default <- function(d1, d2) {
  assertMatrix(d1)
  assertMatrix(d2)

  Q <- coRanking::coranking(d1,
                            d2,
                            input_Xi = "dist")

  nQ <- nrow(Q)
  N <- nQ + 1

  Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) /
    seq_len(nQ) / N

  Rnx <- ((N - 1) * Qnx - seq_len(nQ)) /
    (N - 1 - seq_len(nQ))
  Rnx[-nQ]
}

R_nx.embedding <- function(object, ndim = 2, p_space = FALSE) {
  ld_dist <- as.matrix(dist(object$points[, 1:ndim]))

  if (p_space) {
    Q <- coRanking::coranking(object$p_dist,
                              ld_dist,
                              input_Xi = "dist")
  } else {
    Q <- coRanking::coranking(object$f_dist,
                              ld_dist,
                              input_Xi = "dist")
  }

  nQ <- nrow(Q)
  N <- nQ + 1

  Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) /
    seq_len(nQ) / N

  Rnx <- ((N - 1) * Qnx - seq_len(nQ)) /
    (N - 1 - seq_len(nQ))
  Rnx[-nQ]
}

auc_rnx <- function(x, ...) {
  UseMethod("auc_rnx")
}

auc_rnx.default <- function(d1, d2, weight = "inv") {
  rnx <- R_nx2(d1, d2)

  weight <- match.arg(weight, c("inv", "ln", "log", "log10"))
  switch(
    weight,
    inv   = auc_ln_k_inv(rnx),
    log   = auc_log_k(rnx),
    ln    = auc_log_k(rnx),
    log10 = auc_log10_k(rnx),
    stop("wrong parameter for weight")
  )
}

auc_rnx.embedding <- function(object, ndim = 2, p_space = FALSE, weight = "inv") {
  rnx <- R_nx2(object, ndim = ndim, p_space = p_space)

  weight <- match.arg(weight, c("inv", "ln", "log", "log10"))
  switch(
    weight,
    inv   = auc_ln_k_inv(rnx),
    log   = auc_log_k(rnx),
    ln    = auc_log_k(rnx),
    log10 = auc_log10_k(rnx),
    stop("wrong parameter for weight")
  )
}

auc_ln_k_inv <- function(rnx) {
  Ks <- seq_along(rnx)
  return(sum(rnx / Ks) / sum(1 / Ks))
}

auc_log_k <- function(rnx) {
  Ks <- seq_along(rnx)
  return(sum(rnx * log(Ks)) / sum(log(Ks)))
}

auc_log10_k <- function(rnx) {
  Ks <- seq_along(rnx)
  return(sum(rnx * log10(Ks)) / sum(log10(Ks)))
}

q_local <- function(x, ...) {
  UseMethod("local_q")
}

q_local.default <- function(d1, d2, ...) {
  assertMatrix(d1)
  assertMatrix(d2)

  Q <- coRanking::coranking(d1,
                            d2,
                            input_Xi = "dist")

  nQ <- nrow(Q)
  N <- nQ + 1

  Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
  lcmc <- Qnx - seq_len(nQ) / nQ

  Kmax <- which.max(lcmc)

  Qlocal <- sum(lcmc[1:Kmax]) / Kmax
  return(as.vector(Qlocal))
}

q_local.embedding <-
  function(object, ndim = 3, p_space) {
    ld_dist <- as.matrix(dist(object$points[, 1:ndim]))

    if (p_space) {
      Q <- coRanking::coranking(object$p_dist,
                                ld_dist,
                                input_Xi = "dist")
    } else {
      Q <- coRanking::coranking(object$f_dist,
                                ld_dist,
                                input_Xi = "dist")
    }

    nQ <- nrow(Q)
    N <- nQ + 1

    Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
    lcmc <- Qnx - seq_len(nQ) / nQ

    Kmax <- which.max(lcmc)

    Qlocal <- sum(lcmc[1:Kmax]) / Kmax
    return(as.vector(Qlocal))
  }

q_global <- function(x, ...) {
  UseMethod("global_q")
}

q_global.default <- function(d1, d2) {
  assertMatrix(d1)
  assertMatrix(d2)

  Q <- coRanking::coranking(d1,
                            d2,
                            input_Xi = "dist")

  nQ <- nrow(Q)
  N <- nQ + 1

  Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
  lcmc <- Qnx - seq_len(nQ) / nQ

  Kmax <- which.max(lcmc)

  Qglobal <- sum(lcmc[(Kmax + 1):nQ]) / (N - Kmax)
  return(as.vector(Qglobal))
}

q_global.embedding <-
  function(object, ndim = 3, p_space){
    ld_dist <- as.matrix(dist(object$points[, 1:ndim]))

    if (p_space) {
      Q <- coRanking::coranking(object$p_dist,
                                ld_dist,
                                input_Xi = "dist")
    } else {
      Q <- coRanking::coranking(object$f_dist,
                                ld_dist,
                                input_Xi = "dist")
    }

    nQ <- nrow(Q)
    N <- nQ + 1

    Qnx <- diag(apply(apply(Q, 2, cumsum), 1, cumsum)) / seq_len(nQ) / N
    lcmc <- Qnx - seq_len(nQ) / nQ

    Kmax <- which.max(lcmc)

    Qglobal <- sum(lcmc[(Kmax + 1):nQ]) / (N - Kmax)
    return(as.vector(Qglobal))
  }