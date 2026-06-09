# Markowitz mean-variance portfolio optimization

compute_efficient_frontier <- function(ret_matrix, n_points = 80) {
  ret_matrix <- ret_matrix[, colSums(!is.na(ret_matrix)) > 30, drop = FALSE]
  if (ncol(ret_matrix) < 2) return(NULL)

  mu    <- colMeans(ret_matrix, na.rm = TRUE) * 252
  Sigma <- cov(ret_matrix, use = "pairwise.complete.obs") * 252
  n     <- length(mu)

  Dmat  <- 2 * (Sigma + 1e-7 * diag(n))
  dvec  <- rep(0, n)
  Amat  <- cbind(rep(1, n), diag(n))
  bvec  <- c(1, rep(0, n))

  targets <- seq(min(mu) * 0.85, max(mu) * 1.10, length.out = n_points)

  map_df(targets, function(target) {
    A_full <- cbind(Amat, mu)
    b_full <- c(bvec, target)
    tryCatch({
      sol <- solve.QP(Dmat, dvec, A_full, b_full, meq = 1)
      w   <- pmax(sol$solution, 0)
      w   <- w / sum(w)
      tibble(
        ret    = sum(w * mu),
        vol    = as.numeric(sqrt(t(w) %*% Sigma %*% w)),
        sharpe = ret / vol,
        weights = list(setNames(w, colnames(ret_matrix)))
      )
    }, error = function(e) NULL)
  }) %>%
    filter(!is.na(ret), vol > 0)
}

find_max_sharpe <- function(frontier, rf = 0.05) {
  frontier %>%
    mutate(sharpe_adj = (ret - rf) / vol) %>%
    slice_max(sharpe_adj, n = 1, with_ties = FALSE)
}

find_min_var <- function(frontier) {
  frontier %>% slice_min(vol, n = 1, with_ties = FALSE)
}
