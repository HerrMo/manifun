# metrics

#' @export
d_dist <- function(mat, grid, a = 0) {

  derivs <- t(apply(mat, 1, function(f) my_deriv(grid = grid, vals = f)))

  dfuns <- dist(mat)
  dders <- dist(derivs)

  a * as.matrix(dfuns) + (1 - a) * as.matrix(dders)
}

my_deriv = function(grid, vals){
  diff(vals)/diff(grid)
}


#' @export
wasserstein_metric <- function(f1, f2, grid, a = 1, norm = FALSE) {
  # f1 = mu0, f2 = mu1 in the paper

  require(caTools) # for trapz (numerical integration)
  require(transport) # for wasserstein1d

  if (norm) return(wasserstein1d(f1, f2)) # computes standard wasserstein-1 distance

  # compute standard measure of domain
  omega <- tail(grid, 1) - grid[1]

  f_diff <- f2 - f1
  c <- trapz(grid, f_diff) * (1/omega)

  mu <- f_diff - c
  if (sum(mu) == 0) return(0) # saves some time if f1 = f2

  mu1 <- (mu + abs(mu)) / 2 # compute positive part
  mu2 <- (mu - abs(mu)) / 2 # compute negative part

  wasserstein1d(mu1, mu2) + omega/a * abs(c)
}

#' @export
wasser_dist <- function(dat, ...) {
  nfun <- nrow(dat)
  combs <- combn(1:nfun, 2)
  m_dist <- diag(x = 0, nfun)
  for (i in seq_len(ncol(combs))) {
    ind <- combs[, i]
    ind1 <- ind[1]
    ind2 <- ind[2]
    m_dist[ind1, ind2] <- wasserstein_metric(dat[ind1, ],
                                             dat[ind2, ],
                                             ...)
  }
  m_dist[lower.tri(m_dist)] <- t(m_dist)[lower.tri(m_dist)]
  m_dist
}


# examples
# n_funs <- 1000
# grid_size <- 200
# grid <- seq(0, 1, l = grid_size)
#
# gauss1 <- function(s) dnorm(s, mean = 0.25, sd = 0.1)
# gauss2 <- function(s) dnorm(s, mean = 0.75, sd = 0.1)
# gauss3 <- function(s) dnorm(s, mean = 0.5, sd = 0.1)
#
# f1 <- 2 * gauss1(grid)
# f2 <- 2 * gauss2(grid)
# f3 <- gauss2(grid)
# f4 <- gauss2(grid) + 0.5
# f5 <- gauss3(grid)
# f6 <- 2 * gauss3(grid)
# funs <- rbind(f1, f2, f3, f4, f5, f6)
# matplot(grid, t(funs))
#
# wasser_dist(funs, grid = grid)