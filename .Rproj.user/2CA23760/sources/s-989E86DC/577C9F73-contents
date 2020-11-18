#' Embedding data based on distance matrix
#'
#' @param dist_mat Either an object of class dist or square matrix of pairwise distance of the data to be embedded
#' @param method Manifold method use to compute the embedding. One of "isomap" (isometric feature mapping), "umap"
#' (uniform manifold approximation and projection), "diffmap" (diffusion map), "mds" (multi-dimensional scaling), and
#' "t-sne" (t-distributed stochastic neighborhood embedding.)
#'
#' Returns an embedding object of class <method>.

embed <- function(dist_mat, method = c("isomap", "umap", "diffmap", "mds", "tsne"), ...) {
  method <- match.arg(method, c("isomap", "umap", "diffmap", "mds", "tsne"))

  # change to assert: accept only matrices?
  dist_mat <- if (inherits(dist_mat, "dist")) {as.matrix(dist_mat)}

  emb <-
    switch(
      method,
      "isomap" = vegan::isomap(dist_mat, ...),
      "umap" = umap::umap(dist_mat, input = "dist", ...),
      "diffmap" =  diffuse2(dist_mat, ...),
      "mds" = cmdscale(dist_mat, ...),
      "tsne" = Rtsne::Rtsne(dist_mat, is_distance = TRUE, ...)
    )

  if (method == "tsne") class(emb) <- "tsne"

  emb
}




