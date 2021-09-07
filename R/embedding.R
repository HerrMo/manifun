#' Embedding data
#'
#' Wrapper function for several embedding methods.
#'
#' @param dist_mat Either an object of class dist or square matrix of pairwise distance of the data to be embedded
#' @param method Manifold method use to compute the embedding. One of "isomap" (isometric feature mapping), "umap"
#' (uniform manifold approximation and projection), "diffmap" (diffusion map), "mds" (multi-dimensional scaling), and
#' "tsne" (t-distributed stochastic neighborhood embedding.)
#' @param ... Arguments to be passed to the embedding methods
#'
#' @return Returns an embedding object of class <method>.

#' @export
#' @import vegan
#' @import Rtsne
#' @import diffusionMap
#' @import umap
#' @usage NULL
#' @format NULL
embed <- function(dist_mat, method = c("isomap", "umap", "diffmap", "mds", "tsne"), ...) {
  method <- match.arg(method, c("isomap", "umap", "diffmap", "mds", "tsne"))

  # change to assert: accept only matrices?
  if (inherits(dist_mat, "dist")) dist_mat <- as.matrix(dist_mat)

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


# help fun S3 class to extract embedding coordinates
#' @export
extract_points <- function(x, ...) {
  UseMethod("extract_points")
}

# S3 method for isomap
#' @export
extract_points.isomap <- function(embedding, ndim = dim(embedding$points)[2]) {
  embedding$points[, 1:ndim]
}

# S3 method for umap
#' @export
extract_points.umap <- function(embedding, ndim = dim(embedding$layout)[2]) {
  embedding$layout[, 1:ndim]
}

# S3 method for diffusionMap
extract_points.diffuse <- function(embedding, ndim = dim(embedding$X)[2]) {
  embedding$X[, 1:ndim]
}

# S3 method for matrix output, e.g. mds
#' @export
extract_points.matrix <- function(embedding, ndim = dim(embedding)[2]) {
  embedding[, seq_len(ndim), drop = FALSE]
}

# S3 method for tsne
#' @export
extract_points.tsne <- function(embedding, ndim = dim(embedding$Y)[2]) {
  embedding$Y[, 1:ndim]
}


