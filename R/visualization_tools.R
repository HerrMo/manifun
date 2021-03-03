# viusalization tools

#' @import ggplot2


# function to plot funs of fun_data object
#' @export
plot_funs <- function(data, ...) {
  UseMethod("plot_funs")
}


# default function for data in fundata in matrix format
plot_funs.default <- function(data, col = NULL, args = NULL) {
  n <- nrow(data)
  grid_len <- ncol(data)
  df_dat <- data.frame(
    args = if (is.null(args)) rep(1:grid_len, n) else args,
    vals = c(t(data)),
    id = as.factor(rep(1:n, each = grid_len))
  )

  if (!is.null(col)) df_dat$col <- rep(col, each = grid_len)

  ggplot(df_dat) +
    geom_line(aes(x = args,
                  y = vals,
                  group = id,
                  colour = if (is.null(col)) {id} else {col})) +
    theme(legend.position = "None")
}

# same as above but for objects of class fundat
plot_funs.fundat <- function(data, col = NULL) {
  funs <- get_funs(data)
  grid <- get_grid(data)
  grid_len <- length(grid)

  n <- nrow(funs)
  id <- as.factor(rep(1:n, each = grid_len))

  if (is.null(col)) col <- rep(get_params(data)[[1]], each = grid_len)

  df_dat <- data.frame(
    args = rep(grid, n),
    vals = c(t(funs)),
    id = id,
    col = col
  )

  ggplot(df_dat) +
    geom_line(aes(x = args, y = vals, group = id, colour = col)) +
    theme(legend.position = "None")
}

# function to plot embeddings
#' @export
plot_emb <- function(embedding, ...) {
  UseMethod("plot_emb")
}

# default method for embedding data in 2d matrix format
plot_emb.default <- function(pts, color = NULL, size = 1, ...) {

  dat <- data.frame(dim1 = pts[, 1],
                    dim2 = pts[, 2],
                    color = 1:nrow(pts))

  if (!is.null(color)) dat$color <- color

  p <- ggplot(dat) +
    geom_point(aes(x = dim1,
                   y = dim2,
                   colour = color),
               size = size) +
    theme(legend.position = "Non") +
    ggtitle(label = "2d-embedding")
  p
}

# for embeddings coordinates in matrix format
plot_emb.matrix <- function(embedding, color = NULL, labels = FALSE, size = 1, ...) {
  # TODO argument checking (min 2-d data, etc)

  pts <- extract_points(embedding, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (labels) p <- p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  p
}

# for objects of class embedding
plot_emb.embedding <- function(embedding, color = NULL, labels = FALSE, size = 1) {
  # TODO argument checking (min 2-d data, etc)

  emb <- embedding$emb
  pts <- extract_points(emb, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (labels) p <- p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  p
}

plot_emb.isomap <- function(embedding, color = NULL, labels_off = TRUE, labels = NULL, size = 1, ...) {
  # TODO argument checking (min 2-d data, etc)

  pts <- extract_points(embedding, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (!labels_off) p <- if (is.null(labels)) {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  } else {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = labels))
  }
  p
}

plot_emb.umap <- function(embedding, color = NULL, labels_off = TRUE, labels = NULL, size = 1, ...) {
  # TODO argument checking (min 2-d data, etc)

  pts <- extract_points(embedding, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (!labels_off) p <- if (is.null(labels)) {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  } else {
    p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = labels))
  }
  p
}

plot_emb.tsne <- function(embedding, color = NULL, labels = FALSE, size = 1, ...) {
  # TODO argument checking (min 2-d data, etc)

  pts <- extract_points(embedding, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (labels) p <- p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  p
}

plot_emb.diffuse <- function(embedding, color = NULL, labels = FALSE, size = 1, ...) {
  # TODO argument checking (min 2-d data, etc)

  pts <- extract_points(embedding, 2)
  p <- plot_emb.default(pts, color = color, labels = labels, size = size, ...)
  if (labels) p <- p + ggrepel::geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  p
}
# convenience function for plotly to visualize embedding object
#' @export
plotly_viz <- function(x, ...) {
  UseMethod("plotly_viz", x)
}

plotly_viz.embedding <- function(emb, ..., size = 0.1) {
  plotly::plot_ly(x = emb$points[, 1], y = emb$points[, 2], z = emb$points[, 3],
                  size = size,
                  type = "scatter3d", ...)
}

plotly_viz.default <- function(mat, ..., size = 0.1) {
  plotly::plot_ly(x = mat[, 1], y = mat[, 2], z = mat[, 3],
                  size = size,
                  type = "scatter3d", ...)
}

plotly_viz.umap <- function(emb, ..., size = 0.1) {
  pts <- extract_points(emb)
  plotly::plot_ly(x = pts[, 1], y = pts[, 2], z = pts[, 3],
                  size = size,
                  type = "scatter3d", ...)
}

plotly_viz.tsne <- function(emb, ..., size = 0.1) {
  pts <- extract_points(emb)
  plotly::plot_ly(x = pts[, 1], y = pts[, 2], z = pts[, 3],
                  size = size,
                  type = "scatter3d", ...)
}

plotly_viz.isomap <- function(emb, ..., size = 0.1) {
  pts <- extract_points(emb)
  plotly::plot_ly(x = pts[, 1], y = pts[, 2], z = pts[, 3],
                  size = size,
                  type = "scatter3d", ...)
}


#' Plots images
#' @export
plot_pics <- function(dat, labels = NULL, nrow = NULL, ncol = NULL) {
  n_pxls <- ncol(dat)
  n_obs <- nrow(dat)

  dt_dat <- as.data.table(dat)

  tt_image <- melt(dt_dat, measure.vars = colnames(dt_dat))
  tt_image$id <- rep(1:n_obs, n_pxls)

  pixels <- expand.grid(0:(sqrt(n_pxls)-1), 0:(sqrt(n_pxls)-1))

  tt_image[, x := rep(pixels$Var1, each = n_obs)]
  tt_image[, y := rep(pixels$Var2, each = n_obs)]
  setnames(tt_image, "value", "intensity")

  tt_image[, id_fac := as.factor(tt_image$id)]
  if (!is.null(labels)) levels(tt_image$id_fac) <- labels

  ggplot(data = tt_image) +
    geom_raster(aes(x, y, fill = intensity)) +
    facet_wrap(
      ~ id_fac,
      nrow = if (is.null(nrow)) max(6, ceiling(n_obs/6)) else nrow,
      ncol = if (is.null(ncol)) 6 else ncol) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          strip.text = element_text(size = 15))
}

