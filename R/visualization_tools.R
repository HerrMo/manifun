# viusalization tools

# function to plot funs of fun_data object
plot_funs <- function(data, ...) {
  UseMethod("plot_funs")
}


# default function for data in fundata in matrix format
plot_funs.default <- function(data, col = NULL) {
  n <- nrow(data)
  grid_len <- ncol(data)
  df_dat <- data.frame(
    args = rep(1:grid_len, n),
    vals = c(t(data)),
    id = as.factor(rep(1:n, each = grid_len))
  )

  if (!is.null(col)) df_dat$col <- as.factor(rep(col, each = grid_len))

  ggplot(df_dat) +
    geom_line(aes(x = args,
                  y = vals,
                  group = id,
                  colour = if (is.null(col)) {id} else {col})) +
    theme(legend.position = "None")
}

# same as above but for objects of class von dat
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
plot_emb <- function(embedding, ...) {
  UseMethod("embedding")
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
plot_emb.matrix <- function(embedding, color = NULL, labels = FALSE, size = 1) {
  # TODO argument checking (min 2-d data, etc)

  pts <- extract_points(emb, 2)
  p <- plot_emb.default(pts, ...)
  if (labels) p <- p + geom_text_repel(aes(x = dim1, y = dim2, label = 1:nrow(pts)))
  p
}

# for objects of class embedding
plot_emb.embedding <- function(embedding, color = NULL, labels = FALSE, size = 1) {
  # TODO argument checking (min 2-d data, etc)

  emb <- embedding$emb
  pts <- extract_points(emb, 2)
  p <- plot_emb.default(pts, ...)
  if (labels) p <- p + geom_text_repel(aes(x = dim1, y = dim2, label = label))
  p
}