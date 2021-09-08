#' Interactive visualization of a functional data embedding
#'
#' Visualizes a functional data set and its two- to five-dimensional embeddings
#' using shiny.
#'
#' @param emb an embedding object as produced by 'embed'
#' @param funs matrix or data.frame containing the functional observations row-wise
#' @param grid optional vector of grid points, respectively arguments at which the functions have been evaluated
#' @param color optional vector used for coloring the functions / embedding vectors
#'
#' @details After selecting points
#' in the embedding(s) the corresponding functions are highlighted in red.
#' Clicking on/close to a function also higlights the respective embeddings.
#'
#' @export
#' @import shiny
#' @importFrom tidyr pivot_longer
#' @importFrom rlang sym
#' @importFrom dplyr pull
#' @usage NULL
#' @format NULL
shiny_embed <- function(emb, funs, grid = 1:ncol(funs), color = NULL, ...) {
  # TODO arg check
  # TODO add groups
  checkmate::assert_matrix(emb, mode = "numeric", min.cols = 2, max.cols = 5)
  checkmate::assert_matrix(funs, mode = "numeric", nrows = nrow(emb))
  checkmate::assert_numeric(grid, any.missing = FALSE, len = ncol(funs))
  if (is.null(color)) color <- rep(1, nrow(emb))
  checkmate::assert_atomic_vector(color, any.missing = FALSE, len = nrow(emb))

  n_funs <- nrow(funs)
  n_dims <- ncol(emb)
  grid_size <- length(grid)
  plots <- t(combn(1:n_dims, 2))
  plotnames <- apply(plots, 1, paste0, collapse = "")

  emb <- data.frame(emb)
  colnames(emb) <- paste0("dim", 1:n_dims)
  emb$id <- 1:nrow(emb)
  emb$color <- color

  funs <- data.frame(funs, id = 1:n_funs, color = color)
  colnames(funs) <- c(grid, "id", "color")
  funs <- funs |>
    pivot_longer(-c(id, color), names_to = "arg", names_transform = list(arg = as.numeric))

  color_scale <- switch(tail(class(color), 1),
    "numeric" = scale_color_viridis_c(values = seq(0,1, l = 10)^2),
     scale_color_discrete()
  )

  shinyApp(
    ui <- make_embed_ui(n_dims),
    server <- function(input, output) {
      alpha_f <- max(.1, 1/sqrt(n_funs))
      alpha_p <- max(.3, 1/sqrt(n_funs))

      selected <- reactive(unique(
        c(brushedPoints(emb, input[["select"]]) |> pull(id),
          nearPoints(funs, input[["clicked"]]) |> pull(id))))

      lapply(seq_along(plotnames), function(p) {
        output[[paste0("plot_", plotnames[p])]] <- renderPlot({
          xvar <- paste0("dim", plots[p, 1])
          yvar <- paste0("dim", plots[p, 2])
          p <- ggplot(emb) +
            geom_point(aes_string(x = xvar, y = yvar, color = "color"), alpha = alpha_p) +
            xlab(paste("Dim", plots[p, 1])) + ylab(paste("Dim", plots[p, 2])) +
            theme(legend.position = "none") + color_scale
          if (length(selected())) {
            p <- p + geom_point(data = subset(emb, id %in% selected()),
                                aes_string(x = xvar, y = yvar), col = "red", shape = 1)
          }
          p
        })
      })
      output[["funs"]] <- renderPlot({
        p <- ggplot(funs) +
          geom_line(aes(x = arg, y = value, group = id, color = color), alpha = alpha_f) +
          xlab("t") + ylab("x(t)") + theme(legend.position = "none") + color_scale
        if (length(selected())) {
          p <- p + geom_line(data = subset(funs, id %in% selected()),
                             aes(x = arg, y = value, group = id),
                             col = "red", alpha = 3 *alpha_f, lwd = 1)
        }
        p
      })
      output[["select_info"]] <- renderPrint({
        selected <- unique(
          c(brushedPoints(emb, input[["select"]]) |> pull(id),
            nearPoints(funs, input[["clicked"]]) |> pull(id)))
        as.integer(selected)
      })
    }
  )
}

make_embed_ui <- function(n_dims) {
  if (n_dims == 2) {
    return(fluidPage(
      fluidRow(
        column(6, plotOutput("funs", click = "clicked")),
        column(6, plotOutput("plot_12", brush = "select"))
      ),
      fluidRow(
        column(12, verbatimTextOutput("select_info"))
      )
    ))
  }
  if (n_dims == 3) {
    return(fluidPage(
      fluidRow(
        column(6, plotOutput("plot_12", brush = "select")),
        column(6, plotOutput("plot_13", brush = "select"))
      ),
      fluidRow(
        column(6, plotOutput("funs", click = "clicked")),
        column(6, plotOutput("plot_23", brush = "select"))
      ),
      fluidRow(
        column(12, verbatimTextOutput("select_info"))
      )
    ))
  }
  if (n_dims == 4) {
    return(fluidPage(
      fluidRow(
        column(4, plotOutput("plot_12", brush = "select")),
        column(4, plotOutput("plot_13", brush = "select")),
        column(4, plotOutput("plot_14", brush = "select"))
      ),
      fluidRow(
        column(4, verbatimTextOutput("select_info")),
        column(4, plotOutput("plot_23", brush = "select")),
        column(4, plotOutput("plot_24", brush = "select"))
      ),
      fluidRow(
        column(8, plotOutput("funs", click = "clicked")),
        column(4, plotOutput("plot_34", brush = "select"))
      )
    ))
  }
  if (n_dims == 5) {
    return(fluidPage(
      fluidRow(
        column(3, plotOutput("plot_12", brush = "select")),
        column(3, plotOutput("plot_13", brush = "select")),
        column(3, plotOutput("plot_14", brush = "select")),
        column(3, plotOutput("plot_15", brush = "select"))
      ),
      fluidRow(
        column(3, verbatimTextOutput("select_info")),
        column(3, plotOutput("plot_23", brush = "select")),
        column(3, plotOutput("plot_24", brush = "select")),
        column(3, plotOutput("plot_25", brush = "select"))
      ),
      fluidRow(
        column(6, plotOutput("funs", click = "clicked", height = "800px")),
        column(6,
          fluidRow(
            column(12,
              fluidRow(
                column(6, plotOutput("plot_34", brush = "select")),
                column(6, plotOutput("plot_35", brush = "select"))
              ),
              fluidRow(
                column(6, plotOutput("plot_45", brush = "select"), offset = 6)
              )
            )
          )
        )
      )
    ))
  }
}


