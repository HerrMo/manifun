#' Interactive visualization of embeddings
#'
#' Visualizes a functional data set and up to four embeddings of it using shiny.
#'
#' @param l_embs list of embedding objects as produced by 'embed'
#' @param funs matrix or data.frame containing the functional observations row-wise
#' @param grid vector of grid points, respectively arguments at which the functions have been evaluated
#' @param grouping vector indicating group membership. Used for color coding.
#'
#' @details The dotted black line indicates the global mean function. After selecting points
#' in the embedding(s) the corresponding functions and their mean(dashed black line)
#' will be plotted as well. Note, if you select several hundred points, plotting takes some time.
#' In addition, the indices of the selected functions are depicted.
#'
#' @export
#' @import shiny
#' @usage NULL
#' @format NULL
shiny_viz <- function(l_embs, funs, grid, grouping = NULL, ...) {
  # TODO arg check
  # TODO add groups
  methods <- vapply(l_embs,
                    function(emb) class(emb)[[1]],
                    FUN.VALUE = character(1))

  if (length(unique(methods)) != length(methods)) {
    methods <- paste0(methods, "-", 1:length(methods))
  }

  # make sure we have unique names for every embedding/plot
  # unique_nams <- paste(methods, seq_along(l_embs), sep = "-")
  unique_nams <- methods
  names(l_embs) <- unique_nams

  n_funs <- nrow(funs)
  grid_size <- length(grid)

  shinyApp(
    ui <- fluidPage(
      fluidRow(
        lapply(unique_nams, function(meth) {
          column(3,
                 plotOutput(paste0("plot1_", meth),
                            brush = paste0("plot_brush_", meth),
                            height = 500),
                 plotOutput(paste0("plot2_", meth)),
                 verbatimTextOutput(paste0("info_", meth)))
        })
      )
    ),

    server <- function(input, output) {
      lapply(unique_nams, function(meth) {
        output[[paste0("plot1_", meth)]] <- renderPlot({
          if (!is.null(grouping)) {
            plot_emb(l_embs[[meth]], col = grouping, ...) +
              ggtitle(meth)
          } else {
            plot_emb(l_embs[[meth]], ...) +
              ggtitle(meth)
          }
        })

        output[[paste0("info_", meth)]] <- renderPrint({
          pts <- extract_points(l_embs[[meth]])
          dat <- data.frame(dim1 = pts[, 1],
                            dim2 = pts[, 2],
                            label = as.factor(1:nrow(pts)))

          ids <- brushedPoints(dat, input[[paste0("plot_brush_", meth)]])
          as.integer(ids$label)
        })

        output[[paste0("plot2_", meth)]] <- renderPlot({
          pts <- extract_points(l_embs[[meth]])
          dat <- data.frame(dim1 = pts[, 1],
                            dim2 = pts[, 2],
                            label = as.factor(1:nrow(pts)))


          ids <- brushedPoints(dat, input[[paste0("plot_brush_", meth)]])
          ind <- as.integer(ids$label)

          global_mean <- apply(funs, 2, mean)
          temp_mean <- apply(funs[ind, , drop = FALSE], 2, mean)

          p_temp_mean <-
            geom_line(
              data = data.frame(grid = grid, mean_fun = temp_mean),
              aes(x = grid, y = mean_fun),
              color = "black",
              linetype = 2,
              size = 0.75)
          p_gobal_mean <-
            geom_line(
              data = data.frame(grid = grid, mean_fun = global_mean),
              aes(x = grid, y = mean_fun),
              linetype = 3,
              color = "black",
              size = 0.5
            )

          if (length(ids$dim1) == 0) {
            ggplot() + p_gobal_mean
          } else {
            if (!is.null(grouping)) {
              p <-
                plot_funs(funs[ind, , drop = FALSE], args = grid, col = grouping[ind]) +
                p_temp_mean +
                p_gobal_mean
            } else {
              p <-
                plot_funs(funs[ind, , drop = FALSE], args = grid) +
                p_temp_mean +
                p_gobal_mean
            }
            p
          }
        })
      })
    }
  )
}


