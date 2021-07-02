#' Add Pie Glyphs as a Scatterplot
#'
#' The pieglyph geom is used to plot multivariate data as pie glyphs
#' \insertCite{ward_visualization_2000,fuchs_evaluation_2013}{gglyph} in a
#' scatterplot.
#'
#' @template draw.grid-arg
#' @template full-arg
#' @template fill.gradient-arg
#' @template scale.segment.radius-arg
#' @inheritParams ggplot2::layer
#' @inheritParams starglyphGrob
#' @param cols Name of columns specifying the variables to be plotted in the
#'   glyphs as a character vector.
#' @param fill.segment The fill colour of the segments.
#' @param colour.grid The colour of grid lines.
#' @param linewidth The line width of the segments.
#' @param linewidth.grid The line width for the grid lines.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer()}}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{colour = "green"} or \code{size = 3}. They may also be parameters to
#'   the paired geom/stat.
#'
#' @section Aesthetics: \code{geom_pieglyph()} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item{\strong{x}}
#'   \item{\strong{y}} \item{alpha} \item{colour} \item{fill} \item{group}
#'   \item{shape} \item{size} \item{stroke} \item{linetype} }
#'
#' @return A \code{geom} layer.
#'
#' @importFrom rlang as_quosures syms
#' @importFrom utils modifyList
#' @importFrom ggplot2 layer ggproto aes
#' @importFrom grid grobTree addGrob
#' @importFrom Rdpack reprompt
#' @export
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link[gglyph]{pieglyphGrob}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
# Scale the data
#' zs <- c("hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
#' mtcars[ , zs] <- lapply(mtcars[ , zs], scales::rescale)
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$lab <- row.names(mtcars)
#'
#' library(ggplot2)
#' theme_set(theme_bw())
#' options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(8, "Dark2"))
#' options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(8, "Dark2"))
#'
#' # Mapped fill + scaled radius
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, size = 10,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, size = 10,
#'                 alpha =  0.8, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' # Mapped fill + scaled segment
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 alpha =  0.8, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' # Mapped colour + scaled radius
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 10,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 10, fill = "white",
#'                 alpha =  0.8,  linewidth = 2) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 10,
#'                 alpha =  0.8, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 10, fill = "white",
#'                 alpha =  0.8,  linewidth = 2, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' # Mapped colour + scaled segment
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 5, fill = "white",
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 alpha =  0.8, linewidth = 2) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 alpha =  0.8, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 5, fill = "white",
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 alpha =  0.8, linewidth = 2, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' # Segments with colours + scaled radius
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 10,
#'                 fill.segment = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 10,
#'                 fill.segment = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' # Segments with colours + scaled segment (scatterpie)
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 fill.segment = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = TRUE,
#'                 fill.segment = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' # Gradient fill
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = FALSE,
#'                 fill.gradient = "Greens",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = FALSE,
#'                 fill.gradient = "Blues",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = FALSE,
#'                 fill.gradient = "RdYlBu",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 5,
#'                 scale.radius = FALSE, scale.segment = FALSE,
#'                 fill.gradient = "viridis",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Faceted
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, size = 10,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 10,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 10,
#'                 fill.segment = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' rm(mtcars)
#' mtcars[ , zs] <- lapply(mtcars[ , zs], scales::rescale)
#'
#' mtcars[ , zs] <- lapply(mtcars[, zs],
#'                         function(x) cut(x, breaks = 3,
#'                                         labels = c(1, 2, 3)))
#' mtcars[ , zs] <- lapply(mtcars[ , zs], as.factor)
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$lab <- row.names(mtcars)
#'
#' # Grid lines (when scale.radius = TRUE)
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, size = 2,
#'                 alpha =  0.8, draw.grid = TRUE) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, size = 2,
#'                 alpha =  0.8, draw.grid = TRUE) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_pieglyph(aes(x = mpg, y = disp),
#'                 cols = zs, size = 2,
#'                 scale.radius = TRUE, scale.segment = FALSE,
#'                 fill.gradient = "Blues",
#'                 alpha =  0.8, draw.grid = TRUE) +
#'   ylim(c(-0, 550))
#'
geom_pieglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ...,
                          cols = character(0L),
                          edges = 200,
                          fill.segment = NULL,
                          fill.gradient = NULL,
                          colour.grid = NULL,
                          linewidth = 1,
                          linewidth.grid = linewidth,
                          scale.segment = FALSE,
                          scale.radius = TRUE,
                          full = TRUE,
                          draw.grid = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  # Modify mapping to include cols
  mcols <- rlang::as_quosures(rlang::syms(cols), .GlobalEnv)
  names(mcols) <- cols
  mapping <- modifyList(mapping, mcols)

  params <- list(
    edges = edges,
    colour.grid = colour.grid,
    fill.segment = fill.segment,
    linewidth = linewidth,
    linewidth.grid = linewidth.grid,
    scale.segment = scale.segment,
    scale.radius = scale.radius,
    full = full,
    fill.gradient = fill.gradient,
    draw.grid = draw.grid,
    cols = cols, ...)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPieGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params)

}

GeomPieGlyph <- ggplot2::ggproto("GeomPieGlyph", ggplot2::Geom,
                                 required_aes = c("x", "y"),
                                 default_aes = ggplot2::aes(colour = "black",
                                                            size = 1,
                                                            shape = 19,
                                                            fill = NA,
                                                            stroke = 0.5,
                                                            linetype = 1,
                                                            alpha = 1,
                                                            linejoin = "mitre"),

                                 draw_key = ggplot2::draw_key_polygon,

                                 setup_params = function(data, params) {

                                   params
                                 },

                                 setup_data = function(data, params) {

                                   cols <- params$cols

                                   # Check if "cols" exist in data
                                   if (FALSE %in% (cols %in% colnames(data))) {
                                     stop(paste('The following column(s) specified as "cols" are not present in "data":\n',
                                                paste(cols[!(cols %in% colnames(data))],
                                                      collapse = ", "),
                                                sep = ""))
                                   }


                                   # Check if cols are numeric or factor
                                   intfactcols <- unlist(lapply(data[, cols],
                                                                function(x) FALSE %in% (is.vector(x, mode = "integer") |
                                                                                          is.vector(x, mode = "numeric") |
                                                                                          is.factor(x))))
                                   if (TRUE %in% intfactcols) {
                                     stop('The following column(s) specified as "cols" in ',
                                          '"data" are not of type numeric, integer or factor:\n',
                                          paste(names(intfactcols[intfactcols]), collapse = ", "))
                                   }

                                   draw.grid <- params$draw.grid

                                   if (draw.grid &
                                       !all(unlist(lapply(data[, cols], is.factor)))) {
                                     draw.grid <- FALSE
                                     warning('All the columns specified as "cols" in ',
                                             '"data" are not of type factor.\n',
                                             'Unable to plot grid points.')
                                   }

                                   # Remove rows with missing values in "cols"
                                   # check for missing values
                                   missvcols <- unlist(lapply(data[, cols], function(x) TRUE %in% is.na(x)))
                                   if (TRUE %in% missvcols) {
                                     warning(paste('The following column(s) in "data" have missing values:\n',
                                                   paste(names(missvcols[missvcols]), collapse = ", ")))

                                     data <- remove_missing(df = data, vars = cols)
                                   }

                                   # Check if fill.segment are valid
                                   if (!is.null(params$fill.segment)) {
                                     if (length(params$fill.segment) != length(cols))
                                       stop('The number of colours specified in',
                                            '"fill.segment" are not equal to the number',
                                            'of variables specified in "cols".')

                                     if (!all(iscolour(params$fill.segment))) {
                                       stop('Invalid colour(s) specified in "fill.segment".')
                                     }

                                     data$fill <- NULL
                                   }

                                   # browser()
                                   #
                                   # if (params$draw.grid) {
                                   #   data$point.size <- params$point.size
                                   # } else {
                                   #   data$point.size <- NA
                                   # }

                                   data$linewidth <- params$linewidth
                                   data$linewidth.grid <- params$linewidth.grid
                                   data

                                 },

                                 draw_panel = function(data, panel_params,
                                                       coord, cols,
                                                       edges,
                                                       scale.segment,
                                                       scale.radius,
                                                       linewidth,
                                                       linewidth.grid,
                                                       fill.segment,
                                                       colour.grid,
                                                       full,
                                                       fill.gradient,
                                                       draw.grid) {

                                   data <- coord$transform(data, panel_params)

                                   gl <- grid::grobTree()

                                   if (full) {
                                     astrt <- 0
                                     astp <- 2 * base::pi
                                   } else {
                                     astrt <- 0
                                     astp <- base::pi
                                   }

                                   grid.levels <- NULL

                                   # Convert factor columns to equivalent numeric
                                   if (draw.grid) {
                                     grid.levels <- lapply(data[, cols], function(a) as.integer(levels(a)))
                                   }

                                   fcols <- names(Filter(is.factor, data[, cols]))

                                   if (length(fcols) > 0)  {
                                     data[, fcols] <- lapply(data[, cols], function(f) as.numeric(levels(f))[f])
                                   }

                                   # Gradient colour mapping
                                   if (is.null(fill.segment) & !is.null(fill.gradient)) {
                                     gdata <- data[, cols]

                                     gdata <- lapply(gdata,
                                                     function(x) scales::col_numeric(palette = fill.gradient,
                                                                                     domain = min(x):max(x))(x))
                                     gdata <- data.frame(gdata)
                                   }

                                   for (i in seq_along(data$x)) {
                                     # addGrob to get proper overlappin of glyphs
                                     gl <- grid::addGrob(gl,
                                                         pieglyphGrob(x = data$x[i],
                                                                      y = data$y[i],
                                                                      z = unlist(data[i, cols]),
                                                                      size = data$size[i],
                                                                      edges = edges,
                                                                      col = data$colour[i],
                                                                      fill = if (is.null(fill.segment)) {
                                                                        if (!is.null(fill.gradient)) {
                                                                          unlist(gdata[i, ])
                                                                        } else {
                                                                          data$fill[i]
                                                                        }
                                                                      } else {
                                                                        fill.segment
                                                                      },
                                                                      lwd = data$linewidth[i],
                                                                      lwd.grid = data$linewidth.grid[i],
                                                                      alpha = data$alpha[i],
                                                                      angle.start = astrt,
                                                                      angle.stop = astp,
                                                                      scale.segment = scale.segment,
                                                                      scale.radius = scale.radius,
                                                                      linejoin = data$linejoin[i],
                                                                      grid.levels = grid.levels,
                                                                      draw.grid = draw.grid,
                                                                      col.grid = if (is.null(colour.grid)) {
                                                                        data$colour[i]
                                                                      } else {
                                                                        colour.grid
                                                                      }))
                                   }

                                   ggname("geom_pieglyph",
                                          gl)

                                   # ggname("geom_pieglyph",
                                   #        grid::gTree(
                                   #          children = grid::gList(
                                   #            grid::pointsGrob(x = data$x,
                                   #                             y = data$y,
                                   #                             default.units = "native",
                                   #                             pch = 20,
                                   #                             gp = grid::gpar(col = data$colour,
                                   #                                             fill = data$fill))
                                   #          )))
                                 }
)
