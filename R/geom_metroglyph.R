#' Add Metroglyphs as a Scatterplot
#'
#' The metroglyph geom is used to plot multivariate data as metroglyphs
#' \insertCite{anderson_semigraphical_1957,dutoit_graphical_1986}{gglyph}
#' in a scatterplot.
#'
#' @template draw.grid-arg
#' @template full-arg
#' @inheritParams ggplot2::layer
#' @inheritParams metroglyphGrob
#' @param cols Name of columns specifying the variables to be plotted in the
#'   glyphs as a character vector.
#' @param colour.ray The colour of rays.
#' @param colour.circle The colour of circles.
#' @param colour.points The colour of grid points.
#' @param linewidth.ray The ray line width.
#' @param linewidth.circle The circle line width.
#' @param point.size The size of the grid points in native units.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}()}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{colour = "green"} or \code{size = 3}. They may also be parameters to
#'   the paired geom/stat.
#'
#' @section Aesthetics: \code{geom_metroglyph()} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item{\strong{x}}
#'   \item{\strong{y}} \item{alpha} \item{colour} \item{fill} \item{group}
#'   \item{circle.size} }
#'
#' @family geoms
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
#' @seealso \code{\link[gglyph]{metroglyphGrob}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' # Scale the data
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
#' # Mapped colour
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2, fill = "gray30",
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   full = FALSE,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   full = FALSE,
#'                   linewidth.circle = 2, linewidth.ray = 2, fill = "gray30",
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped colour + fill
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   full = FALSE,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped fill
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   colour.circle = "transparent",
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   full = FALSE,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   full = FALSE, colour.circle = "transparent",
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Rays with colours
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp),
#'                   cols = zs, circle.size = 3,
#'                   linewidth.circle = 0, linewidth.ray = 2,
#'                   colour.circle = "transparent", fill = "gray",
#'                   colour.ray = RColorBrewer::brewer.pal(8, "Dark2"),
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp),
#'                   cols = zs, circle.size = 3,
#'                   linewidth.circle = 0, linewidth.ray = 2,
#'                   colour.circle = "transparent", fill = "gray",
#'                   colour.ray = RColorBrewer::brewer.pal(8, "Dark2"),
#'                   size = 10, alpha =  0.8, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' # Faceted
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 10, alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
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
#' # Grid points
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, colour = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 2.5, alpha =  0.8,
#'                   draw.grid = TRUE, point.size = 5) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp, fill = cyl),
#'                   cols = zs, circle.size = 3, colour.ray = NULL,
#'                   linewidth.circle = 2, linewidth.ray = 2,
#'                   size = 2.5, alpha =  0.8,
#'                   draw.grid = TRUE, point.size = 5) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_metroglyph(aes(x = mpg, y = disp),
#'                   cols = zs, circle.size = 3,
#'                   linewidth.circle = 0, linewidth.ray = 2,
#'                   colour.circle = "transparent", fill = "gray",
#'                   colour.ray = RColorBrewer::brewer.pal(8, "Dark2"),
#'                   size = 2.5, alpha =  0.8,
#'                   draw.grid = TRUE, point.size = 5) +
#'   ylim(c(-0, 550))
#'
geom_metroglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ...,
                            cols = character(0L),
                            circle.size = 1,
                            colour.circle = NULL,
                            colour.ray = NULL,
                            colour.points = NULL,
                            linewidth.circle = 1,
                            linewidth.ray = 1,
                            lineend = "butt",
                            full = TRUE,
                            draw.grid = FALSE,
                            point.size = 1,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  # Modify mapping to include cols
  mcols <- rlang::as_quosures(rlang::syms(cols), .GlobalEnv)
  names(mcols) <- cols
  mapping <- modifyList(mapping, mcols)

  params <- list(
    circle.size = circle.size,
    linewidth.circle = linewidth.circle,
    linewidth.ray = linewidth.ray,
    colour.circle = colour.circle,
    colour.ray = colour.ray,
    colour.points = colour.points,
    full = full,
    lineend = lineend,
    draw.grid = draw.grid,
    point.size = point.size,
    cols = cols, ...)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMetroGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params)

}

GeomMetroGlyph <- ggplot2::ggproto("GeomMetroGlyph", ggplot2::Geom,
                                   required_aes = c("x", "y"),
                                   default_aes = ggplot2::aes(colour = "black",
                                                              size = 1,
                                                              circle.size = 1,
                                                              fill = NA,
                                                              # linetype = 1,
                                                              alpha = 1),

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

                                     # Check if col.ray are valid
                                     if (!is.null(params$col.ray)) {
                                       if (length(params$col.ray) != length(cols))
                                         stop('The number of colours specified in',
                                              '"col.ray" are not equal to the number',
                                              'of variables specified in "cols".')

                                       if (!all(iscolour(params$col.ray))) {
                                         stop('Invalid colour(s) specified in "col.ray".')
                                       }

                                       data$colour <- NULL
                                     }

                                     data$linewidth.ray <- params$linewidth.ray
                                     data$linewidth.circle <- params$linewidth.circle
                                     data$lineend <- params$lineend
                                     data
                                   },

                                   draw_panel = function(data, panel_params,
                                                         circle.size,
                                                         coord, cols,
                                                         linewidth.ray,
                                                         linewidth.circle,
                                                         colour.ray,
                                                         colour.circle,
                                                         colour.points,
                                                         full,
                                                         lineend,
                                                         draw.grid,
                                                         point.size) {

                                     data <- coord$transform(data, panel_params)

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

                                     gl <- lapply(seq_along(data$x),
                                                  function(i) metroglyphGrob(x = data$x[i],
                                                                             y = data$y[i],
                                                                             z = unlist(data[i, cols]),
                                                                             size = data$size[i],
                                                                             circle.size = data$circle.size[i],
                                                                             col.ray = if (is.null(colour.ray)) {
                                                                               data$colour[i]
                                                                             } else {
                                                                               colour.ray
                                                                             },
                                                                             col.circle = if (is.null(colour.circle)) {
                                                                               data$colour[i]
                                                                             } else {
                                                                               colour.circle
                                                                             },
                                                                             fill = data$fill[i],
                                                                             lwd.ray = data$linewidth.ray[i],
                                                                             lwd.circle = data$linewidth.circle[i],
                                                                             alpha = data$alpha[i],
                                                                             angle.start = astrt,
                                                                             angle.stop = astp,
                                                                             lineend = data$lineend[i],
                                                                             grid.levels = grid.levels,
                                                                             draw.grid = draw.grid,
                                                                             point.size = grid::unit(point.size, "pt"),
                                                                             col.points = if (is.null(colour.points)) {
                                                                               if (is.null(colour.ray)) {
                                                                                 data$colour[i]
                                                                               } else {
                                                                                 NA
                                                                               }
                                                                             } else {
                                                                               colour.points
                                                                             }))

                                     gl <- do.call(grid::gList, gl)

                                     glout <- grid::grobTree()

                                     glout <- grid::setChildren(glout, gl)

                                     ggname("geom_starglyph",
                                            glout)

                                     # ggname("geom_metroglyph",
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
