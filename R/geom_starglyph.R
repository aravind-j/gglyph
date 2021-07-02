#' Add Star Glyphs as a Scatterplot
#'
#' The starglyph geom is used to plot multivariate data as star glyphs
#' \insertCite{siegel_surgical_1972,chambers_graphical_1983,dutoit_graphical_1986}{gglyph}
#' in a scatterplot.
#'
#' @template draw.grid-arg
#' @template full-arg
#' @inheritParams ggplot2::layer
#' @inheritParams starglyphGrob
#' @param cols Name of columns specifying the variables to be plotted in the
#'   glyphs as a character vector.
#' @param colour.whisker The colour of whiskers.
#' @param colour.contour The colour of contours.
#' @param colour.points The colour of grid points.
#' @param linewidth.whisker The whisker line width.
#' @param linewidth.contour The contour line width.
#' @param point.size The size of the grid points in native units.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}()}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{colour = "green"} or \code{size = 3}. They may also be parameters to
#'   the paired geom/stat.
#'
#' @section Aesthetics: \code{geom_starglyph()} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item{\strong{x}}
#'   \item{\strong{y}} \item{alpha} \item{colour} \item{fill} \item{group}
#'   \item{shape} \item{size} \item{stroke} \item{linetype} }
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
#' @seealso \code{\link[gglyph]{starglyphGrob}}
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
#' # Both whiskers and contour
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 10, alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 10, alpha =  0.5, full = FALSE) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 10, alpha =  0.5,
#'                  linewidth.whisker = 3, linewidth.contour = 0.1) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 10, alpha =  0.5,
#'                  linewidth.whisker = 1, linewidth.contour = 3) +
#'   ylim(c(-0, 550))
#'
#'
#' # Only contours (polygon)
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = FALSE, contour = TRUE,
#'                  size = 10, alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = FALSE, contour = TRUE,
#'                  size = 10, alpha =  0.5, linewidth.contour = 3) +
#'   ylim(c(-0, 550))
#'
#' # Only whiskers
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 10) +
#'   geom_point(data = mtcars, aes(x = mpg, y = disp, colour = cyl)) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 10, full = FALSE) +
#'   geom_point(data = mtcars, aes(x = mpg, y = disp, colour = cyl)) +
#'   ylim(c(-0, 550))
#'
#' # Whiskers with colours
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 10,
#'                  colour.whisker = RColorBrewer::brewer.pal(8, "Dark2")) +
#'   geom_point(data = mtcars, aes(x = mpg, y = disp)) +
#'   ylim(c(-0, 550))
#'
#' # With text annotations
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 10) +
#'   geom_point(data = mtcars, aes(x = mpg, y = disp, colour = cyl)) +
#'   geom_text(data = mtcars, aes(x = mpg, y = disp, label = lab), cex = 2) +
#'   ylim(c(-0, 550))
#'
#' # Faceted
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 10, alpha =  0.5) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 10) +
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
#' # Grid points
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 3, alpha =  0.5, draw.grid = TRUE,
#'                  point.size = 5) +
#'   ylim(c(-0, 550))
#'
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 3, draw.grid = TRUE, point.size = 7,
#'                  linewidth.whisker = 2, alpha = 0.7) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 3, draw.grid = TRUE,
#'                  point.size = 5, alpha =  0.8,
#'                  colour.whisker = RColorBrewer::brewer.pal(8, "Dark2")) +
#'   geom_point(data = mtcars, aes(x = mpg, y = disp)) +
#'   ylim(c(-0, 550))
#'
geom_starglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ...,
                           cols = character(0L),
                           whisker = TRUE,
                           contour = TRUE,
                           colour.whisker = NULL,
                           colour.contour = NULL,
                           colour.points = NULL,
                           linewidth.whisker = 1,
                           linewidth.contour = 1,
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
    whisker = whisker,
    contour = contour,
    linewidth.whisker = linewidth.whisker,
    linewidth.contour = linewidth.contour,
    colour.whisker = colour.whisker,
    colour.contour = colour.contour,
    colour.points = colour.points,
    full = full,
    draw.grid = draw.grid,
    point.size = point.size,
    cols = cols, ...)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStarGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params)

}

GeomStarGlyph <- ggplot2::ggproto("GeomStarGlyph", ggplot2::Geom,
                                  required_aes = c("x", "y"),
                                  default_aes = ggplot2::aes(colour = "black",
                                                             size = 1,
                                                             shape = 19,
                                                             fill = NA,
                                                             stroke = 0.5,
                                                             linetype = 1,
                                                             alpha = 1,
                                                             linejoin = "mitre",
                                                             lineend = "round"),

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

                                    # Check if col.whisker are valid
                                    if (!is.null(params$colour.whisker)) {
                                      if (length(params$colour.whisker) != length(cols))
                                        stop('The number of colours specified in',
                                             '"colour.whisker" are not equal to the number',
                                             'of variables specified in "cols".')

                                      if (!all(iscolour(params$colour.whisker))) {
                                        stop('Invalid colour(s) specified in "colour.whisker".')
                                      }

                                      data$colour <- NULL
                                    }

                                    # browser()
                                    #
                                    # if (params$draw.grid) {
                                    #   data$point.size <- params$point.size
                                    # } else {
                                    #   data$point.size <- NA
                                    # }

                                    data$linewidth.whisker <- params$linewidth.whisker
                                    data$linewidth.contour <- params$linewidth.contour
                                    data
                                  },

                                  draw_panel = function(data, panel_params,
                                                        coord, cols,
                                                        whisker, contour,
                                                        linewidth.whisker,
                                                        linewidth.contour,
                                                        colour.whisker,
                                                        colour.contour,
                                                        colour.points,
                                                        full,
                                                        draw.grid,
                                                        point.size) {

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

                                    for (i in seq_along(data$x)) {
                                      # addGrob to get proper overlappin of glyphs
                                      gl <- grid::addGrob(gl,
                                                          starglyphGrob(x = data$x[i],
                                                                        y = data$y[i],
                                                                        z = unlist(data[i, cols]),
                                                                        size = data$size[i],
                                                                        col.whisker = if (is.null(colour.whisker)) {
                                                                          data$colour[i]
                                                                        } else {
                                                                          colour.whisker
                                                                        },
                                                                        col.contour = if (is.null(colour.contour)) {
                                                                          data$colour[i]
                                                                        } else {
                                                                          colour.contour
                                                                        },
                                                                        fill = data$fill[i],
                                                                        lwd.whisker = data$linewidth.whisker[i],
                                                                        lwd.contour = data$linewidth.contour[i],
                                                                        alpha = data$alpha[i],
                                                                        angle.start = astrt,
                                                                        angle.stop = astp,
                                                                        whisker = whisker,
                                                                        contour = contour,
                                                                        linejoin = data$linejoin[i],
                                                                        lineend = data$lineend[i],
                                                                        grid.levels = grid.levels,
                                                                        draw.grid = draw.grid,
                                                                        point.size = grid::unit(point.size, "pt"),
                                                                        col.points = if (is.null(colour.points)) {
                                                                          if (is.null(colour.whisker)) {
                                                                            data$colour[i]
                                                                          } else {
                                                                            NA
                                                                          }
                                                                        } else {
                                                                          colour.points
                                                                        }))
                                    }

                                    ggname("geom_starglyph",
                                           gl)

                                    # ggname("geom_starglyph",
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
