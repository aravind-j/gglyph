#' Add Dot Profile Glyphs as a Scatterplot
#'
#' The dotglyph geom is used to plot multivariate data as dot profile glyphs
#' \insertCite{chambers_graphical_1983,dutoit_graphical_1986}{gglyph} in a
#' scatterplot.
#'
#' @template fill.gradient-arg
#' @inheritParams ggplot2::layer
#' @inheritParams starglyphGrob
#' @inheritParams dotglyphGrob
#' @param cols Name of columns specifying the variables to be plotted in the
#'   glyphs as a character vector.
#' @param fill.dot The fill colour of the stacked dots.
#' @param linewidth The line width of the dot glyphs.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}()}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{colour = "green"} or \code{size = 3}. They may also be parameters to
#'   the paired geom/stat.
#'
#' @section Aesthetics: \code{geom_dotglyph()} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item{\strong{x}}
#'   \item{\strong{y}} \item{alpha} \item{colour} \item{fill} \item{group} }
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
#' @seealso \code{\link[gglyph]{dotglyphGrob}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#'
#' # Convert data to classes
#' zs <- c("hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
#'
#' mtcars[ , zs] <- lapply(mtcars[, zs],
#'                         function(x) cut(x, breaks = 5,
#'                                         labels = c(1, 2, 3, 4, 5)))
#' mtcars[ , zs] <- lapply(mtcars[ , zs], as.factor)
#'
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$lab <- row.names(mtcars)
#'
#' library(ggplot2)
#' theme_set(theme_bw())
#' options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(8, "Dark2"))
#' options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(8, "Dark2"))
#'
#' # Mapped fill
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, radius = 0.5,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, radius = 0.5,
#'                 mirror = FALSE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, radius = 0.5,
#'                 flip.axes = TRUE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, radius = 0.5,
#'                 mirror = FALSE, flip.axes = TRUE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped colour
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, radius = 0.5,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, radius = 0.5,
#'                 mirror = FALSE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, radius = 0.5,
#'                 flip.axes = TRUE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, radius = 0.5,
#'                 mirror = FALSE, flip.axes = TRUE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Different fill colours
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 fill.dot = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 mirror = FALSE,
#'                 fill.dot = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 flip.axes = TRUE,
#'                 fill.dot = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 mirror = FALSE, flip.axes = TRUE,
#'                 fill.dot = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Gradient fill
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 fill.gradient = "Greens",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 fill.gradient = "Blues",
#'                 mirror = FALSE,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 flip.axes = TRUE,
#'                 fill.gradient = "RdYlBu",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 mirror = FALSE, flip.axes = TRUE,
#'                 fill.gradient = "viridis",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Faceted
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, fill = cyl),
#'                 cols = zs, radius = 0.5,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp, colour = cyl),
#'                 cols = zs, radius = 0.5,
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 fill.dot = RColorBrewer::brewer.pal(8, "Dark2"),
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_dotglyph(aes(x = mpg, y = disp),
#'                 cols = zs, radius = 0.5,
#'                 fill.gradient = "viridis",
#'                 alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
geom_dotglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ...,
                          cols = character(0L),
                          radius = 1,
                          fill.dot = NULL,
                          fill.gradient = NULL,
                          linewidth = 1,
                          mirror = TRUE,
                          flip.axes = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  # Modify mapping to include cols
  mcols <- rlang::as_quosures(rlang::syms(cols), .GlobalEnv)
  names(mcols) <- cols
  mapping <- modifyList(mapping, mcols)

  params <- list(
    radius = radius,
    flip.axes = flip.axes,
    mirror = mirror,
    fill.dot = fill.dot,
    fill.gradient = fill.gradient,
    linewidth = linewidth,
    cols = cols, ...)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDotGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params)

}

GeomDotGlyph <- ggplot2::ggproto("GeomDotGlyph", ggplot2::Geom,
                                 required_aes = c("x", "y"),
                                 default_aes = ggplot2::aes(colour = "black",
                                                            fill = NA,
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

                                   # Remove rows with missing values in "cols"
                                   # check for missing values
                                   missvcols <- unlist(lapply(data[, cols], function(x) TRUE %in% is.na(x)))
                                   if (TRUE %in% missvcols) {
                                     warning(paste('The following column(s) in "data" have missing values:\n',
                                                   paste(names(missvcols[missvcols]), collapse = ", ")))

                                     data <- remove_missing(df = data, vars = cols)
                                   }

                                   # Check if fill.dot are valid
                                   if (!is.null(params$fill.dot)) {
                                     if (length(params$fill.dot) != length(cols))
                                       stop('The number of colours specified in',
                                            '"fill.dot" are not equal to the number',
                                            'of variables specified in "cols".')

                                     if (!all(iscolour(params$fill.dot))) {
                                       stop('Invalid colour(s) specified in "fill.dot".')
                                     }
                                     data$colour <- NULL
                                   }

                                   data$linewidth <- params$linewidth
                                   data
                                 },

                                 draw_panel = function(data, panel_params,
                                                       coord, cols,
                                                       radius,
                                                       fill.dot,
                                                       fill.gradient,
                                                       flip.axes,
                                                       linewidth,
                                                       mirror) {

                                   data <- coord$transform(data, panel_params)

                                   # Convert factor columns to equivalent numeric
                                   fcols <- names(Filter(is.factor, data[, cols]))

                                   if (length(fcols) > 0)  {
                                     data[, fcols] <- lapply(data[, cols], function(f) as.numeric(levels(f))[f])
                                   }

                                   # Gradient colour mapping
                                   if (is.null(fill.dot) & !is.null(fill.gradient)) {
                                     gdata <- data[, cols]

                                     gdata <- lapply(gdata,
                                                     function(x) scales::col_numeric(palette = fill.gradient,
                                                                                     domain = min(x):max(x))(x))
                                     gdata <- data.frame(gdata)
                                   }

                                   gl <- lapply(seq_along(data$x),
                                                function(i) dotglyphGrob(x = data$x[i],
                                                                         y = data$y[i],
                                                                         z = unlist(data[i, cols]),
                                                                         radius = radius,
                                                                         mirror = mirror,
                                                                         flip.axes = flip.axes,
                                                                         fill = if (is.null(fill.dot)) {
                                                                           if (!is.null(fill.gradient)) {
                                                                             unlist(mapply(function(a, b) rep(a, b),
                                                                                           unlist(gdata[i, ]),
                                                                                           round(unlist(data[i, cols]))))
                                                                           } else {
                                                                             data$fill[i]
                                                                           }
                                                                         } else {
                                                                           unlist(mapply(function(a, b) rep(a, b),
                                                                                         fill.dot,
                                                                                         round(unlist(data[i, cols]))))
                                                                         },
                                                                         col = data$colour[i],
                                                                         lwd = data$linewidth[i],
                                                                         alpha = data$alpha[i]))

                                   gl <- do.call(grid::gList, gl)

                                   glout <- grid::grobTree()

                                   glout <- grid::setChildren(glout, gl)

                                   ggname("geom_starglyph",
                                          glout)

                                   # ggname("geom_dotglyph",
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
