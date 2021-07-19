#' Add Profile Glyphs as a Scatterplot
#'
#' The profileglyph geom is used to plot multivariate data as profile glyphs
#' \insertCite{chambers_graphical_1983,dutoit_graphical_1986}{gglyph} in a
#' scatterplot.
#'
#' @template draw.grid-arg
#' @template fill.gradient-arg
#' @inheritParams ggplot2::layer
#' @inheritParams starglyphGrob
#' @inheritParams profileglyphGrob
#' @param cols Name of columns specifying the variables to be plotted in the
#'   glyphs as a character vector.
#' @param colour.bar The colour of bars.
#' @param colour.line The colour of profile line(s).
#' @param colour.grid The colour of the grid lines.
#' @param linewidth.line The line width of the profile line(s)
#' @param linewidth.bar The line width of the bars.
#' @param linewidth.grid The line width of the grid lines.
#' @param fill.bar The fill colour of the bars.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}()}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{colour = "green"} or \code{size = 3}. They may also be parameters to
#'   the paired geom/stat.
#'
#' @section Aesthetics: \code{geom_pieglyph()} understands the following
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
#' @seealso \code{\link[gglyph]{profileglyphGrob}}
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
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped fill + line
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE, mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE, mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped fill + bar
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE, mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE, mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped colour + bar and line
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped colour + line
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE, mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE, mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Mapped colour + bar
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE, mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     bar = FALSE, mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Bars with different fill + bar and line
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Bars with different fill + bar
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Bars with gradient fill + bar and line
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.gradient = "Greens",
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.gradient = "Blues",
#'                     mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.gradient = "RdYlBu",
#'                     flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.gradient = "viridis",
#'                     mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Bars with gradient fill + bar
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.gradient = "Greens",
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.gradient = "Blues",
#'                     mirror = FALSE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.gradient = "RdYlBu",
#'                     flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     line = FALSE,
#'                     fill.gradient = "viridis",
#'                     mirror = FALSE, flip.axes = TRUE,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550))
#'
#' # Faceted
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, colour = cyl),
#'                     cols = zs, size = 5, width = 1,
#'                     alpha =  0.8) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp),
#'                     cols = zs, size = 5, width = 1,
#'                     fill.bar = RColorBrewer::brewer.pal(8, "Dark2"),
#'                     alpha =  0.8) +
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
#' # Grid lines (when bar = TRUE)
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 3, width = 1,
#'                     alpha =  0.8, draw.grid = TRUE) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, col = cyl),
#'                     cols = zs, size = 3, width = 1,
#'                     alpha =  0.8, draw.grid = TRUE) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_profileglyph(aes(x = mpg, y = disp, fill = cyl),
#'                     cols = zs, size = 3, width = 1,
#'                     fill.gradient = "Blues",
#'                     alpha =  0.8, draw.grid = TRUE) +
#'   ylim(c(-0, 550))
#'
geom_profileglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", ...,
                              cols = character(0L),
                              width = 10,
                              size = 1,
                              colour.bar = NULL,
                              colour.line = NULL,
                              colour.grid = NULL,
                              linewidth.line = 1,
                              linewidth.bar = 1,
                              linewidth.grid = 1,
                              fill.bar = NULL,
                              fill.gradient = NULL,
                              flip.axes = FALSE,
                              bar = TRUE,
                              line = TRUE,
                              mirror = TRUE,
                              draw.grid = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  # Modify mapping to include cols
  mcols <- rlang::as_quosures(rlang::syms(cols), .GlobalEnv)
  names(mcols) <- cols
  mapping <- modifyList(mapping, mcols)

  params <- list(
    width = width,
    size = size,
    colour.grid = colour.grid,
    colour.line = colour.line,
    colour.bar = colour.bar,
    linewidth.line = linewidth.line,
    linewidth.bar = linewidth.bar,
    linewidth.grid = linewidth.grid,
    fill.bar = fill.bar,
    fill.gradient = fill.gradient,
    flip.axes = flip.axes,
    bar = bar,
    line = line,
    mirror = mirror,
    draw.grid = draw.grid,
    cols = cols, ...)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomProfileGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params)

}

GeomProfileGlyph <- ggplot2::ggproto("GeomProfileGlyph", ggplot2::Geom,
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

                                       # Check if fill.bar are valid
                                       if (!is.null(params$fill.bar)) {
                                         if (length(params$fill.bar) != length(cols))
                                           stop('The number of colours specified in',
                                                '"fill.bar" are not equal to the number',
                                                'of variables specified in "cols".')

                                         if (!all(iscolour(params$fill.bar))) {
                                           stop('Invalid colour(s) specified in "fill.bar".')
                                         }

                                         data$colour <- NULL
                                       }

                                       data$linewidth.bar <- params$linewidth.bar
                                       data$linewidth.line <- params$linewidth.line
                                       data$linewidth.grid <- params$linewidth.grid
                                       data$linejoin <- "mitre"
                                       data$lineend <- "round"
                                       data$size <- params$size
                                       data
                                     },

                                     draw_panel = function(data, panel_params,
                                                           coord, cols,
                                                           width,
                                                           size,
                                                           colour.grid,
                                                           colour.line,
                                                           colour.bar,
                                                           linewidth.line,
                                                           linewidth.bar,
                                                           linewidth.grid,
                                                           fill.bar,
                                                           fill.gradient,
                                                           flip.axes,
                                                           bar, line,
                                                           mirror,
                                                           draw.grid) {

                                       data <- coord$transform(data, panel_params)

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
                                       if (is.null(fill.bar) & !is.null(fill.gradient)) {
                                         gdata <- data[, cols]

                                         gdata <- lapply(gdata,
                                                         function(x) scales::col_numeric(palette = fill.gradient,
                                                                                         domain = min(x):max(x))(x))
                                         gdata <- data.frame(gdata)
                                       }

                                       gl <- lapply(seq_along(data$x),
                                                    function(i) profileglyphGrob(x = data$x[i],
                                                                                 y = data$y[i],
                                                                                 z = unlist(data[i, cols]),
                                                                                 size = data$size[i],
                                                                                 width = width,
                                                                                 mirror = mirror,
                                                                                 flip.axes = flip.axes,
                                                                                 col.bar = if (is.null(colour.bar)) {
                                                                                   data$colour[i]
                                                                                 } else {
                                                                                   colour.bar
                                                                                 },
                                                                                 col.line = if (is.null(colour.line)) {
                                                                                   data$colour[i]
                                                                                 } else {
                                                                                   colour.line
                                                                                 },
                                                                                 fill = if (is.null(fill.bar)) {
                                                                                   if (!is.null(fill.gradient)) {
                                                                                     unlist(gdata[i, ])
                                                                                   } else {
                                                                                     data$fill[i]
                                                                                   }
                                                                                 } else {
                                                                                   fill.bar
                                                                                 },
                                                                                 lwd.bar = data$linewidth.bar[i],
                                                                                 lwd.line = data$linewidth.line[i],
                                                                                 lwd.grid = data$linewidth.grid[i],
                                                                                 alpha = data$alpha[i],
                                                                                 bar = bar,
                                                                                 line = line,
                                                                                 linejoin = data$linejoin[i],
                                                                                 lineend = data$lineend[i],
                                                                                 grid.levels = grid.levels,
                                                                                 draw.grid = draw.grid,
                                                                                 col.grid = if (is.null(colour.grid)) {
                                                                                   data$colour[i]
                                                                                 } else {
                                                                                   colour.grid
                                                                                 }))

                                       gl <- do.call(grid::gList, gl)

                                       glout <- grid::grobTree()

                                       glout <- grid::setChildren(glout, gl)

                                       ggname("geom_starglyph",
                                              glout)

                                       # ggname("geom_profileglyph",
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
