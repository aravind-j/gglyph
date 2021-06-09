#' Add Star Glyphs as a Scatterplot
#'
#' The starglyph geom is used to plot multivariate data as star glyphs
#' \insertCite{siegel_surgical_1972,chambers_graphical_1983}{ggglyph} in a
#' scatterplot.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams starglyphGrob
#' @param cols Name of columns specifying the variables to be plotted in the
#'   glyphs as a character vector.
#' @param linewidth The line width.
#' @param ... additional parameters
#'
#' @section Aesthetics: \code{geom_starglyph()} understands the following aesthetics
#'   (required aesthetics are in bold): \itemize{ \item{\strong{x}}
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
#' @seealso \code{\link[ggglyph]{starglyphGrob}}
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
#'
#' # Both whiskers and contour
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 0.1, alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
#' # Only contours (polygon)
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = FALSE, contour = TRUE,
#'                  size = 0.1, alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
#' # Only whiskers
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 0.1) +
#'   geom_point(data = mtcars, aes(x = mpg, y = disp, colour = cyl)) +
#'   ylim(c(-0, 550))
#'
#' # With text annotations
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
#'                  cols = zs, whisker = TRUE, contour = FALSE,
#'                  size = 0.1) +
#'   geom_point(data = mtcars, aes(x = mpg, y = disp, colour = cyl)) +
#'   geom_text(data = mtcars, aes(x = mpg, y = disp, label = lab), cex = 2) +
#'   ylim(c(-0, 550))
#'
#' # Faceted
#' ggplot(data = mtcars) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 0.1, alpha =  0.5) +
#'   ylim(c(-0, 550)) +
#'   facet_grid(. ~ cyl)
#'
geom_starglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ...,
                           cols = character(0L),
                           whisker = TRUE,
                           contour = TRUE,
                           linewidth = 1,
                           show.legend = NA, inherit.aes = TRUE) {


  # Check if cols are numeric or factor

  # Remove rows with missing values in "cols"

  mcols <- rlang::as_quosures(rlang::syms(cols), .GlobalEnv)
  names(mcols) <- cols
  mapping <- modifyList(mapping, mcols)

  params <- list(
    whisker = whisker,
    contour = contour,
    linewidth = linewidth,
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
                                                 paste(cols[!(cols %in% colnames(data))], collapse = ", "),
                                                 sep = ""))
                                    }

                                    data$linewidth <- params$linewidth
                                    data
                                  },

                                  draw_panel = function(data, panel_params,
                                                        coord, cols,
                                                        whisker, contour,
                                                        linewidth) {

                                    data <- coord$transform(data, panel_params)

                                    gl <- grid::grobTree()

                                    for (i in seq_along(data$x)) {
                                      # addGrob to get proper overlappin of glyphs
                                      gl <- grid::addGrob(gl,
                                                          starglyphGrob(x = data$x[i],
                                                                        y = data$y[i],
                                                                        z = unlist(data[i, cols]),
                                                                        size = data$size[i],
                                                                        col = data$colour[i],
                                                                        fill = data$fill[i],
                                                                        lwd = data$linewidth[i],
                                                                        alpha = data$alpha[i],
                                                                        angle.start = 0,
                                                                        angle.stop = 2*base::pi,
                                                                        whisker = whisker,
                                                                        contour = contour))
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
