#' Add Tile Glyphs as a Scatterplot
#'
#' The tileglyph geom is used to plot multivariate data as tile glyphs similar
#' to 'autoglyph' \insertCite{beddow_shape_1990}{gglyph} or 'stripe glyph'
#' \insertCite{fuchs_evaluation_2013}{gglyph} in a scatterplot.
#'
#' @template fill.gradient-arg
#' @inheritParams ggplot2::layer
#' @inheritParams starglyphGrob
#' @inheritParams tileglyphGrob
#' @param cols Name of columns specifying the variables to be plotted in the
#'   glyphs as a character vector.
#' @param linewidth The line width of the tile glyphs.
#' @param colour The colour of the tile glyphs.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}()}. These
#'   are often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{colour = "green"} or \code{size = 3}. They may also be parameters to
#'   the paired geom/stat.
#'
#' @section Aesthetics: \code{geom_pieglyph()} understands the following
#'   aesthetics (required aesthetics are in bold): \itemize{ \item{\strong{x}}
#'   \item{\strong{y}} \item{alpha} \item{group} \item{size} }
#'
#' @family geoms
#'
#' @return A \code{geom} layer.
#'
#' @importFrom rlang as_quosures syms
#' @importFrom utils modifyList
#' @importFrom ggplot2 layer ggproto aes
#' @importFrom grid grobTree addGrob makeContent gTree setChildren
#' @importFrom Rdpack reprompt
#' @export
#'
#' @seealso \code{\link[gglyph]{tileglyphGrob}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#'
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
#'   geom_tileglyph(aes(x = mpg, y = disp),
#'                  cols = zs, size = 2,
#'                  fill.gradient = "Blues",
#'                  alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_tileglyph(aes(x = mpg, y = disp),
#'                  cols = zs, size = 2,
#'                  nrow = 2,
#'                  fill.gradient = "Greens",
#'                  alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_tileglyph(aes(x = mpg, y = disp),
#'                  cols = zs, size = 1,
#'                  ratio = 4,
#'                  fill.gradient = "RdYlBu",
#'                  alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
#' ggplot(data = mtcars) +
#'   geom_tileglyph(aes(x = mpg, y = disp),
#'                  cols = zs, size = 1,
#'                  ratio = 4, nrow = 2,
#'                  fill.gradient = "viridis",
#'                  alpha =  0.5) +
#'   ylim(c(-0, 550))
#'
geom_tileglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ...,
                           cols = character(0L),
                           colour = "black",
                           # fill = NA,
                           ratio = 1,
                           nrow = 1,
                           linewidth = 1,
                           fill.gradient = NULL,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  # Modify mapping to include cols
  mcols <- rlang::as_quosures(rlang::syms(cols), .GlobalEnv)
  names(mcols) <- cols
  mapping <- modifyList(mapping, mcols)

  params <- list(
    ratio = ratio,
    nrow = nrow,
    colour = colour,
    # fill = fill,
    fill.gradient = fill.gradient,
    cols = cols, ...)

  # Modify geom aesthetics to include cols
  geomout <- GeomTileGlyph
  geomout$required_aes <- c(geomout$required_aes, cols)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geomout,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params)

}

GeomTileGlyph <- ggplot2::ggproto("GeomTileGlyph", ggplot2::Geom,
                                  required_aes = c("x", "y"),
                                  default_aes = ggplot2::aes(size = 1,
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

                                    if (is.null(params$fill.gradient)) {
                                      stop('The "fill.gradient" gradient fill palette is missing.')
                                    }

                                    data$linewidth <- params$linewidth
                                    data$colour <- params$colour
                                    # data$fill <- params$fill
                                    data
                                  },

                                  draw_panel = function(data, panel_params,
                                                        coord, cols,
                                                        ratio,
                                                        nrow,
                                                        colour,
                                                        # fill,
                                                        fill.gradient) {

                                    data <- coord$transform(data, panel_params)

                                    grid.levels <- NULL

                                    # Convert factor columns to equivalent numeric
                                    fcols <- names(Filter(is.factor, data[, cols]))

                                    if (length(fcols) > 0)  {
                                      data[, fcols] <- lapply(data[, cols], function(f) as.numeric(levels(f))[f])
                                    }

                                    # Gradient colour mapping
                                    gdata <- NULL
                                    if (!is.null(fill.gradient)) {
                                      gdata <- data[, cols]

                                      gdata <- lapply(gdata,
                                                      function(x) scales::col_numeric(palette = fill.gradient,
                                                                                      domain = min(x):max(x))(x))
                                      gdata <- data.frame(gdata)
                                    }

                                    ggname("geom_tileglyph",
                                           grid::gTree(data=data,
                                                       # x = x, y = y,
                                                       cols=cols,
                                                       # fill = fill,
                                                       ratio = ratio,
                                                       nrow = nrow,
                                                       fill.gradient = fill.gradient,
                                                       gdata = gdata,
                                                       colour = colour,
                                                       # alpha = alpha,
                                                       # linejoin = "mitre",
                                                       cl="tileglyphtree"))

                                    # ggname("geom_tileglyph",
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

#' grid::makeContent function for the grobTree of tileglyphGrob objects
#' @param g A grid grobTree.
#' @export
#' @noRd
makeContent.tileglyphtree <- function(g) {

  gl <- lapply(seq_along(g$data$x),
               function(i) tileglyphGrob(x = g$data$x[i],
                                         y = g$data$y[i],
                                         z = unlist(g$data[i, g$cols]),
                                         size = g$data$size[i],
                                         ratio = g$ratio,
                                         nrow = g$nrow,
                                         fill = unlist(g$gdata[i, ]),
                                         col = g$colour,
                                         lwd = g$data$linewidth[i],
                                         alpha = g$data$alpha[i],
                                         linejoin = "mitre"))

  gl <- do.call(grid::gList, gl)

  grid::setChildren(g, gl)
}
