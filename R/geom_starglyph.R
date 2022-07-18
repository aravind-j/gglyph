#' Add Star Glyphs as a Scatterplot
#'
#' The starglyph geom is used to plot multivariate data as star glyphs
#' \insertCite{siegel_surgical_1972,chambers_graphical_1983,dutoit_graphical_1986}{gglyph}
#' in a scatterplot.
#'
#' @template draw.grid-arg
#' @template full-arg
#' @template repel-arg
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
#' @import Rcpp
#' @importFrom rlang as_quosures syms
#' @importFrom utils modifyList
#' @importFrom ggplot2 layer ggproto aes
#' @importFrom grid addGrob convertHeight convertWidth grobTree grobX grobY
#'   makeContent setChildren unit
#' @importFrom Rdpack reprompt
#' @export
#'
#' @useDynLib gglyph
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
#' # Repel glyphs
#' ggplot(data = mtcars) +
#' geom_point(aes(x = mpg, y = disp, colour = cyl)) +
#'   geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
#'                  cols = zs, whisker = TRUE, contour = TRUE,
#'                  size = 10, alpha = 1, repel = TRUE) +
#'   ylim(c(-0, 550)) +
#'   xlim(c(8, 35))
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
                           repel = FALSE,
                           repel.control = gglyph.repel.control(),
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
    repel = repel,
    cols = cols,
    box.padding = unit(repel.control$box.padding, "lines"),
    point.padding = unit(repel.control$point.padding, "lines"),
    min.segment.length = unit(repel.control$min.segment.length, "lines"),
    arrow = repel.control$arrow,
    force = repel.control$force,
    force_pull = repel.control$force_pull,
    max.time = repel.control$max.time,
    max.iter = repel.control$max.iter,
    max.overlaps = repel.control$max.overlaps,
    nudge_x = repel.control$nudge_x,
    nudge_y = repel.control$nudge_y,
    xlim = repel.control$xlim,
    ylim = repel.control$ylim,
    direction = repel.control$direction,
    seed = repel.control$seed,
    verbose = repel.control$verbose,
    ...)

  # Modify geom aesthetics to include cols
  geomout <- GeomStarGlyph
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
                                                             lineend = "round",
                                                             # repel aes
                                                             point.size = 1,
                                                             segment.linetype = 1,
                                                             segment.colour = NULL,
                                                             segment.size = 0.5,
                                                             segment.alpha = NULL,
                                                             segment.curvature = -1e-20,
                                                             segment.angle = 20,
                                                             segment.ncp = 3,
                                                             segment.shape = 0.5,
                                                             segment.square = TRUE,
                                                             segment.squareShape = 1,
                                                             segment.inflect = FALSE,
                                                             segment.debug = FALSE),

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
                                                        point.size,
                                                        repel,
                                                        box.padding,
                                                        point.padding,
                                                        min.segment.length,
                                                        arrow,
                                                        force,
                                                        force_pull,
                                                        max.time,
                                                        max.iter,
                                                        max.overlaps,
                                                        nudge_x,
                                                        nudge_y,
                                                        xlim,
                                                        ylim,
                                                        direction,
                                                        seed,
                                                        verbose) {

                                    # if needed rename columns using our convention
                                    for (this_dim in c("x", "y")) {
                                      this_orig <- sprintf("%s_orig", this_dim)
                                      this_nudge <- sprintf("nudge_%s", this_dim)
                                      if (!this_nudge %in% colnames(data)) {
                                        data[[this_nudge]] <- data[[this_dim]]
                                        if (this_orig %in% colnames(data)) {
                                          data[[this_dim]] <- data[[this_orig]]
                                          data[[this_orig]] <- NULL
                                        }
                                      }
                                    }

                                    # Transform the nudges to the panel scales.
                                    nudges <- data.frame(x = data$nudge_x, y = data$nudge_y)
                                    nudges <- coord$transform(nudges, panel_params)

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

                                    # The nudge is relative to the data.
                                    data$nudge_x <- nudges$x - data$x
                                    data$nudge_y <- nudges$y - data$y

                                    # Transform limits to panel scales.
                                    limits <- data.frame(x = xlim, y = ylim)
                                    limits <- coord$transform(limits, panel_params)

                                    # Allow Inf.
                                    if (length(limits$x) == length(xlim)) {
                                      limits$x[is.infinite(xlim)] <- xlim[is.infinite(xlim)]
                                    }
                                    if (length(limits$y) == length(ylim)) {
                                      limits$y[is.infinite(ylim)] <- ylim[is.infinite(ylim)]
                                    }

                                    # Fill NAs with defaults.
                                    limits$x[is.na(limits$x)] <- c(0, 1)[is.na(limits$x)]
                                    limits$y[is.na(limits$y)] <- c(0, 1)[is.na(limits$y)]

                                    ggname("geom_starglyph",
                                           grid::gTree(data=data,
                                                       # x = x, y = y,
                                                       cols=cols,
                                                       # fill = fill,
                                                       colour.whisker = colour.whisker,
                                                       colour.contour = colour.contour,
                                                       linewidth.whisker = linewidth.whisker,
                                                       linewidth.contour = linewidth.contour,
                                                       # alpha = alpha,
                                                       astrt = astrt,
                                                       astp = astp,
                                                       whisker = whisker,
                                                       contour = contour,
                                                       # linejoin = "mitre",
                                                       # lineend = "round",
                                                       grid.levels = grid.levels,
                                                       draw.grid = draw.grid,
                                                       point.size = point.size,
                                                       colour.points = colour.points,
                                                       repel = repel,
                                                       limits = limits,
                                                       box.padding = box.padding,
                                                       point.padding = point.padding,
                                                       min.segment.length = min.segment.length,
                                                       arrow = arrow,
                                                       force = force,
                                                       force_pull = force_pull,
                                                       max.time = max.time,
                                                       max.iter = max.iter,
                                                       max.overlaps = max.overlaps,
                                                       nudge_x = nudge_x,
                                                       nudge_y = nudge_y,
                                                       xlim = xlim,
                                                       ylim = ylim,
                                                       direction = direction,
                                                       seed = seed,
                                                       verbose = verbose,
                                                       cl="starglyphtree"))

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

#' grid::makeContent function for the grobTree of starglyphGrob objects
#' @param g A grid grobTree.
#' @export
#' @noRd
makeContent.starglyphtree <- function(g) {

  if (g$repel) {
    # The padding around each bounding box.
    box_padding_x <- grid::convertWidth(g$box.padding, "native", valueOnly = TRUE)
    box_padding_y <- grid::convertHeight(g$box.padding, "native", valueOnly = TRUE)

    # The padding around each point.
    if (is.na(g$point.padding)) {
      g$point.padding = unit(0, "lines")
    }

    # create circle grobs
    boxg <- lapply(seq_along(g$data$x), function(i) {
      grid::circleGrob(g$data$x[i], g$data$y[i],
                       r = unit(max(g$data[i, g$cols]) * g$data[i, ]$size, "mm"),
                       gp = grid::gpar(col = "grey", fill = "transparent"))
    })

    boxg2 <- lapply(seq_along(g$data$x),
                    function(i) starglyphGrob(x = g$data$x[i],
                                              y = g$data$y[i],
                                              z = unlist(g$data[i, g$cols]),
                                              size = g$data$size[i],
                                              angle.start = g$astrt,
                                              angle.stop = g$astp,
                                              lwd.contour = g$data$linewidth.contour[i],
                                              col.contour = "grey",
                                              whisker = FALSE,
                                              contour = TRUE,
                                              draw.grid = FALSE))
    boxg2 <- lapply(seq_along(g$data$x),
                    function(i) boxg2[[i]]$children[[1]])

    # Create a dataframe with x1 y1 x2 y2
    boxes <- lapply(seq_along(boxg), function(i) {
      x1 <- grid::convertWidth(grid::grobX(boxg[[i]], "west"), "native", TRUE)
      x2 <- grid::convertWidth(grid::grobX(boxg[[i]], "east"), "native", TRUE)
      y1 <- grid::convertHeight(grid::grobY(boxg[[i]], "south"), "native", TRUE)
      y2 <- grid::convertHeight(grid::grobY(boxg[[i]], "north"), "native", TRUE)
      c(
        "x1" = x1 - box_padding_x + g$nudge_x,
        "y1" = y1 - box_padding_y + g$nudge_y,
        "x2" = x2 + box_padding_x + g$nudge_x,
        "y2" = y2 + box_padding_y + g$nudge_y
      )
    })

    boxes2 <- data.frame(do.call(rbind, boxes))
    # pg <- lapply(seq_along(boxes2$x1), function(i) {
    #   grid::polylineGrob(x = c(boxes2$x1[i], g$data$x[i],    boxes2$x2[i], g$data$x[i], boxes2$x1[i]),
    #                      y = c(g$data$y[i],  boxes2$y1[i], g$data$y[i],  boxes2$y2[i], g$data$y[i]),
    #                      gp = gpar(col = "grey"))
    # })
    pg <- lapply(seq_along(boxes2$x1), function(i) {
      grid::polylineGrob(x = c(boxes2$x1[i], boxes2$x1[i], boxes2$x2[i], boxes2$x2[i], boxes2$x1[i]),
                         y = c(boxes2$y1[i], boxes2$y2[i], boxes2$y2[i], boxes2$y1[i], boxes2$y1[i]),
                         gp = gpar(col = "gray"))
    })

    # Make the repulsion reproducible if desired.
    if (is.null(g$seed) || !is.na(g$seed)) {
      set.seed(g$seed)
    }

    # Beware the magic numbers. I do not understand them.
    # I just accept them as necessary to get the code to work.
    p_width <- grid::convertWidth(unit(1, "npc"), "inch", TRUE)
    p_height <- grid::convertHeight(unit(1, "npc"), "inch", TRUE)
    p_ratio <- (p_width / p_height)
    if (p_ratio > 1) {
      p_ratio <- p_ratio ^ (1 / (1.15 * p_ratio))
    }
    point_size <- p_ratio * grid::convertWidth(
      grid::unit(g$data$point.size, "lines"), "native", valueOnly = TRUE
    ) / 13
    point_padding <- p_ratio * grid::convertWidth(
      grid::unit(g$point.size, "lines"), "native", valueOnly = TRUE
    ) / 13

    # Repel overlapping bounding boxes away from each other.
    repel <- repel_boxes2(
      data_points     = as.matrix(g$data[,c("x","y")]),
      point_size      = point_size,
      point_padding_x = point_padding,
      point_padding_y = point_padding,
      boxes           = do.call(rbind, boxes),
      xlim            = range(g$limits$x),
      ylim            = range(g$limits$y),
      hjust           = rep (0.5, nrow(g$data)),
      vjust           = rep (0.5, nrow(g$data)),
      force_push      = g$force * 1e-6,
      force_pull      = g$force_pull * 1e-2,
      max_time        = g$max.time,
      max_iter        = ifelse(is.infinite(g$max.iter), 1e9, g$max.iter),
      max_overlaps    = g$max.overlaps,
      direction       = g$direction,
      verbose         = g$verbose
    )

    if (any(repel$too_many_overlaps)) {
      warning(sum(repel$too_many_overlaps, na.rm = TRUE),
              ' glyphs have too many overlaps.\nConsider increasing "max.overlaps"')
    }

    # if (all(repel$too_many_overlaps)) {
    #   grobs <- list()
    #   class(grobs) <- "gList"
    #   return(setChildren(x, grobs))
    # }

    # create segment grobs
    segg <- lapply(seq_along(g$data$x), function(i) {

      if (!repel$too_many_overlaps[i]) {
        row <- g$data[i, , drop = FALSE]
        grid::curveGrob(x1 = repel[i,]$x, y1 = repel[i,]$y, x2 = row$x, y2 = row$y,
                        default.units = "native",
                        curvature = row$segment.curvature,
                        angle = row$segment.angle,
                        ncp = row$segment.ncp,
                        shape = row$segment.shape,
                        square = row$segment.square,
                        squareShape = row$segment.squareShape,
                        inflect = row$segment.inflect,
                        debug = row$segment.debug,
                        gp = gpar(col = row$segment.colour,
                                  lwd = row$segment.size * ggplot2::.pt,
                                  lty = row$segment.linetype),
                        arrow = row$arrow)
      } else {
        grid::nullGrob()
      }
    })

  }

  gl <- lapply(seq_along(g$data$x),
               function(i) starglyphGrob(x = if (g$repel) {
                 repel$x[i]
               } else {
                 g$data$x[i]
               },
               y = if (g$repel) {
                 repel$y[i]
               } else {
                 g$data$y[i]
               },
               # x = g$data$x[i],
               # y = g$data$y[i],
               z = unlist(g$data[i, g$cols]),
               size = g$data$size[i],
               col.whisker = if (is.null(g$colour.whisker)) {
                 g$data$colour[i]
               } else {
                 g$colour.whisker
               },
               col.contour = if (is.null(g$colour.contour)) {
                 g$data$colour[i]
               } else {
                 g$colour.contour
               },
               fill = g$data$fill[i],
               lwd.whisker = g$data$linewidth.whisker[i],
               lwd.contour = g$data$linewidth.contour[i],
               alpha = g$data$alpha[i],
               angle.start = g$astrt,
               angle.stop = g$astp,
               whisker = g$whisker,
               contour = g$contour,
               linejoin = g$data$linejoin[i],
               lineend = g$data$lineend[i],
               grid.levels = g$grid.levels,
               draw.grid = g$draw.grid,
               point.size = grid::unit(g$point.size, "pt"),
               col.points = if (is.null(g$colour.points)) {
                 if (is.null(g$colour.whisker)) {
                   g$data$colour[i]
                 } else {
                   NA
                 }
               } else {
                 g$colour.points
               }))

  if (g$repel) {

    # gl <- lapply(seq_along(gl), function(i) grid::addGrob(gl[[i]], boxg[[i]]))

    # gl <- lapply(seq_along(gl), function(i) grid::addGrob(gl[[i]], boxg2[[i]]))
    #
    # gl <- lapply(seq_along(gl), function(i) grid::addGrob(gl[[i]], pg[[i]]))
    #
    gl <- lapply(seq_along(gl), function(i) grid::addGrob(gl[[i]], segg[[i]]))

    # reorder grobs
    # gl <- lapply(seq_along(gl),
    #              function(i) grid::reorderGrob(gl[[i]], c(7, 1:3, 4:6)))

    # gl <- lapply(seq_along(gl),
    #              function(i) grid::reorderGrob(gl[[i]], c(4, 1:3)))
  }

  gl <- do.call(grid::gList, gl)

  grid::setChildren(g, gl)

}
