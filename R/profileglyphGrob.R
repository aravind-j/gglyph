#' Draw a Profile Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a profile glyph
#' \insertCite{chambers_graphical_1983,dutoit_graphical_1986}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the values to be plotted as dimensions
#'   of the profile (length of the bars).
#' @param size The size of glyphs.
#' @param col.bar The colour of bars.
#' @param col.line The colour of line(s).
#' @param fill The fill colour.
#' @param lwd The line width.
#' @param alpha The alpha transparency value.
#' @param width The width of the bars.
#' @param flip.axes logical. If \code{TRUE}, axes are flipped.
#' @param bar logical. If \code{TRUE}, profile bars are plotted.
#' @param line logical. If \code{TRUE}, profile line is plotted.
#' @param mirror logical. If \code{TRUE}, mirror profile is plotted.
#' @param linejoin The line join style for the profile line(s) and bars. Either
#'   \code{"mitre"}, \code{"round"} or \code{"bevel"}.
#' @param draw.grid logical. If \code{TRUE}, grid lines are plotted along the
#'   bars. Default is \code{FALSE}.
#' @param grid.levels A list of grid levels (as vectors) corresponding to the
#'   values in \code{z} at which grid lines are to be plotted. The values in
#'   \code{z} should be present in the list specified.
#' @param lwd.grid The line width of the grid lines.
#' @param col.grid The colour of the grid lines.
#'
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom grid rectGrob polygonGrob polylineGrob nullGrob grobTree gpar
#'   unit.c
#' @export
#'
#' @seealso \code{\link[gglyph]{geom_profileglyph}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' # mirror = TRUE
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' barglyph <- profileglyphGrob(x = 200, y = 200, z = dims,
#'                              size = 20)
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 200, z = dims,
#'                                     size = 20, line = FALSE)
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 200, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 200, y = 450, z = dims,
#'                              size = 20,
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 450, z = dims,
#'                                     size = 20, line = FALSE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 450, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE,
#'                                  col.line = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 200, y = 700, z = dims, size = 20,
#'                              fill = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 700, z = dims,
#'                                     size = 20, line = FALSE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 700, z = dims, size = 20,
#'                                  line = TRUE, bar = FALSE,
#'                                  fill = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#'
#' # mirror = FALSE
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' barglyph <- profileglyphGrob(x = 200, y = 300, z = dims,
#'                              size = 20,
#'                              mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 300, z = dims,
#'                                     size = 20, line = FALSE,
#'                                     mirror = FALSE)
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 300, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE,
#'                                  mirror = FALSE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 200, y = 550, z = dims,
#'                              size = 20, mirror = FALSE,
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 550, z = dims,
#'                                     size = 20, line = FALSE, mirror = FALSE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 550, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE,
#'                                  mirror = FALSE, col.line = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)#'
#'
#' barglyph <- profileglyphGrob(x = 200, y = 800, z = dims, size = 20,
#'                              fill = "salmon", mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 800, z = dims,
#'                                     size = 20, line = FALSE, mirror = FALSE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 800, z = dims, size = 20,
#'                                  line = TRUE, bar = FALSE,
#'                                  mirror = FALSE, fill = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' # mirror = TRUE, flip.axes = TRUE
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' barglyph <- profileglyphGrob(x = 200, y = 200, z = dims,
#'                              size = 20, flip.axes = TRUE)
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 200, z = dims,
#'                                     size = 20, line = FALSE,
#'                                     flip.axes = TRUE)
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 200, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 200, y = 450, z = dims,
#'                              size = 20, flip.axes = TRUE,
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 450, z = dims,
#'                                     size = 20, line = FALSE,
#'                                     flip.axes = TRUE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 450, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  col.line = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 200, y = 700, z = dims, size = 20,
#'                              flip.axes = TRUE,
#'                              fill = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 700, z = dims,
#'                                     size = 20, line = FALSE,
#'                                     flip.axes = TRUE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 700, z = dims, size = 20,
#'                                  line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  fill = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#'
#' # mirror = FALSE, flip.axes = TRUE
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' barglyph <- profileglyphGrob(x = 200, y = 200, z = dims,
#'                              size = 20, flip.axes = TRUE,
#'                              mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 200, z = dims,
#'                                     size = 20, line = FALSE,
#'                                     flip.axes = TRUE,
#'                                     mirror = FALSE)
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 200, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  mirror = FALSE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 200, y = 450, z = dims,
#'                              size = 20, mirror = FALSE,
#'                              flip.axes = TRUE,
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 450, z = dims,
#'                                     size = 20, line = FALSE, mirror = FALSE,
#'                                     flip.axes = TRUE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 450, z = dims,
#'                                  size = 20, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  mirror = FALSE, col.line = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 200, y = 700, z = dims, size = 20,
#'                              flip.axes = TRUE,
#'                              fill = "salmon", mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 450, y = 700, z = dims,
#'                                     size = 20, line = FALSE, mirror = FALSE,
#'                                     flip.axes = TRUE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 700, y = 700, z = dims, size = 20,
#'                                  line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  mirror = FALSE, fill = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' # linejoin variants
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' pg1 <- profileglyphGrob(x = 200, y = 150, z = dims,
#'                         size = 25, lwd.bar = 5, width = 8)
#'
#' pg2 <- profileglyphGrob(x = 500, y = 400, z = dims,
#'                         size = 25, lwd.bar = 5, width = 8,
#'                         linejoin = "round")
#'
#' pg3 <- profileglyphGrob(x = 800, y = 650, z = dims,
#'                         size = 25, lwd.bar = 5, width = 8,
#'                         linejoin = "bevel")
#'
#' grid::grid.newpage()
#' grid::grid.draw(pg1)
#' grid::grid.draw(pg2)
#' grid::grid.draw(pg3)
#'
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' pg1 <- profileglyphGrob(x = 200, y = 150, z = dims,
#'                         size = 25, lwd.line = 5, width = 8,
#'                         bar = FALSE)
#'
#' pg2 <- profileglyphGrob(x = 500, y = 400, z = dims,
#'                         size = 25, lwd.line = 5, width = 8,
#'                         linejoin = "round", bar = FALSE)
#'
#' pg3 <- profileglyphGrob(x = 800, y = 650, z = dims,
#'                         size = 25, lwd.line = 5, width = 8,
#'                         linejoin = "bevel", bar = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(pg1)
#' grid::grid.draw(pg2)
#' grid::grid.draw(pg3)
#'
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' pg1 <- profileglyphGrob(x = 200, y = 150, z = dims,
#'                         size = 25, lwd.bar = 5, width = 8,
#'                         line = FALSE)
#'
#' pg2 <- profileglyphGrob(x = 500, y = 400, z = dims,
#'                         size = 25, lwd.bar = 5, width = 8,
#'                         linejoin = "round", line = FALSE)
#'
#' pg3 <- profileglyphGrob(x = 800, y = 650, z = dims,
#'                         size = 25, lwd.bar = 5, width = 8,
#'                         linejoin = "bevel", line = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(pg1)
#' grid::grid.draw(pg2)
#' grid::grid.draw(pg3)
#'
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' bg1 <- profileglyphGrob(x = 200, y = 200, z = dims,
#'                         size = 20,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg1 <- profileglyphGrob(x = 700, y = 200, z = dims,
#'                          size = 20, line = FALSE,
#'                          fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg2 <- profileglyphGrob(x = 350, y = 450, z = dims,
#'                         size = 20, mirror = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg2 <- profileglyphGrob(x = 850, y = 450, z = dims,
#'                          size = 20, line = FALSE, mirror = FALSE,
#'                          fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg3 <- profileglyphGrob(x = 200, y = 650, z = dims,
#'                         size = 20, flip.axes = TRUE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg3 <- profileglyphGrob(x = 700, y = 650, z = dims,
#'                          size = 20, line = FALSE, flip.axes = TRUE,
#'                          fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg4 <- profileglyphGrob(x = 350, y = 700, z = dims,
#'                         size = 20, mirror = FALSE, flip.axes = TRUE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg4 <- profileglyphGrob(x = 850, y = 700, z = dims,
#'                          size = 20, line = FALSE, mirror = FALSE,
#'                          flip.axes = TRUE,
#'                          fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#'
#' grid::grid.newpage()
#' grid::grid.draw(bg1)
#' grid::grid.draw(bpg1)
#' grid::grid.draw(bg2)
#' grid::grid.draw(bpg2)
#' grid::grid.draw(bg3)
#' grid::grid.draw(bpg3)
#' grid::grid.draw(bg4)
#' grid::grid.draw(bpg4)
#'
#' dims = c(1, 3, 2, 1, 2, 3)
#' gl <- split(x = rep(c(1, 2, 3), 6),
#'             f = rep(1:6, each = 3))
#'
#' bg1 <- profileglyphGrob(x = 150, y = 200, z = dims,
#'                         size = 10, width = 5,
#'                         draw.grid = TRUE, lwd.bar = 5,
#'                         grid.levels = gl, col.grid = "black")
#'
#' bg2 <- profileglyphGrob(x = 400, y = 250, z = dims,
#'                         size = 10, width = 5, lwd.bar = 5,
#'                         draw.grid = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "black")
#'
#' bg3 <- profileglyphGrob(x = 650, y = 200, z = dims,
#'                         size = 10, width = 5, flip.axes = TRUE,
#'                         draw.grid = TRUE, lwd.bar = 5,
#'                         grid.levels = gl, col.grid = "black")
#'
#' bg4 <- profileglyphGrob(x = 800, y = 200, z = dims,
#'                         size = 10, width = 5, flip.axes = TRUE,
#'                         draw.grid = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "black",
#'                         lwd.bar = 5)
#'
#' bg5 <- profileglyphGrob(x = 150, y = 500, z = dims,
#'                         size = 10, width = 5,
#'                         draw.grid = TRUE, lwd.bar = 5,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", line = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg6 <- profileglyphGrob(x = 400, y = 550, z = dims,
#'                         size = 10, width = 5, lwd.bar = 5,
#'                         draw.grid = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", line = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg7 <- profileglyphGrob(x = 650, y = 500, z = dims,
#'                         size = 10, width = 5, flip.axes = TRUE,
#'                         draw.grid = TRUE, lwd.bar = 5,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", line = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg8 <- profileglyphGrob(x = 800, y = 500, z = dims,
#'                         size = 10, width = 5, flip.axes = TRUE,
#'                         draw.grid = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", lwd.bar = 5, line = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#'
#' grid::grid.newpage()
#' grid::grid.draw(bg1)
#' grid::grid.draw(bg2)
#' grid::grid.draw(bg3)
#' grid::grid.draw(bg4)
#' grid::grid.draw(bg5)
#' grid::grid.draw(bg6)
#' grid::grid.draw(bg7)
#' grid::grid.draw(bg8)
#'
profileglyphGrob <- function(x = .5, y = .5, z,
                          size = 1,
                          col.bar = "black",
                          col.line = "black",
                          fill = NA,
                          lwd.bar = 1,
                          lwd.line = 1,
                          alpha = 1,
                          width = 5,
                          flip.axes = FALSE,
                          bar = TRUE,
                          line = TRUE,
                          mirror = TRUE,
                          linejoin = c("mitre", "round", "bevel"),
                          grid.levels = NULL,
                          draw.grid = FALSE,
                          col.grid = "grey",
                          lwd.grid = 1) {

  linejoin <- match.arg(linejoin)

  # grid::grid.rect(gp=gpar(col="gray"))
  # grid::grid.points(x = x, y = y, pch =  20)

  # Checks for grid lines
  drawgridlines <- FALSE
  if (draw.grid) {
    if (!is.null(grid.levels)) { # Check if grid lines are to be plotted
      # Check if grid.levels is a list in appropriate format
      if (is.list(grid.levels) &
          all(unlist(lapply(grid.levels,
                            function(x) is.numeric(x) | is.integer(x))))) {
        # Check if z is present in corresponding grid.levels
        if (!all(mapply(function(a, b) a %in% b, z, grid.levels))) {
          warning('Mismatch in values "z" values and corresponding "grid.levels".\n',
                  'Unable to plot grid lines.')
        } else {
          drawgridlines <- TRUE
        }

      } else {
        warning('Non-standard format specified as "grid.levels".\n',
                'Unable to plot grid lines.')
      }

    } else {
      warning('"grid.levels" not specified.\n',
              'Unable to plot grid lines.')
    }
  }

  # Empty grobs
  bargrob <- grid::nullGrob()
  blinegrob <- grid::nullGrob()
  glinesGrob <- grid::nullGrob()

  dimension <- length(z)

  width <- grid::unit(width, "mm")

  if (!flip.axes) {
    # Get bar central points
    # xpos <- x + (width * seq(-(dimension - 1) / 2, (dimension - 1) / 2,
    #                          length.out = dimension))
    # ypos <- y - z * size

    xpos <- unit(x, "native") +
      (width * seq(-(dimension - 1) / 2, (dimension - 1) / 2,
                   length.out = dimension))
    # ypos <- unit(y, "native") - unit(z * size, "mm")
    ypos <- unit(y, "native") + unit(z * size, "mm")

    # Specify justification
    if (mirror) {
      barjust <- "center"
      # Line y points
      # ypos1 <- y - ((z / 2) * size)
      # ypos2 <- y + ((z / 2) * size)

      ypos1 <- unit(y, "native") - unit((z / 2) * size, "mm")
      ypos2 <- unit(y, "native") + unit((z / 2) * size, "mm")
    } else {
      # barjust <- "top"
      barjust <- "bottom"
    }

    # Bar profile with/without line
    if (bar) {
      bargrob <- grid::rectGrob(x = xpos,
                                # y = rep(y, dimension),
                                y = rep(unit(y, "native") + unit(0, "mm"),
                                        dimension),
                                width = width,
                                # height = z * size,
                                height = unit(z * size, "mm"),
                                # default.units = "native",
                                just = barjust,
                                gp = grid::gpar(lwd = lwd.bar, alpha = alpha,
                                                col = col.bar, fill = fill,
                                                linejoin = linejoin))
      if (line) {
        if (mirror) {
          blinegrob <- grid::polylineGrob(x = grid::unit.c(xpos, rev(xpos)),
                                          y = grid::unit.c(ypos1, rev(ypos2)),
                                          # x = c(xpos, rev(xpos)),
                                          # y = c(ypos1, rev(ypos2)),
                                          id = rep(1:2, each = dimension),
                                          # default.units = "native",
                                          gp = grid::gpar(lwd = lwd.line,
                                                          alpha = alpha,
                                                          col = col.line,
                                                          linejoin = linejoin))
        } else {
          blinegrob <- grid::polylineGrob(x = xpos, y = ypos,
                                          # default.units = "native",
                                          gp = grid::gpar(lwd = lwd.line,
                                                          alpha = alpha,
                                                          col = col.line,
                                                          linejoin = linejoin))
        }
      }

    }

    # Line porfile without bar
    if (!bar & line) {
      if (mirror) {
        blinegrob <- grid::polygonGrob(x = grid::unit.c(xpos, rev(xpos)),
                                       y = grid::unit.c(ypos1, rev(ypos2)),
                                       # x = c(xpos, rev(xpos)),
                                       # y = c(ypos1, rev(ypos2)),
                                       # default.units = "native",
                                       gp = grid::gpar(lwd = lwd.line,
                                                       alpha = alpha,
                                                       col = col.line,
                                                       linejoin = linejoin))
      } else {
        blinegrob <- grid::polygonGrob(x = grid::unit.c(xpos[1],
                                                        xpos,
                                                        xpos[dimension]),
                                       y = grid::unit.c(unit(y, "native") + unit(0, "mm"),
                                                        ypos,
                                                        unit(y, "native") + unit(0, "mm")),
                                       # x = c(xpos[1], xpos, xpos[dimension]),
                                       # y = c(y, ypos, y),
                                       # default.units = "native",
                                       gp = grid::gpar(lwd = lwd.line,
                                                       alpha = alpha,
                                                       col = col.line,
                                                       linejoin = linejoin))
      }
    }

    # grid::grid.points(x= xpos, y = rep(y, dimension), default.units = "native")
    # grid::grid.points(x= xpos, y = rep(unit(y, "native"), dimension), default.units = "native")

    if (drawgridlines) {
      # plot grid lines
      grid.levels <- mapply(function(a, b) b[b <= a], z, grid.levels)

      if (mirror) {
        # gridy <- mapply(function(a, b) a - (b * size), ypos2, grid.levels)
        gridy <- mapply(function(a, b) a - unit(b * size, "mm"),
                        ypos2, grid.levels)
        # gridy <- mapply(function(a, b) setdiff(b, a), ypos1, gridy)
      } else {
        # gridy <- lapply(grid.levels, function(a) y - (a * size))
        gridy <- lapply(grid.levels, function(a) unit(y, "native") + unit(a * size, "mm"))
        # gridy <- mapply(function(a, b) setdiff(b, a), y - (z * size), gridy)
      }

      gridx <- mapply(function(a, b) rep(a, length(b)), xpos, gridy)

      # gridx <- unlist(gridx)
      # gridy <- unlist(gridy)

      gridx <- upgradeUnit.unit.list(gridx)
      gridy <- upgradeUnit.unit.list(gridy)

      if (is.na(col.grid)) {
        if (length(col.bar == length(grid.levels))) {
          col.grid <- mapply(function(a, b) rep(a, length(b)),
                             col.bar, grid.levels)
          col.grid <- unlist(col.grid)
        } else {
          col.grid <- col.bar
        }
      }

      # grid.draw(pointsGrob(gridx, gridy, pch = 3))

      gridxstrt <- gridx - (width / 2)
      gridxstp <- gridx + (width / 2)

      # grid::grid.points(c(gridxstrt, gridxstp), rep(gridy, 2), pch = 20)

      glinesGrob <- grid::polylineGrob(x = grid::unit.c(gridxstrt, gridxstp),
                                       # x = c(gridxstrt, gridxstp),
                                       y = rep(gridy, 2),
                                       id = rep(seq_len(length(gridx)), 2),
                                       # default.units = "native",
                                       gp = gpar(col = col.grid,
                                                 lwd = lwd.grid,
                                                 lineend = "butt",
                                                 alpha = alpha))
    }

    #---------------------------------------------------------------------------


  } else {
    # Get bar central points
    # xpos <- x + z * size
    # ypos <- y + (width * seq(-(dimension - 1) / 2, (dimension - 1)/ 2,
    #                          length.out = dimension))

    xpos <- unit(x, "native") + unit(z * size, "mm")
    ypos <- unit(y, "native") +
      (width * seq(-(dimension - 1) / 2, (dimension - 1)/ 2,
                   length.out = dimension))

    # Specify justification
    if (mirror) {
      barjust <- "center"
      barjusth <- NULL
      # Line x points
      # xpos1 <- x - ((z / 2) * size)
      # xpos2 <- x + ((z / 2) * size)

      xpos1 <- unit(x, "native") - unit((z / 2) * size, "mm")
      xpos2 <- unit(x, "native") + unit((z / 2) * size, "mm")
    } else {
      barjust <- "center"
      barjusth <- 0
    }

    # Bar profile with/without line
    if (bar) {
      bargrob <- grid::rectGrob(x= rep(unit(x, "native") + unit(0, "mm"),
                                       dimension),
                                # x= rep(x, dimension),
                                y = ypos,
                                width = unit(z * size, "mm"),
                                # width = z * size,
                                height = width,
                                # default.units = "native",
                                just = barjust,
                                hjust = barjusth,
                                gp = grid::gpar(lwd = lwd.bar, alpha = alpha,
                                                col = col.bar, fill = fill,
                                                linejoin = linejoin))
      if (line) {
        if (mirror) {
          blinegrob <- grid::polylineGrob(x = grid::unit.c(xpos1, rev(xpos2)),
                                          y = grid::unit.c(ypos, rev(ypos)),
                                          # x = c(xpos1, rev(xpos2)),
                                          # y = c(ypos, rev(ypos)),
                                          id = rep(1:2, each = dimension),
                                          # default.units = "native",
                                          gp = grid::gpar(lwd = lwd.line,
                                                          alpha = alpha,
                                                          col = col.line,
                                                          linejoin = linejoin))
        } else {
          blinegrob <- grid::polylineGrob(x = xpos, y = ypos,
                                          # default.units = "native",
                                          gp = grid::gpar(lwd = lwd.line,
                                                          alpha = alpha,
                                                          col = col.line,
                                                          linejoin = linejoin))
        }
      }

    }

    # Line porfile without bar
    if (!bar & line) {
      if (mirror) {
        blinegrob <- grid::polygonGrob(x = grid::unit.c(xpos1, rev(xpos2)),
                                       y = grid::unit.c(ypos, rev(ypos)),
                                       # x = c(xpos1, rev(xpos2)),
                                       # y = c(ypos, rev(ypos)),
                                       # default.units = "native",
                                       gp = grid::gpar(lwd = lwd.line,
                                                       alpha = alpha,
                                                       col = col.line,
                                                       linejoin = linejoin))
      } else {
        blinegrob <- grid::polygonGrob(x = grid::unit.c(unit(x, "native") + unit(0, "mm"),
                                                        xpos,
                                                        unit(x, "native") + unit(0, "mm")),
                                       y = grid::unit.c(ypos[1],
                                                        ypos,
                                                        ypos[dimension]),
                                       # x = c(x, xpos, x),
                                       # y = c(ypos[1], ypos, ypos[dimension]),
                                       default.units = "native",
                                       gp = grid::gpar(lwd = lwd.line,
                                                       alpha = alpha,
                                                       col = col.line,
                                                       linejoin = linejoin))
      }
    }

    # grid::grid.points(x= rep(y, dimension), y = ypos, default.units = "native")

    if (drawgridlines) {
      # plot grid lines
      grid.levels <- mapply(function(a, b) b[b <= a], z, grid.levels)

      if (mirror) {
        # gridx <- mapply(function(a, b) a - (b * size), xpos2, grid.levels)
        gridx <- mapply(function(a, b) a - unit(b * size, "mm"),
                                                xpos2, grid.levels)
        # gridx <- mapply(function(a, b) a - (b * size), xpos2, grid.levels)
      } else {
        # gridx <- lapply(grid.levels, function(a) x + (a * size))
        gridx <- lapply(grid.levels, function(a) unit(x, "native") + unit(a * size, "mm"))
        # gridx <- mapply(function(a, b) setdiff(b, a), x + (z * size), gridx)
      }

      gridy <- mapply(function(a, b) rep(a, length(b)), ypos, gridx)

      # gridx <- unlist(gridx)
      # gridy <- unlist(gridy)

      gridx <- upgradeUnit.unit.list(gridx)
      gridy <- upgradeUnit.unit.list(gridy)

      if (is.na(col.grid)) {
        if (length(col.bar == length(grid.levels))) {
          col.grid <- mapply(function(a, b) rep(a, length(b)),
                             col.bar, grid.levels)
          col.grid <- unlist(col.grid)
        } else {
          col.grid <- col.bar
        }
      }

      # grid.draw(pointsGrob(gridx, gridy, pch = 3))

      gridystrt <- gridy - (width / 2)
      gridystp <- gridy + (width / 2)

      # grid::grid.points(rep(gridx, 2), c(gridystrt, gridystp), pch = 20)

      glinesGrob <- grid::polylineGrob(x = rep(gridx, 2),
                                       # y = c(gridystrt, gridystp),
                                       y = grid::unit.c(gridystrt, gridystp),
                                       id = rep(seq_len(length(gridy)), 2),
                                       # default.units = "native",
                                       gp = gpar(col = col.grid,
                                                 lwd = lwd.grid,
                                                 lineend = "butt",
                                                 alpha = alpha))
    }

  }

  gridout <- grid::grobTree(bargrob, blinegrob, glinesGrob,
                            gp = grid::gpar(alpha = alpha,
                                            fill = fill,
                                            linejoin = linejoin))
  return(gridout)

}
