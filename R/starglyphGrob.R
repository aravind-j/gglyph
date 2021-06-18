#' Draw a Star Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a star glyph
#' \insertCite{siegel_surgical_1972,chambers_graphical_1983,dutoit_graphical_1986}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the distance of star glyph points from
#'   the centre.
#' @param size The size of glyphs.
#' @param col.whisker The colour of whisker.
#' @param col.contour The colour of contours.
#' @param col.points The colour of grid.points.
#' @param fill The fill colour.
#' @param lwd.whisker The whisker line width.
#' @param lwd.contour The contour line width.
#' @param alpha The alpha transparency value.
#' @param angle.start The start angle for the glyph in radians. Default is zero.
#' @param angle.stop The stop angle for the glyph in radians. Default is
#'   \eqn{2\pi}.
#' @param whisker logical. If \code{TRUE}, plots the star glyph whiskers.
#' @param contour logical. If \code{TRUE}, plots the star glyph contours.
#' @param linejoin The line join style for the contour polygon. Either
#'   \code{"mitre"}, \code{"round"} or \code{"bevel"}.
#' @param lineend The line end style for the whisker lines. Either
#'   \code{"round"}, \code{"butt"} or \code{"square"}.
#' @param grid.points logical. If \code{TRUE}, grid points are plotted along the
#'   whiskers. Default is \code{FALSE}.
#' @param grid.levels A list of grid levels (as vectors) corresponding to the
#'   values in \code{z} at which points are to be plotted. The values in
#'   \code{z} should be present in the list specified.
#' @param point.size The size of the grid points in native units.
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom grid polygonGrob polylineGrob nullGrob grobTree gpar unit
#' @export
#'
#' @seealso \code{\link[gglyph]{geom_starglyph}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' sg1 <- starglyphGrob(x = 250, y = 150,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100)
#'
#' sg2 <- starglyphGrob(x = 250, y = 300,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.1)
#'
#' sg3 <- starglyphGrob(x = 250, y = 450,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3)
#'
#' sg4 <- starglyphGrob(x = 500, y = 150,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      angle.start = 0, angle.stop = -base::pi)
#'
#' sg5 <- starglyphGrob(x = 500, y = 300,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.1,
#'                      angle.start = 0, angle.stop = -base::pi)
#'
#' sg6 <- starglyphGrob(x = 500, y = 450,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      angle.start = 0, angle.stop = -base::pi)
#'
#' grid::grid.newpage()
#' grid::grid.draw(sg1)
#' grid::grid.draw(sg2)
#' grid::grid.draw(sg3)
#' grid::grid.draw(sg4)
#' grid::grid.draw(sg5)
#' grid::grid.draw(sg6)
#'
#' sg1 <- starglyphGrob(x = 250, y = 150,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      fill = "salmon")
#'
#' sg2 <- starglyphGrob(x = 250, y = 300,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.1,
#'                      fill = "cyan")
#'
#' sg3 <- starglyphGrob(x = 250, y = 450,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      fill = "green")
#'
#' sg4 <- starglyphGrob(x = 500, y = 150,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      fill = "salmon")
#'
#' sg5 <- starglyphGrob(x = 500, y = 300,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.1,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      fill = "cyan")
#'
#' sg6 <- starglyphGrob(x = 500, y = 450,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      fill = "green")
#'
#' grid::grid.newpage()
#' grid::grid.draw(sg1)
#' grid::grid.draw(sg2)
#' grid::grid.draw(sg3)
#' grid::grid.draw(sg4)
#' grid::grid.draw(sg5)
#' grid::grid.draw(sg6)
#'
#' sg1 <- starglyphGrob(x = 250, y = 150,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      col.contour = "gray")
#'
#' sg2 <- starglyphGrob(x = 250, y = 300,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.1,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      col.contour = "gray")
#'
#' sg3 <- starglyphGrob(x = 250, y = 450,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      col.contour = "gray")
#'
#' sg4 <- starglyphGrob(x = 500, y = 150,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      col.contour = "gray")
#'
#' sg5 <- starglyphGrob(x = 500, y = 300,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.1,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      col.contour = "gray")
#'
#' sg6 <- starglyphGrob(x = 500, y = 450,
#'                      z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      col.contour = "gray")
#'
#' grid::grid.newpage()
#' grid::grid.draw(sg1)
#' grid::grid.draw(sg2)
#' grid::grid.draw(sg3)
#' grid::grid.draw(sg4)
#' grid::grid.draw(sg5)
#' grid::grid.draw(sg6)
#'
#' sg1 <- starglyphGrob(x = 150, y = 250,
#'                      z = c(0.28, 0.33, 0.8, 1.2, 0.6, 0.5, 0.7), size = 100,
#'                      lwd.contour = 10)
#'
#' sg2 <- starglyphGrob(x = 300, y = 250,
#'                      z = c(0.28, 0.33, 0.8, 1.2, 0.6, 0.5, 0.7), size = 100,
#'                      lwd.contour = 10, linejoin = "bevel")
#'
#' sg3 <- starglyphGrob(x = 450, y = 250,
#'                      z = c(0.28, 0.33, 0.8, 1.2, 0.6, 0.5, 0.7), size = 100,
#'                      lwd.contour = 10, linejoin = "round")
#'
#' grid::grid.newpage()
#' grid::grid.draw(sg1)
#' grid::grid.draw(sg2)
#' grid::grid.draw(sg3)
#'
#' sg1 <- starglyphGrob(x = 150, y = 250,
#'                      z = c(0.28, 0.33, 0.8, 1.2, 0.6, 0.5, 0.7), size = 100,
#'                      lwd.whisker = 10, contour = FALSE)
#'
#' sg2 <- starglyphGrob(x = 300, y = 250,
#'                      z = c(0.28, 0.33, 0.8, 1.2, 0.6, 0.5, 0.7), size = 100,
#'                      lwd.whisker = 10, lineend = "butt", contour = FALSE)
#'
#' sg3 <- starglyphGrob(x = 450, y = 250,
#'                      z = c(0.28, 0.33, 0.8, 1.2, 0.6, 0.5, 0.7), size = 100,
#'                      lwd.whisker = 10, lineend = "square", contour = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(sg1)
#' grid::grid.draw(sg2)
#' grid::grid.draw(sg3)
#'
#' gl <- split(x = rep(c(1, 2, 3), 6),
#'             f = rep(1:6, each = 3))
#'
#' sg1 <- starglyphGrob(x = 150, y = 150,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      grid.points = TRUE, grid.levels = gl)
#'
#' sg2 <- starglyphGrob(x = 150, y = 300,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.01,
#'                      grid.points = TRUE, grid.levels = gl,
#'                      contour = FALSE)
#'
#' sg3 <- starglyphGrob(x = 150, y = 450,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      grid.points = FALSE, grid.levels = gl,
#'                      whisker = FALSE)
#'
#' sg4 <- starglyphGrob(x = 400, y = 150,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      grid.points = TRUE, grid.levels = gl,
#'                      contour = FALSE,
#'                      point.size = 5)
#'
#' sg5 <- starglyphGrob(x = 400, y = 300,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.01,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      grid.points = TRUE, grid.levels = gl,
#'                      point.size = 20)
#'
#' sg6 <- starglyphGrob(x = 400, y = 450,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      grid.points = FALSE, grid.levels = gl,
#'                      whisker = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(sg1)
#' grid::grid.draw(sg2)
#' grid::grid.draw(sg3)
#' grid::grid.draw(sg4)
#' grid::grid.draw(sg5)
#' grid::grid.draw(sg6)
#'
#' gl <- split(x = rep(c(1, 2, 3), 6),
#'             f = rep(1:6, each = 3))
#'
#' sg1 <- starglyphGrob(x = 150, y = 150,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      grid.points = TRUE, grid.levels = gl,
#'                      col.points = NA)
#'
#' sg2 <- starglyphGrob(x = 150, y = 300,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.01,
#'                      grid.points = TRUE, grid.levels = gl,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      contour = FALSE)
#'
#' sg3 <- starglyphGrob(x = 150, y = 450,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      grid.points = FALSE, grid.levels = gl,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      whisker = FALSE)
#'
#' sg4 <- starglyphGrob(x = 400, y = 150,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      grid.points = TRUE, grid.levels = gl,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      contour = FALSE,
#'                      point.size = 10, col.points = "gray")
#'
#' sg5 <- starglyphGrob(x = 400, y = 300,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 3,
#'                      lwd.contour = 0.01,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      grid.points = TRUE, grid.levels = gl,
#'                      col.whisker = RColorBrewer::brewer.pal(6, "Dark2"),
#'                      point.size = 20, col.points = NA)
#'
#' sg6 <- starglyphGrob(x = 400, y = 450,
#'                      z = c(1, 3, 2, 1, 2, 3), size = 25,
#'                      lwd.whisker = 0.1,
#'                      lwd.contour = 3,
#'                      angle.start = 0, angle.stop = -base::pi,
#'                      grid.points = FALSE, grid.levels = gl,
#'                      whisker = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(sg1)
#' grid::grid.draw(sg2)
#' grid::grid.draw(sg3)
#' grid::grid.draw(sg4)
#' grid::grid.draw(sg5)
#' grid::grid.draw(sg6)
#'
starglyphGrob <- function(x = .5, y = .5, z,
                          size = 1,
                          col.whisker = "black",
                          col.contour = "black",
                          col.points = "black",
                          fill = NA,
                          lwd.whisker = 1,
                          lwd.contour = 1,
                          alpha = 1,
                          angle.start = 0,
                          angle.stop = 2 * base::pi,
                          whisker = TRUE,
                          contour = TRUE,
                          linejoin = c("mitre", "round", "bevel"),
                          lineend = c("round", "butt", "square"),
                          grid.levels = NULL,
                          grid.points = FALSE,
                          point.size = 10) {

  linejoin <- match.arg(linejoin)
  lineend <- match.arg(lineend)

  # grid::grid.rect(gp=gpar(col="gray"))
  # grid::grid.points(x = x, y = y, pch =  20)

  # Get polygon points
  dimension <- length(z)

  if (abs(angle.start - angle.stop) == 2 * base::pi) {
    angle <- seq(angle.start, angle.stop,
                 length.out = dimension + 1)[1:dimension]
  } else {
    angle <- seq(angle.start, angle.stop,
                 length.out = dimension)
  }

  starx <- x + (z * size * cos(angle))
  stary <- y + (z * size * sin(angle))

  # Empty Grobs
  contourGrob <- grid::nullGrob()
  whiskerGrob <- grid::nullGrob()
  gpointsGrob <- grid::nullGrob()

  # Plot contours/polygon
  if (contour == TRUE) {
    # grid::grid.points(x = starx, y = stary, pch = 3)
    contourGrob <- grid::polygonGrob(x = starx,
                                     y = stary,
                                     default.units = "native",
                                     gp = grid::gpar(fill = fill,
                                                     col = col.contour,
                                                     lwd = lwd.contour,
                                                     alpha = alpha,
                                                     linejoin = linejoin))
  }

  # Plot whiskers
  if (whisker == TRUE) {
    rayx <- unlist(t(expand.grid(x, starx)))
    rayy <- unlist(t(expand.grid(y, stary)))
    rayid <- rep(1:dimension, each = 2)

    whiskerGrob <- grid::polylineGrob(x = rayx,
                                      y = rayy,
                                      id = rayid,
                                      default.units = "native",
                                      gp = grid::gpar(col = col.whisker,
                                                      lwd = lwd.whisker,
                                                      alpha = alpha,
                                                      lineend = lineend))
  }

  # Plot grid points

  if (grid.points) {
    if (!is.null(grid.levels)) { # Check if grid points are to be plotted
      # Check if grid.levels is a list in appropriate format
      if (is.list(grid.levels) &
          all(unlist(lapply(grid.levels,
                            function(x) is.numeric(x) | is.integer(x))))) {
        # Check if z is present in corresponding grid.levels
        if (!all(mapply(function(a, b) a %in% b, z, grid.levels))) {
          warning('Mismatch in values "z" values and corresponding "grid.levels".\n',
                  'Unable to plot grid points.')
        } else {
          # plot points
          grid.levels <- mapply(function(a, b) b[b <= a], z, grid.levels)

          starpx <- mapply(function(a, b) x + (a * size * cos(b)),
                           grid.levels, angle)
          starpy <- mapply(function(a, b) y + (a * size * sin(b)),
                           grid.levels, angle)

          starpx <- unlist(starpx)
          starpy <- unlist(starpy)

          if (is.na(col.points)) {
            if (length(col.whisker == length(grid.levels))) {
              col.points <- mapply(function(a, b) rep(a, length(b)),
                                   col.whisker, grid.levels)
              col.points <- unlist(col.points)
            } else {
              col.points <- col.whisker
            }
          }

          gpointsGrob <- grid::pointsGrob(starpx, starpy,
                                          default.units = "native",
                                          pch = 20,
                                          size = grid::unit(point.size,
                                                      "native"),
                                          gp = grid::gpar(col = col.points,
                                                          alpha = alpha))
        }

      } else {
        warning('Non-standard format specified as "grid.levels".\n',
                'Unable to plot grid points.')
      }

    } else {
      warning('"grid.levels" not specified.\n',
              'Unable to plot grid points.')
    }
  }


  grid::grobTree(contourGrob, whiskerGrob, gpointsGrob,
                 gp = grid::gpar(alpha = alpha,
                                 fill = fill))

}
