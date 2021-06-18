#' Draw a Metroglyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a metroglyph
#' \insertCite{anderson_semigraphical_1957,dutoit_graphical_1986}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the length of rays.
#' @param size The size of rays.
#' @param circle.size The size of the central circle.
#' @param col.circle The circle colour.
#' @param col.ray The colour of rays.
#' @param col.points The colour of grid.points.
#' @param fill The circle fill colour.
#' @param lwd.circle The circle line width.
#' @param lwd.ray The ray line width.
#' @param alpha The alpha transparency value.
#' @param angle.start The start angle for the glyph rays in radians. Default is
#'   zero.
#' @param angle.stop The stop angle for the glyph rays in radians. Default is
#'   \eqn{2\pi}.
#' @param lineend The line end style for the rays. Either \code{"round"},
#'   \code{"butt"} or \code{"square"}.
#' @param grid.points logical. If \code{TRUE}, grid points are plotted along the
#'   whiskers. Default is \code{FALSE}.
#' @param grid.levels A list of grid levels (as vectors) corresponding to the
#'   values in \code{z} at which points are to be plotted. The values in
#'   \code{z} should be present in the list specified.
#' @param point.size The size of the grid points in native units.
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom grid circleGrob polylineGrob grobTree gpar nullGrob unit
#' @export
#'
#' @seealso \code{\link[gglyph]{geom_metroglyph}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' mglyph1 <- metroglyphGrob(x = 200, y = 100,
#'                              z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                              size = 100, circle.size = 10)
#'
#' mglyph2 <- metroglyphGrob(x = 500, y = 100,
#'                              z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                              size = 100, circle.size = 25)
#'
#' mglyph3 <- metroglyphGrob(x = 200, y = 300,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 0,
#'                           angle.start = base::pi, angle.stop = -base::pi)
#'
#' mglyph4 <- metroglyphGrob(x = 500, y = 300,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 50,
#'                           angle.start = base::pi, angle.stop = -base::pi)
#'
#' grid::grid.newpage()
#' grid::grid.draw(mglyph1)
#' grid::grid.draw(mglyph2)
#' grid::grid.draw(mglyph3)
#' grid::grid.draw(mglyph4)
#'
#' mglyph1 <- metroglyphGrob(x = 200, y = 100,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 10,
#'                           angle.start = base::pi, angle.stop = 0)
#'
#' mglyph2 <- metroglyphGrob(x = 500, y = 100,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 25,
#'                           angle.start = base::pi, angle.stop = 0)
#'
#' mglyph3 <- metroglyphGrob(x = 200, y = 400,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 0,
#'                           angle.start = 0, angle.stop = -base::pi)
#'
#' mglyph4 <- metroglyphGrob(x = 500, y = 400,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 50,
#'                           angle.start = 0, angle.stop = -base::pi)
#'
#' grid::grid.newpage()
#' grid::grid.draw(mglyph1)
#' grid::grid.draw(mglyph2)
#' grid::grid.draw(mglyph3)
#' grid::grid.draw(mglyph4)
#'
#' mglyph1 <- metroglyphGrob(x = 200, y = 100,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 10, lwd.circle = 3)
#'
#' mglyph2 <- metroglyphGrob(x = 500, y = 100,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 25, lwd.circle = 3)
#'
#' mglyph3 <- metroglyphGrob(x = 200, y = 300,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 0,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 3)
#'
#' mglyph4 <- metroglyphGrob(x = 500, y = 300,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 50,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 3)
#'
#' grid::grid.newpage()
#' grid::grid.draw(mglyph1)
#' grid::grid.draw(mglyph2)
#' grid::grid.draw(mglyph3)
#' grid::grid.draw(mglyph4)
#'
#' mglyph1 <- metroglyphGrob(x = 200, y = 100,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 10, lwd.circle = 3,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"),
#'                           col.circle = "gray")
#'
#' mglyph2 <- metroglyphGrob(x = 500, y = 100,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 25, lwd.circle = 3,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"),
#'                           col.circle = "white", fill = "black")
#'
#' mglyph3 <- metroglyphGrob(x = 200, y = 300,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 0,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 3,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' mglyph4 <- metroglyphGrob(x = 500, y = 300,
#'                           z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                           size = 100, circle.size = 50,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 5, lwd.circle = 15,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"),
#'                           col.circle = "white", fill = "gray")
#'
#' grid::grid.newpage()
#' grid::grid.draw(mglyph1)
#' grid::grid.draw(mglyph2)
#' grid::grid.draw(mglyph3)
#' grid::grid.draw(mglyph4)
#'
#' mg1 <- metroglyphGrob(x = 150, y = 150,
#'                       z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                       size = 50, circle.size = 25,
#'                       lwd.ray = 5)
#'
#' mg2 <- metroglyphGrob(x = 300, y = 250,
#'                       z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                       size = 50, circle.size = 25,
#'                       lwd.ray = 5, lineend = "butt")
#'
#' mg3 <- metroglyphGrob(x = 450, y = 350,
#'                       z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                       size = 50, circle.size = 25,
#'                       lwd.ray = 5, lineend = "square")
#'
#' grid::grid.newpage()
#' grid::grid.draw(mg1)
#' grid::grid.draw(mg2)
#' grid::grid.draw(mg3)
#'
#' gl <- split(x = rep(c(1, 2, 3), 6),
#'             f = rep(1:6, each = 3))
#'
#' mglyph1 <- metroglyphGrob(x = 200, y = 100,
#'                           z = c(1, 3, 2, 1, 2, 3),
#'                           size = 25, circle.size = 10, lwd.circle = 3,
#'                           grid.points = TRUE, grid.levels = gl)
#'
#' mglyph2 <- metroglyphGrob(x = 500, y = 100,
#'                           z = c(1, 3, 2, 1, 2, 3),
#'                           size = 25, circle.size = 25, lwd.circle = 3,
#'                           grid.points = TRUE, grid.levels = gl)
#'
#' mglyph3 <- metroglyphGrob(x = 200, y = 300,
#'                           z = c(1, 3, 2, 1, 2, 3),
#'                           size = 25, circle.size = 0,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 3,
#'                           grid.points = TRUE, grid.levels = gl)
#'
#' mglyph4 <- metroglyphGrob(x = 500, y = 300,
#'                           z = c(1, 3, 2, 1, 2, 3),
#'                           size = 25, circle.size = 50,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 3,
#'                           grid.points = TRUE, grid.levels = gl)
#'
#' grid::grid.newpage()
#' grid::grid.draw(mglyph1)
#' grid::grid.draw(mglyph2)
#' grid::grid.draw(mglyph3)
#' grid::grid.draw(mglyph4)
#'
#' gl <- split(x = rep(c(0, 1, 2), 6),
#'             f = rep(1:6, each = 3))
#'
#' mglyph1 <- metroglyphGrob(x = 200, y = 100,
#'                           z = c(0, 2, 1, 0, 1, 2),
#'                           size = 25, circle.size = 10, lwd.circle = 3,
#'                           grid.points = TRUE, grid.levels = gl,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"),
#'                           col.points = NA)
#'
#' mglyph2 <- metroglyphGrob(x = 500, y = 100,
#'                           z = c(0, 2, 1, 0, 1, 2),
#'                           size = 25, circle.size = 25, lwd.circle = 3,
#'                           grid.points = TRUE, grid.levels = gl,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' mglyph3 <- metroglyphGrob(x = 200, y = 300,
#'                           z = c(0, 2, 1, 0, 1, 2),
#'                           size = 25, circle.size = 0,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 3,
#'                           grid.points = TRUE, grid.levels = gl,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"),
#'                           col.points = "white")
#'
#' mglyph4 <- metroglyphGrob(x = 500, y = 300,
#'                           z = c(0, 2, 1, 0, 1, 2),
#'                           size = 25, circle.size = 50,
#'                           angle.start = base::pi, angle.stop = -base::pi,
#'                           lwd.ray = 3,
#'                           grid.points = TRUE, grid.levels = gl,
#'                           col.ray = RColorBrewer::brewer.pal(6, "Dark2"),
#'                           col.points = NA, point.size = 20)
#'
#' grid::grid.newpage()
#' grid::grid.draw(mglyph1)
#' grid::grid.draw(mglyph2)
#' grid::grid.draw(mglyph3)
#' grid::grid.draw(mglyph4)
#'
metroglyphGrob <- function(x = .5, y = .5, z,
                          size = 1, circle.size = 10,
                          col.circle = "black",
                          col.ray = "black",
                          col.points = "black",
                          fill = NA,
                          lwd.circle = 1,
                          lwd.ray = 1,
                          alpha = 1,
                          angle.start = 0,
                          angle.stop = 2 * base::pi,
                          lineend = c("round", "butt", "square"),
                          grid.levels = NULL,
                          grid.points = FALSE,
                          point.size = 10) {

  lineend <- match.arg(lineend)

  # grid::grid.points(x = x, y = y, pch =  20)

  circGrob <- grid::circleGrob(x = x, y = y, r = circle.size,
                               default.units = "native",
                               gp = grid::gpar(lwd = lwd.circle,
                                               alpha = alpha,
                                               col = col.circle,
                                               fill = fill))

  # Get polygon points
  dimension <- length(z)

  if (abs(angle.start - angle.stop) == 2 * base::pi) {
    angle <- seq(angle.start, angle.stop,
                 length.out = dimension + 1)[1:dimension]
  } else {
    angle <- seq(angle.start, angle.stop, length.out = dimension)
  }

  # rayx <- x + (z * size * cos(angle))
  # rayy <- y + (z * size * sin(angle))
  # grid.points(rayx, rayy)

  rayxstp <- x + ((circle.size + (z * size)) * cos(angle))
  rayystp <- y + ((circle.size + (z * size)) * sin(angle))
  # grid.points(rayxstp, rayystp, pch = 4)

  rayxstrt <- x + (circle.size * cos(angle))
  rayystrt <- y + (circle.size * sin(angle))
  # grid.points(rayxstrt, rayystrt, pch = 20)

  rayx <- c(rayxstrt, rayxstp)
  rayy <- c(rayystrt, rayystp)
  rayid <- rep(1:dimension, 2)

  rayGrob <- grid::polylineGrob(x = rayx,
                                y = rayy,
                                id = rayid,
                                default.units = "native",
                                gp = grid::gpar(col = col.ray,
                                                lwd = lwd.ray,
                                                alpha = alpha,
                                                lineend = lineend))

  gpointsGrob <- grid::nullGrob()

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

          gridx <- mapply(function(a, b) x + ((circle.size +
                                                 (a * size)) * cos(b)),
                           grid.levels, angle)
          gridy <- mapply(function(a, b) y + ((circle.size +
                                                 (a * size)) * sin(b)),
                           grid.levels, angle)

          gridx <- unlist(gridx)
          gridy <- unlist(gridy)

          if (is.na(col.points)) {
            if (length(col.ray == length(grid.levels))) {
              col.points <- mapply(function(a, b) rep(a, length(b)),
                                   col.ray, grid.levels)
              col.points <- unlist(col.points)
            } else {
              col.points <- col.ray
            }
          }

          gpointsGrob <- grid::pointsGrob(gridx, gridy,
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

  grid::grobTree(circGrob, rayGrob, gpointsGrob,
                 gp = grid::gpar(alpha = alpha, fill = fill,
                                 lineend = lineend))

}
