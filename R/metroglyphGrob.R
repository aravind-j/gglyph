#' Draw a Metroglyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a metroglyph
#' \insertCite{anderson_semigraphical_1957,dutoit_graphical_1986}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the distance of star glyph points from
#'   the center.
#' @param size The size of rays.
#' @param circle.size The size of the central circle.
#' @param col.circle The circle colour.
#' @param col.ray The colour of rays.
#' @param fill The circle fill colour.
#' @param lwd.circle The circle line width.
#' @param lwd.ray The ray line width.
#' @param alpha The alpha transparency value.
#' @param angle.start The start angle for the glyph rays in radians. Default is
#'   zero.
#' @param angle.stop The stop anlge for the glyph rays in radians. Default is
#'   \eqn{2\pi}.
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom grid circleGrob polylineGrob grobTree gpar
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
metroglyphGrob <- function(x = .5, y = .5, z,
                          size = 1, circle.size = 10,
                          col.circle = 'black',
                          col.ray = 'black',
                          fill = NA,
                          lwd.circle = 1,
                          lwd.ray = 1,
                          alpha = 1,
                          angle.start = 0,
                          angle.stop = 2*base::pi) {

  # grid::grid.points(x = x, y = y, pch =  20)

  circGrob <- grid::circleGrob(x = x, y = y, r = circle.size,
                               default.units = "native",
                               gp = grid::gpar(lwd = lwd.circle,
                                               alpha = alpha,
                                               col = col.circle,
                                               fill = fill))

  # Get polygon points
  dimension <- length(z)

  if (abs(angle.start - angle.stop) == 2*base::pi) {
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
                                                alpha = alpha))

  grid::grobTree(circGrob, rayGrob,
                 gp = grid::gpar(alpha = alpha, fill = fill))

}
