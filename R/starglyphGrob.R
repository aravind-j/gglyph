#' Draw a Star Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a star glyph
#' \insertCite{siegel_surgical_1972,chambers_graphical_1983,dutoit_graphical_1986}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the distance of star gylph points from
#'   the center.
#' @param size The size of glyphs.
#' @param col.whisker The colour of whisker.
#' @param col.contour The colour of contours.
#' @param fill The fill colour.
#' @param lwd.whisker The whisker line width.
#' @param lwd.contour The contour line width.
#' @param alpha The alpha transparency value.
#' @param angle.start The start angle for the glyph in radians. Default is zero.
#' @param angle.stop The stop anlge for the glyph in radians. Default is
#'   \eqn{2\pi}.
#' @param whisker logical. If \code{TRUE}, plots the star glyph whiskers.
#' @param contour logical. If \code{TRUE}, plots the star glyph contours. glyph.
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom grid polygonGrob polylineGrob nullGrob grobTree gpar
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
#'                      lwd.contour = 0.01)
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
#'                      lwd.contour = 0.01,
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
#'                      lwd.contour = 0.01,
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
#'                      lwd.contour = 0.01,
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
#'                      lwd.contour = 0.01,
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
#'                      lwd.contour = 0.01,
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
starglyphGrob <- function(x = .5, y = .5, z,
                          size = 1,
                          col.whisker = 'black',
                          col.contour = 'black',
                          fill = NA,
                          lwd.whisker = 1,
                          lwd.contour = 1,
                          alpha = 1,
                          angle.start = 0,
                          angle.stop = 2*base::pi,
                          whisker = TRUE,
                          contour = TRUE) {

  # grid::grid.rect(gp=gpar(col="gray"))
  # grid::grid.points(x = x, y = y, pch =  20)

  # Get polygon points
  dimension <- length(z)

  if (abs(angle.start - angle.stop) == 2*base::pi) {
    angle <- seq(angle.start, angle.stop, length.out = dimension + 1)[1:dimension]
  } else {
    angle <- seq(angle.start, angle.stop, length.out = dimension)
  }

  starx <- x + (z * size * cos(angle))
  stary <- y + (z * size * sin(angle))

  # Empty Grobs
  contourGrob <- grid::nullGrob()
  whiskerGrob <- grid::nullGrob()


  # Plot contours/polygon
  if (contour == TRUE) {
    # grid::grid.points(x = starx, y = stary, pch = 3)
    contourGrob <- grid::polygonGrob(x = starx,
                                     y = stary,
                                     default.units = "native",
                                     gp = grid::gpar(fill = fill,
                                                     col = col.contour,
                                                     lwd = lwd.contour,
                                                     alpha = alpha))
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
                                                      alpha = alpha))
  }

  grid::grobTree(contourGrob, whiskerGrob,
                 gp = grid::gpar(alpha = alpha,
                                 fill = fill))


}
