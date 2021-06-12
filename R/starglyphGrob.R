#' Draw a Star Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a star glyph
#' \insertCite{siegel_surgical_1972,chambers_graphical_1983}{ggglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the distance of star gylph points from
#'   the center.
#' @param size The size of glyphs.
#' @param col The colour of whisker and contours.
#' @param fill The fill colour.
#' @param lwd The line width.
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
#' @seealso \code{\link[ggglyph]{geom_starglyph}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' stargylph <- starglyphGrob(x = 250, y = 250,
#'                            z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33), size = 100)
#' grid::grid.newpage()
#' grid::grid.draw(stargylph)
#'
starglyphGrob <- function(x = .5, y = .5, z,
                          size = 1,
                          col = 'black',
                          fill = NA,
                          lwd = 1,
                          alpha = 1,
                          angle.start = 0,
                          angle.stop = 2*base::pi,
                          whisker = TRUE,
                          contour = TRUE) {

  # grid::grid.rect(gp=gpar(col="gray"))
  # grid::grid.points(x = x, y = y, pch =  20)

  # Get polygon points
  dimension <- length(z)
  angle <- seq(angle.start, 2*base::pi, length.out = dimension + 1)[1:dimension]

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
                                                     lwd = lwd,
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
                                      gp = grid::gpar(col = col,
                                                      lwd = lwd,
                                                      alpha = alpha))
  }

  grid::grobTree(contourGrob, whiskerGrob,
                 gp = grid::gpar(lwd = lwd, alpha = alpha,
                                 col = col, fill = fill))


}
