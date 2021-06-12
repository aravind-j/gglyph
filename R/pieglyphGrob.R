#' Draw a Pie Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a circular pie or clock glyph
#' \insertCite{ward_visualization_2000,fuchs_evaluation_2013}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the distance of star gylph points from
#'   the center.
#' @param size The size of glyphs.
#' @param edges The number of edges of the polygon to depict the circular glyph
#'   outline.
#' @param col The colour of whisker and contours.
#' @param fill The fill colour.
#' @param lwd The line width.
#' @param alpha The alpha transparency value.
#' @param angle.start The start angle for the glyph in radians. Default is zero.
#' @param angle.stop The stop anlge for the glyph in radians. Default is
#'   \eqn{2\pi}.
#' @param scale.segment logical. If \code{TRUE}, the segments (pie slices) are
#'   scaled according to value of z.
#' @param scale.radius logical. If \code{TRUE}, the radius of segments (pie
#'   slices) are scaled according to value of z.
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom dplyr bind_rows
#' @importFrom grid gpar polygonGrob
#' @importFrom scales rescale
#' @export
#'
#' @seealso \code{\link[gglyph]{geom_pieglyph}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' p1 <- pieglyphGrob(x = 150, y = 150,
#'              z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'              size = 50)
#'
#' p2 <- pieglyphGrob(x = 300, y = 150,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.radius = FALSE)
#'
#' p3 <- pieglyphGrob(x = 450, y = 150,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.segment = TRUE, scale.radius = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(p1)
#' grid::grid.draw(p2)
#' grid::grid.draw(p3)
#'
pieglyphGrob <- function(x = .5, y = .5, z,
                         size = 1, edges = 200,
                         col = 'black', # equal to no. of segments
                         fill = NA, # equal to no. of segments
                         lwd = 1,
                         alpha = 1,
                         angle.start = 0,
                         angle.stop = 2*base::pi,
                         scale.segment = FALSE, scale.radius = TRUE) {

  # grid::grid.points(x = x, y = y, pch =  20)

  # Get polygon points
  dimension <- length(z)
  angle <- seq(angle.start, 2*base::pi, length.out = dimension + 1)[1:dimension]

  # Convert z to cumulative proportions
  if (scale.segment) {
    cumpropz <- c(0, cumsum(z)/sum(z))
  } else {
    # Convert z to cumulative proportions
    cumpropz <- c(0, scales::rescale(1:dimension))
  }

  diffz <- diff(cumpropz)

  gl <- grid::grobTree()
  segxylist <- vector("list", dimension)

  for (i in 1:dimension) {
    # No. of nodes for sector arc
    n <- max(2, floor(edges * diffz[i]))

    # Sector arc points
    arcp <- seq.int(cumpropz[i], cumpropz[i + 1], length.out = n)

    # Sector arc point radians
    arcpr <- (angle.stop * arcp) + (angle.start * pi/180)

    # Sector arc coordinates

    if (!scale.radius) {
      segxylist[[i]] <- data.frame(arcx = c(x, x + (size * cos(arcpr))),
                                   arcy = c(y, y + (size * sin(arcpr))),
                                   arcid = i)
    } else {
      segxylist[[i]] <- data.frame(arcx = c(x, x + (z[i] * size * cos(arcpr))),
                                   arcy = c(y, y + (z[i] * size * sin(arcpr))),
                                   arcid = i)
    }
  }

  segxy <- dplyr::bind_rows(segxylist)

  grid::polygonGrob(x = segxy$arcx,
                    y = segxy$arcy,
                    default.units = "native",
                    gp = grid::gpar(col = col,
                                    fill = fill,
                                    lwd = lwd,
                                    alpha = alpha))
}
