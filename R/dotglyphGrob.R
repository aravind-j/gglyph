#' Draw a Dot Profile Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a dot profile glyph
#' \insertCite{chambers_graphical_1983,dutoit_graphical_1986}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the values to be plotted as dimensions
#'   of the dot glyph (number of stacked dots).
#' @param radius The radius of the glyphs.
#' @param col The line colour.
#' @param fill The fill colour.
#' @param lwd The line width.
#' @param alpha The alpha transparency value.
#' @param mirror logical. If \code{TRUE}, mirror profile is plotted.
#' @param flip.axes logical. If \code{TRUE}, axes are flipped.
#'
#' @family grobs
#'
#' @return A \code{\link[grid]{grob}} object.
#'
#' @importFrom grid circleGrob gpar
#' @export
#'
#' @seealso \code{\link[gglyph]{geom_dotglyph}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' dg1 <- dotglyphGrob(x = 150, y = 300,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2)
#'
#' dg2 <- dotglyphGrob(x = 550, y = 300,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, mirror = TRUE)
#'
#' dg3 <- dotglyphGrob(x = 100, y = 550,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, flip.axes = TRUE)
#'
#' dg4 <- dotglyphGrob(x = 550, y = 550,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, mirror = TRUE,
#'                     flip.axes = TRUE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(dg1)
#' grid::grid.draw(dg2)
#' grid::grid.draw(dg3)
#' grid::grid.draw(dg4)
#'
#' dg1 <- dotglyphGrob(x = 150, y = 300,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, fill = "black", col = "white")
#'
#' dg2 <- dotglyphGrob(x = 550, y = 300,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, mirror = TRUE,
#'                     fill = "salmon", col = "black")
#'
#' dg3 <- dotglyphGrob(x = 100, y = 550,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, flip.axes = TRUE,
#'                     fill = "cyan", col = "grey")
#'
#' dg4 <- dotglyphGrob(x = 550, y = 550,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, mirror = TRUE,
#'                     flip.axes = TRUE,
#'                     fill = "green", col = "grey")
#'
#' grid::grid.newpage()
#' grid::grid.draw(dg1)
#' grid::grid.draw(dg2)
#' grid::grid.draw(dg3)
#' grid::grid.draw(dg4)
#'
#' clrs <- mapply(function(a, b) rep(a, b),
#'                RColorBrewer::brewer.pal(6, "Dark2"),
#'                round(c(4, 3.5, 2.7, 6.8, 3.4, 5.7)))
#' clrs <- unlist(clrs)
#'
#' dg1 <- dotglyphGrob(x = 150, y = 300,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, fill = clrs, col = "white")
#'
#' dg2 <- dotglyphGrob(x = 550, y = 300,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, mirror = TRUE,
#'                     fill = clrs, col = "black")
#'
#' dg3 <- dotglyphGrob(x = 100, y = 550,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, flip.axes = TRUE,
#'                     fill = "black", col = clrs, lwd = 5)
#'
#' dg4 <- dotglyphGrob(x = 550, y = 550,
#'                     z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                     radius = 2, mirror = TRUE,
#'                     flip.axes = TRUE,
#'                     col = clrs)
#'
#' grid::grid.newpage()
#' grid::grid.draw(dg1)
#' grid::grid.draw(dg2)
#' grid::grid.draw(dg3)
#' grid::grid.draw(dg4)
#'
dotglyphGrob <- function(x = .5, y = .5, z,
                         radius = 1,
                         col = "black",
                         fill = NA,
                         lwd = 1,
                         alpha = 1,
                         mirror = FALSE,
                         flip.axes = FALSE) {
  z <- round(z)
  dimension <- length(z)

  # grid::grid.points(x = x, y = y, pch =  20)
  radius <- grid::unit(radius, "mm")
  lwd2 <- grid::convertUnit(unit(lwd, "points")/4, "mm") # how does this work ?

  if (mirror) {
    stksq <- stackseq(z, radius + lwd2)
    stksq <- unit(stksq, "mm")
    stksq <- 2 * stksq
  } else {
    stksq <- ((radius + lwd2) * 2) *
      seq(-(dimension - 1) / 2,
          (dimension - 1) / 2,
          length.out = dimension)
  }

  if (!flip.axes) {
    # xpos <- x + ((radius * 2) * seq(-(dimension - 1) / 2, (dimension - 1) / 2,
    #                                 length.out = dimension))
    xpos <- unit(x, "native") + stksq
    # ypos <- rep(y, dimension)
    ypos <- rep(unit(y, "native"), dimension)

    circx <- mapply(function(a, b) rep(a, b), xpos, z)
    # circy <- lapply(z, function(c) y - (1:c * (radius * 2)) + radius)
    circy <- lapply(z, function(c) unit(y, "native") +
                      (1:c * ((radius + lwd2) * 2)) - (radius + lwd2))

    if (mirror) {
      circy <- mapply(function(a, b) a - ((radius + lwd2) * b), circy, z)
    }

  } else {
    # xpos <- rep(x, dimension)
    xpos <- rep(unit(x, "native"), dimension)
    # ypos <- y - ((radius * 2) * seq(-(dimension - 1) / 2, (dimension - 1) / 2,
    #                                 length.out = dimension))
    ypos <- unit(y, "native") - stksq

    circy <- mapply(function(a, b) rep(a, b), ypos, z)
    # circx <- lapply(z, function(c) x + (1:c * (radius * 2)) - radius)
    circx <- lapply(z, function(c) unit(x, "native") +
                      (1:c * ((radius + lwd2) * 2)) - (radius + lwd2))

    if (mirror) {
      circx <- mapply(function(a, b) a - ((radius + lwd2) * b), circx, z)
    }

  }

  # circx <- unlist(circx)
  # circy <- unlist(circy)

  circx <- upgradeUnit.unit.list(circx)
  circy <- upgradeUnit.unit.list(circy)

  # grid::grid.points(x = xpos, y = ypos, pch =  1)

  grid::circleGrob(x = circx, y = circy, r = radius,
                   # default.units = "native",
                   gp = gpar(col = col,
                             fill = fill,
                             lwd = lwd,
                             alpha = alpha))
}


stackseq <- function(z, r) {

  zevn <- (z %% 2) == 0
  apthm <- (sqrt(3)/2) * r

  sq <- vector("list", length(z))

  for (i in seq_along(z)) {
    if (i == 1) {
      sq[[i]] <- r + unit(0, "mm")
    } else {
      if (zevn[i] == zevn[i-1]) {
        sq[[i]] <- sq[[i-1]] + r
      } else {
        sq[[i]] <- sq[[i-1]] + apthm
      }
    }
  }

  sq <- unlist(sq)
  sq <- sq - mean(sq)

  return(sq)
}
