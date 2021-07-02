#' Draw a Tile Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a tile glyph similar to
#' 'autoglyph' \insertCite{beddow_shape_1990}{gglyph} or 'stripe glyph'
#' \insertCite{fuchs_evaluation_2013}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the values to be plotted as dimensions
#'   in the tileglyph.
#' @param size The size of glyphs.
#' @param ratio The aspect ratio (height / width).
#' @param nrow The number of rows.
#' @param col The line colour.
#' @param fill The fill colour.
#' @param lwd The line width.
#' @param alpha The alpha transparency value.
#' @param linejoin The line join style for the tile polygon. Either
#'   \code{"mitre"}, \code{"round"} or \code{"bevel"}.
#'
#' @family grobs
#'
#' @return A \code{\link[grid]{grob}} object.
#'
#' @importFrom grid rectGrob gpar
#' @export
#'
#' @seealso \code{\link[gglyph]{geom_tileglyph}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' tg1 <- tileglyphGrob(x = 150, y = 150,
#'                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                    size = 5)
#'
#' tg2 <- tileglyphGrob(x = 450, y = 150,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 5)
#'
#' tg3 <- tileglyphGrob(x = 150, y = 250,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 5, nrow = 2)
#'
#' tg4 <- tileglyphGrob(x = 450, y = 250,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 5, nrow = 2)
#'
#' tg5 <- tileglyphGrob(x = 150, y = 350,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 5,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' tg6 <- tileglyphGrob(x = 450, y = 350,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 5,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' tg7 <- tileglyphGrob(x = 150, y = 450,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 5, nrow = 2,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' tg8 <- tileglyphGrob(x = 450, y = 450,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 5, nrow = 2,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' grid::grid.newpage()
#' grid::grid.draw(tg1)
#' grid::grid.draw(tg2)
#' grid::grid.draw(tg3)
#' grid::grid.draw(tg4)
#' grid::grid.draw(tg5)
#' grid::grid.draw(tg6)
#' grid::grid.draw(tg7)
#' grid::grid.draw(tg8)
#'
#' tg1 <- tileglyphGrob(x = 150, y = 150,
#'                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                    size = 2, ratio = 6)
#'
#' tg2 <- tileglyphGrob(x = 450, y = 150,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 2, ratio = 6)
#'
#' tg3 <- tileglyphGrob(x = 150, y = 300,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 2, nrow = 2, ratio = 6)
#'
#' tg4 <- tileglyphGrob(x = 450, y = 300,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 2, nrow = 2, ratio = 6)
#'
#' tg5 <- tileglyphGrob(x = 150, y = 450,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 2, ratio = 6,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' tg6 <- tileglyphGrob(x = 450, y = 450,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 2, ratio = 6,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' tg7 <- tileglyphGrob(x = 150, y = 600,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 2, nrow = 2, ratio = 6,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' tg8 <- tileglyphGrob(x = 450, y = 600,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
#'                      size = 2, nrow = 2, ratio = 6,
#'                      fill = RColorBrewer::brewer.pal(7, "Dark2"))
#'
#' grid::grid.newpage()
#' grid::grid.draw(tg1)
#' grid::grid.draw(tg2)
#' grid::grid.draw(tg3)
#' grid::grid.draw(tg4)
#' grid::grid.draw(tg5)
#' grid::grid.draw(tg6)
#' grid::grid.draw(tg7)
#' grid::grid.draw(tg8)
#'
#' tg1 <- tileglyphGrob(x = 150, y = 150,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 5, nrow = 2, lwd = 5)
#'
#' tg2 <- tileglyphGrob(x = 300, y = 300,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 5, nrow = 2, lwd = 5,
#'                      linejoin = "round")
#'
#' tg3 <- tileglyphGrob(x = 450, y = 450,
#'                      z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7, 4.3),
#'                      size = 5, nrow = 2, lwd = 5,
#'                      linejoin = "bevel")
#'
#'
#' grid::grid.newpage()
#' grid::grid.draw(tg1)
#' grid::grid.draw(tg2)
#' grid::grid.draw(tg3)
#'
tileglyphGrob <- function(x = .5, y = .5, z,
                          size = 10,
                          ratio = 1,
                          nrow = 1,
                          col = "black",
                          fill = NA,
                          lwd = 1,
                          alpha = 1,
                          linejoin = c("mitre", "round", "bevel")) {

  linejoin <- match.arg(linejoin)

  # grid::grid.points(x = x, y = y, pch =  20)

  dimension <- length(z)

  # width <- size
  # height <- size * ratio

  width <- unit(size, "mm")
  height <- unit(size, "mm") * ratio

  dim <- ceiling(dimension / nrow)

  zs <- split(z, sort(rep_len(1:nrow, length(z))))

  if (nrow > 1) {
    # ys <- y + (height * seq(-(nrow - 1) / 2, (nrow - 1) / 2,
    #                         length.out = nrow))
    ys <- unit(y, "native") +
      (height * seq(-(nrow - 1) / 2, (nrow - 1) / 2,
                    length.out = nrow))
  } else {
    # ys <- y
    ys <- unit(y, "native") + unit(0, "mm")
  }

  # xs <- x + (width * seq(-(dim - 1) / 2, (dim - 1) / 2,
  #                        length.out = dim))

  xs <- unit(x, "native") +
    (width * seq(-(dim - 1) / 2, (dim - 1) / 2,
                 length.out = dim))

  # xylist <- vector("list", nrow)
  xlist <- vector("list", nrow)
  ylist <- vector("list", nrow)

  for (i in 1:nrow) {
    nz <- length(zs[[i]])
    # xylist[[i]] <- data.frame(xpos = xs[1:nz],
    #                           ypos = rep(ys[[i]], nz))
    xlist[[i]] <- xs[1:nz]
    ylist[[i]] <- rep(ys[[i]], nz)
  }

  # xylist <- dplyr::bind_rows(xylist)
  xlist <- upgradeUnit.unit.list(xlist)
  ylist <- upgradeUnit.unit.list(ylist)

  # grid::grid.points(x = xylist$xpos, y = xylist$ypos, pch =  1)

  grid::rectGrob(x = xlist,
                 y = ylist,
                 # x = xylist$xpos,
                 # y = xylist$ypos,
                 width = width, height = height,
                 # default.units = "native",
                 just = "centre",
                 gp = gpar(col = col,
                           fill = fill,
                           lwd = lwd,
                           alpha = alpha,
                           linejoin = linejoin))
}
