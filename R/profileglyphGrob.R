#' Draw a Profile Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a profile glyph
#' \insertCite{chambers_graphical_1983,dutoit_graphical_1986}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the distance of star glyph points from
#'   the center.
#' @param size The size of glyphs.
#' @param col The colour of whisker and contours.
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
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom grid rectGrob polygonGrob polylineGrob nullGrob grobTree gpar
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
#' barglyph <- profileglyphGrob(x = 100, y = 100, z = dims,
#'                              size = 100)
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 100, z = dims,
#'                                     size = 100, line = FALSE)
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 100, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 250, z = dims,
#'                              size = 100,
#'                              col = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 250, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     col = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 250, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  col = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 400, z = dims, size = 100,
#'                              fill = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 400, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 400, z = dims, size = 100,
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
#' barglyph <- profileglyphGrob(x = 100, y = 200, z = dims,
#'                              size = 100,
#'                              mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 200, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     mirror = FALSE)
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 200, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  mirror = FALSE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 350, z = dims,
#'                              size = 100, mirror = FALSE,
#'                              col = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 350, z = dims,
#'                                     size = 100, line = FALSE, mirror = FALSE,
#'                                     col = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 350, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  mirror = FALSE, col = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 500, z = dims, size = 100,
#'                              fill = "salmon", mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 500, z = dims,
#'                                     size = 100, line = FALSE, mirror = FALSE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 500, z = dims, size = 100,
#'                                  line = TRUE, bar = FALSE,
#'                                  mirror = FALSE, fill = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#'
#' # mirror = TRUE, flip.axes = TRUE
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' barglyph <- profileglyphGrob(x = 100, y = 100, z = dims,
#'                              size = 100, flip.axes = TRUE)
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 100, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     flip.axes = TRUE)
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 100, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 250, z = dims,
#'                              size = 100, flip.axes = TRUE,
#'                              col = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 250, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     flip.axes = TRUE,
#'                                     col = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 250, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  col = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 400, z = dims, size = 100,
#'                              flip.axes = TRUE,
#'                              fill = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 400, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     flip.axes = TRUE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 400, z = dims, size = 100,
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
#' barglyph <- profileglyphGrob(x = 100, y = 100, z = dims,
#'                              size = 100, flip.axes = TRUE,
#'                              mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 100, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     flip.axes = TRUE,
#'                                     mirror = FALSE)
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 100, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  mirror = FALSE)
#' grid::grid.newpage()
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 250, z = dims,
#'                              size = 100, mirror = FALSE,
#'                              flip.axes = TRUE,
#'                              col = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 250, z = dims,
#'                                     size = 100, line = FALSE, mirror = FALSE,
#'                                     flip.axes = TRUE,
#'                                     col = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 250, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  mirror = FALSE, col = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' barglyph <- profileglyphGrob(x = 100, y = 400, z = dims, size = 100,
#'                              flip.axes = TRUE,
#'                              fill = "salmon", mirror = FALSE)
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 400, z = dims,
#'                                     size = 100, line = FALSE, mirror = FALSE,
#'                                     flip.axes = TRUE,
#'                                     fill = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 400, z = dims, size = 100,
#'                                  line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  mirror = FALSE, fill = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)
#'
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' pg1 <- profileglyphGrob(x = 100, y = 150, z = dims,
#'                         size = 150, lwd = 5, width = 25)
#'
#' pg2 <- profileglyphGrob(x = 250, y = 250, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         linejoin = "round")
#'
#' pg3 <- profileglyphGrob(x = 400, y = 350, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         linejoin = "bevel")
#'
#' grid::grid.newpage()
#' grid::grid.draw(pg1)
#' grid::grid.draw(pg2)
#' grid::grid.draw(pg3)
#'
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' pg1 <- profileglyphGrob(x = 100, y = 150, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         bar = FALSE)
#'
#' pg2 <- profileglyphGrob(x = 250, y = 250, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         linejoin = "round", bar = FALSE)
#'
#' pg3 <- profileglyphGrob(x = 400, y = 350, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         linejoin = "bevel", bar = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(pg1)
#' grid::grid.draw(pg2)
#' grid::grid.draw(pg3)
#'
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' pg1 <- profileglyphGrob(x = 100, y = 150, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         line = FALSE)
#'
#' pg2 <- profileglyphGrob(x = 250, y = 250, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         linejoin = "round", line = FALSE)
#'
#' pg3 <- profileglyphGrob(x = 400, y = 350, z = dims,
#'                         size = 150, lwd = 5, width = 25,
#'                         linejoin = "bevel", line = FALSE)
#'
#' grid::grid.newpage()
#' grid::grid.draw(pg1)
#' grid::grid.draw(pg2)
#' grid::grid.draw(pg3)
#'
profileglyphGrob <- function(x = .5, y = .5, z,
                          size = 1,
                          col = 'black',
                          fill = NA,
                          lwd = 1,
                          alpha = 1,
                          width = 10,
                          flip.axes = FALSE,
                          bar = TRUE,
                          line = TRUE,
                          mirror = TRUE,
                          linejoin = c("mitre", "round", "bevel")) {

  linejoin <- match.arg(linejoin)

  # grid::grid.rect(gp=gpar(col="gray"))
  # grid::grid.points(x = x, y = y, pch =  20)

  dimension <- length(z)

  if (!flip.axes) {
    # Get bar central points
    xpos <- x + (width * seq(-(dimension-1)/2, (dimension-1)/2,
                             length.out = dimension))
    ypos <- y - z*size

    # Specify justification
    if (mirror) {
      barjust <- "center"
      # Line y points
      ypos1 <- y - ((z/2) * size)
      ypos2 <- y + ((z/2) * size)
    } else {
      barjust <- "top"
    }

    # Empty grobs
    bargrob <- grid::nullGrob()
    blinegrob <- grid::nullGrob()

    # Bar profile with/without line
    if (bar) {
      bargrob <- grid::rectGrob(x= xpos, y = rep(y, dimension),
                                width = width, height = z*size,
                                default.units = "native", just = barjust,
                                gp = grid::gpar(lwd = lwd, alpha = alpha,
                                                col = col, fill = fill,
                                                linejoin = linejoin))
      if (line) {
        if (mirror) {
          blinegrob <- grid::polylineGrob(x = c(xpos, rev(xpos)),
                                          y = c(ypos1, rev(ypos2)),
                                          id = rep(1:2, each = dimension),
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col,
                                                          linejoin = linejoin))
        } else {
          blinegrob <- grid::polylineGrob(x = xpos, y = ypos,
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col,
                                                          linejoin = linejoin))
        }
      }

    }

    # Line porfile without bar
    if (!bar & line) {
      if (mirror) {
        blinegrob <- grid::polygonGrob(x = c(xpos, rev(xpos)),
                                       y = c(ypos1, rev(ypos2)),
                                       default.units = "native",
                                       gp = grid::gpar(lwd = lwd,
                                                       alpha = alpha,
                                                       col = col,
                                                       linejoin = linejoin))
      } else {
        blinegrob <- grid::polygonGrob(x = c(xpos[1], xpos, xpos[dimension]),
                                       y = c(y, ypos, y),
                                       default.units = "native",
                                       gp = grid::gpar(lwd = lwd,
                                                       alpha = alpha,
                                                       col = col,
                                                       linejoin = linejoin))
      }
    }

    # grid::grid.points(x= xpos, y = rep(y, dimension), default.units = "native")

    gridout <- grid::grobTree(bargrob, blinegrob,
                              gp = grid::gpar(lwd = lwd, alpha = alpha,
                                              col = col, fill = fill,
                                              linejoin = linejoin))

    #---------------------------------------------------------------------------


  } else {
    # Get bar central points
    xpos <- x + z*size
    ypos <- y + (width * seq(-(dimension-1)/2, (dimension-1)/2,
                             length.out = dimension))

    # Specify justification
    if (mirror) {
      barjust <- "center"
      barjusth <- NULL
      # Line x points
      xpos1 <- x - ((z/2) * size)
      xpos2 <- x + ((z/2) * size)
    } else {
      barjust <- "center"
      barjusth <- 0
    }

    # Empty grobs
    bargrob <- grid::nullGrob()
    blinegrob <- grid::nullGrob()

    # Bar profile with/without line
    if (bar) {
      bargrob <- grid::rectGrob(x= rep(x, dimension), y = ypos,
                                width = z*size, height = width,
                                default.units = "native", just = barjust,
                                hjust = barjusth,
                                gp = grid::gpar(lwd = lwd, alpha = alpha,
                                                col = col, fill = fill,
                                                linejoin = linejoin))
      if (line) {
        if (mirror) {
          blinegrob <- grid::polylineGrob(x = c(xpos1, rev(xpos2)),
                                          y = c(ypos, rev(ypos)),
                                          id = rep(1:2, each = dimension),
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col,
                                                          linejoin = linejoin))
        } else {
          blinegrob <- grid::polylineGrob(x = xpos, y = ypos,
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col,
                                                          linejoin = linejoin))
        }
      }

    }

    # Line porfile without bar
    if (!bar & line) {
      if (mirror) {
        blinegrob <- grid::polygonGrob(x = c(xpos1, rev(xpos2)),
                                       y = c(ypos, rev(ypos)),
                                       default.units = "native",
                                       gp = grid::gpar(lwd = lwd,
                                                       alpha = alpha,
                                                       col = col,
                                                       linejoin = linejoin))
      } else {
        blinegrob <- grid::polygonGrob(x = c(x, xpos, x),
                                       y = c(ypos[1], ypos, ypos[dimension]),
                                       default.units = "native",
                                       gp = grid::gpar(lwd = lwd,
                                                       alpha = alpha,
                                                       col = col,
                                                       linejoin = linejoin))
      }
    }

    # grid::grid.points(x= rep(y, dimension), y = ypos, default.units = "native")

    gridout <- grid::grobTree(bargrob, blinegrob,
                              gp = grid::gpar(lwd = lwd, alpha = alpha,
                                              col = col, fill = fill,
                                              linejoin = linejoin))
  }

  return(gridout)

}


