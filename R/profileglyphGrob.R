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
#' @param grid.lines logical. If \code{TRUE}, grid lines are plotted along the
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
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 250, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 250, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  col.line = "green")
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
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 350, z = dims,
#'                                     size = 100, line = FALSE, mirror = FALSE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 350, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  mirror = FALSE, col.line = "green")
#'
#' grid::grid.draw(barglyph)
#' grid::grid.draw(barprofileglyph)
#' grid::grid.draw(profileglyph)#'
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
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 250, z = dims,
#'                                     size = 100, line = FALSE,
#'                                     flip.axes = TRUE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 250, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  col.line = "green")
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
#'                              col.bar = "salmon", col.line = "salmon")
#'
#' barprofileglyph <- profileglyphGrob(x = 300, y = 250, z = dims,
#'                                     size = 100, line = FALSE, mirror = FALSE,
#'                                     flip.axes = TRUE,
#'                                     col.bar = "cyan")
#'
#' profileglyph <- profileglyphGrob(x = 500, y = 250, z = dims,
#'                                  size = 100, line = TRUE, bar = FALSE,
#'                                  flip.axes = TRUE,
#'                                  mirror = FALSE, col.line = "green")
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
#' # linejoin variants
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
#' dims = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33)
#' bg1 <- profileglyphGrob(x = 100, y = 100, z = dims,
#'                         size = 100,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg1 <- profileglyphGrob(x = 300, y = 100, z = dims,
#'                          size = 100, line = FALSE,
#'                          fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg2 <- profileglyphGrob(x = 150, y = 250, z = dims,
#'                         size = 100, mirror = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg2 <- profileglyphGrob(x = 350, y = 250, z = dims,
#'                          size = 100, line = FALSE, mirror = FALSE,
#'                          fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg3 <- profileglyphGrob(x = 100, y = 300, z = dims,
#'                         size = 100, flip.axes = TRUE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg3 <- profileglyphGrob(x = 300, y = 300, z = dims,
#'                          size = 100, line = FALSE, flip.axes = TRUE,
#'                          fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg4 <- profileglyphGrob(x = 150, y = 400, z = dims,
#'                         size = 100, mirror = FALSE, flip.axes = TRUE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bpg4 <- profileglyphGrob(x = 350, y = 400, z = dims,
#'                          size = 100, line = FALSE, mirror = FALSE,
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
#' bg1 <- profileglyphGrob(x = 100, y = 150, z = dims,
#'                         size = 50, width = 20,
#'                         grid.lines = TRUE, lwd = 2,
#'                         grid.levels = gl, col.grid = "black")
#'
#' bg2 <- profileglyphGrob(x = 250, y = 200, z = dims,
#'                         size = 50, width = 20, lwd = 2,
#'                         grid.lines = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "black")
#'
#' bg3 <- profileglyphGrob(x = 400, y = 150, z = dims,
#'                         size = 50, width = 20, flip.axes = TRUE,
#'                         grid.lines = TRUE, lwd = 2,
#'                         grid.levels = gl, col.grid = "black")
#'
#' bg4 <- profileglyphGrob(x = 500, y = 150, z = dims,
#'                         size = 50, width = 20, flip.axes = TRUE,
#'                         grid.lines = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "black",
#'                         lwd = 2)
#'
#' bg5 <- profileglyphGrob(x = 100, y = 350, z = dims,
#'                         size = 50, width = 20,
#'                         grid.lines = TRUE, lwd = 2,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", line = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg6 <- profileglyphGrob(x = 250, y = 400, z = dims,
#'                         size = 50, width = 20, lwd = 2,
#'                         grid.lines = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", line = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg7 <- profileglyphGrob(x = 400, y = 350, z = dims,
#'                         size = 50, width = 20, flip.axes = TRUE,
#'                         grid.lines = TRUE, lwd = 2,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", line = FALSE,
#'                         fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' bg8 <- profileglyphGrob(x = 500, y = 350, z = dims,
#'                         size = 50, width = 20, flip.axes = TRUE,
#'                         grid.lines = TRUE, mirror = FALSE,
#'                         grid.levels = gl, col.grid = "white",
#'                         col.bar = "white", lwd = 2, line = FALSE,
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
                          lwd = 1,
                          alpha = 1,
                          width = 10,
                          flip.axes = FALSE,
                          bar = TRUE,
                          line = TRUE,
                          mirror = TRUE,
                          linejoin = c("mitre", "round", "bevel"),
                          grid.levels = NULL,
                          grid.lines = FALSE,
                          col.grid = "grey",
                          lwd.grid = lwd) {

  linejoin <- match.arg(linejoin)

  # grid::grid.rect(gp=gpar(col="gray"))
  # grid::grid.points(x = x, y = y, pch =  20)

  # Checks for grid lines
  drawgridlines <- FALSE
  if (grid.lines) {
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

  if (!flip.axes) {
    # Get bar central points
    xpos <- x + (width * seq(-(dimension - 1) / 2, (dimension - 1) / 2,
                             length.out = dimension))
    ypos <- y - z * size

    # Specify justification
    if (mirror) {
      barjust <- "center"
      # Line y points
      ypos1 <- y - ((z / 2) * size)
      ypos2 <- y + ((z / 2) * size)
    } else {
      barjust <- "top"
    }

    # Bar profile with/without line
    if (bar) {
      bargrob <- grid::rectGrob(x = xpos, y = rep(y, dimension),
                                width = width, height = z * size,
                                default.units = "native", just = barjust,
                                gp = grid::gpar(lwd = lwd, alpha = alpha,
                                                col = col.bar, fill = fill,
                                                linejoin = linejoin))
      if (line) {
        if (mirror) {
          blinegrob <- grid::polylineGrob(x = c(xpos, rev(xpos)),
                                          y = c(ypos1, rev(ypos2)),
                                          id = rep(1:2, each = dimension),
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col.line,
                                                          linejoin = linejoin))
        } else {
          blinegrob <- grid::polylineGrob(x = xpos, y = ypos,
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col.line,
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
                                                       col = col.line,
                                                       linejoin = linejoin))
      } else {
        blinegrob <- grid::polygonGrob(x = c(xpos[1], xpos, xpos[dimension]),
                                       y = c(y, ypos, y),
                                       default.units = "native",
                                       gp = grid::gpar(lwd = lwd,
                                                       alpha = alpha,
                                                       col = col.line,
                                                       linejoin = linejoin))
      }
    }

    # grid::grid.points(x= xpos, y = rep(y, dimension), default.units = "native")

    if (drawgridlines) {
      # plot grid lines
      grid.levels <- mapply(function(a, b) b[b <= a], z, grid.levels)

      if (mirror) {
        gridy <- mapply(function(a, b) a - (b * size), ypos2, grid.levels)
        gridy <- mapply(function(a, b) setdiff(b, a), ypos1, gridy)
      } else {
        gridy <- lapply(grid.levels, function(a) y - (a * size))
        gridy <- mapply(function(a, b) setdiff(b, a), y - (z * size), gridy)
      }

      gridx <- mapply(function(a, b) rep(a, length(b)), xpos, gridy)

      gridx <- unlist(gridx)
      gridy <- unlist(gridy)

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

      glinesGrob <- grid::polylineGrob(x = c(gridxstrt, gridxstp),
                                       y = rep(gridy, 2),
                                       id = rep(seq_len(length(gridx)), 2),
                                       default.units = "native",
                                       gp = gpar(col = col.grid,
                                                 lwd = lwd.grid,
                                                 lineend = "butt",
                                                 alpha = alpha))
    }

    #---------------------------------------------------------------------------


  } else {
    # Get bar central points
    xpos <- x + z * size
    ypos <- y + (width * seq(-(dimension - 1) / 2, (dimension - 1)/ 2,
                             length.out = dimension))

    # Specify justification
    if (mirror) {
      barjust <- "center"
      barjusth <- NULL
      # Line x points
      xpos1 <- x - ((z / 2) * size)
      xpos2 <- x + ((z / 2) * size)
    } else {
      barjust <- "center"
      barjusth <- 0
    }

    # Bar profile with/without line
    if (bar) {
      bargrob <- grid::rectGrob(x= rep(x, dimension), y = ypos,
                                width = z * size, height = width,
                                default.units = "native", just = barjust,
                                hjust = barjusth,
                                gp = grid::gpar(lwd = lwd, alpha = alpha,
                                                col = col.bar, fill = fill,
                                                linejoin = linejoin))
      if (line) {
        if (mirror) {
          blinegrob <- grid::polylineGrob(x = c(xpos1, rev(xpos2)),
                                          y = c(ypos, rev(ypos)),
                                          id = rep(1:2, each = dimension),
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col.line,
                                                          linejoin = linejoin))
        } else {
          blinegrob <- grid::polylineGrob(x = xpos, y = ypos,
                                          default.units = "native",
                                          gp = grid::gpar(lwd = lwd,
                                                          alpha = alpha,
                                                          col = col.line,
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
                                                       col = col.line,
                                                       linejoin = linejoin))
      } else {
        blinegrob <- grid::polygonGrob(x = c(x, xpos, x),
                                       y = c(ypos[1], ypos, ypos[dimension]),
                                       default.units = "native",
                                       gp = grid::gpar(lwd = lwd,
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
        gridx <- mapply(function(a, b) a - (b * size), xpos2, grid.levels)
        gridx <- mapply(function(a, b) setdiff(b, a), xpos1, gridx)
      } else {
        gridx <- lapply(grid.levels, function(a) x + (a * size))
        gridx <- mapply(function(a, b) setdiff(b, a), x + (z * size), gridx)
      }

      gridy <- mapply(function(a, b) rep(a, length(b)), ypos, gridx)

      gridx <- unlist(gridx)
      gridy <- unlist(gridy)

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
                                       y = c(gridystrt, gridystp),
                                       id = rep(seq_len(length(gridy)), 2),
                                       default.units = "native",
                                       gp = gpar(col = col.grid,
                                                 lwd = lwd.grid,
                                                 lineend = "butt",
                                                 alpha = alpha))
    }

  }

  gridout <- grid::grobTree(bargrob, blinegrob, glinesGrob,
                            gp = grid::gpar(lwd = lwd, alpha = alpha,
                                            fill = fill,
                                            linejoin = linejoin))
  return(gridout)

}
