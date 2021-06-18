#' Draw a Pie Glyph
#'
#' Uses \code{\link[grid]{Grid}} graphics to draw a circular pie or clock glyph
#' \insertCite{ward_visualization_2000,fuchs_evaluation_2013}{gglyph}.
#'
#' @param x A numeric vector or unit object specifying x-locations.
#' @param y A numeric vector or unit object specifying y-locations.
#' @param z A numeric vector specifying the values to be plotted as dimensions
#'   of the pie glyph according to the arguments \code{scale.segment} or
#'   \code{scale.radius}.
#' @param size The size of glyphs.
#' @param edges The number of edges of the polygon to depict the circular glyph
#'   outline.
#' @param col The line colour.
#' @param fill The fill colour.
#' @param lwd The line width.
#' @param alpha The alpha transparency value.
#' @param angle.start The start angle for the glyph in radians. Default is zero.
#' @param angle.stop The stop anlge for the glyph in radians. Default is
#'   \eqn{2\pi}.
#' @param linejoin The line join style for the pie segment polygons. Either
#'   \code{"mitre"}, \code{"round"} or \code{"bevel"}.
#' @param scale.segment logical. If \code{TRUE}, the segments (pie slices) are
#'   scaled according to value of \code{z}.
#' @param scale.radius logical. If \code{TRUE}, the radius of segments (pie
#'   slices) are scaled according to value of \code{z}.
#' @param grid.lines logical. If \code{TRUE}, grid lines are plotted along the
#'   segments when \code{scale.radius = TRUE}. Default is \code{FALSE}.
#' @param grid.levels A list of grid levels (as vectors) corresponding to the
#'   values in \code{z} at which grid lines are to be plotted. The values in
#'   \code{z} should be present in the list specified.
#' @param lwd.grid The line width of the grid lines.
#' @param col.grid The colour of the grid lines.
#'
#' @return A \code{\link[grid]{grobTree}} object.
#'
#' @importFrom dplyr bind_rows
#' @importFrom grid gpar polygonGrob nullGrob grobTree
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
#' p4 <- pieglyphGrob(x = 150, y = 350,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, angle.start = 0, angle.stop = -base::pi)
#'
#' p5 <- pieglyphGrob(x = 300, y = 350,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.radius = FALSE,
#'                    angle.start = 0, angle.stop = -base::pi)
#'
#' p6 <- pieglyphGrob(x = 450, y = 350,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.segment = TRUE, scale.radius = FALSE,
#'                    angle.start = 0, angle.stop = -base::pi)
#'
#' grid::grid.newpage()
#' grid::grid.draw(p1)
#' grid::grid.draw(p2)
#' grid::grid.draw(p3)
#' grid::grid.draw(p4)
#' grid::grid.draw(p5)
#' grid::grid.draw(p6)
#'
#' p1 <- pieglyphGrob(x = 150, y = 150,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p2 <- pieglyphGrob(x = 300, y = 150,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.radius = FALSE,
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p3 <- pieglyphGrob(x = 450, y = 150,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.segment = TRUE, scale.radius = FALSE,
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p4 <- pieglyphGrob(x = 150, y = 350,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, angle.start = 0, angle.stop = -base::pi,
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p5 <- pieglyphGrob(x = 300, y = 350,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.radius = FALSE,
#'                    angle.start = 0, angle.stop = -base::pi,
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p6 <- pieglyphGrob(x = 450, y = 350,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 50, scale.segment = TRUE, scale.radius = FALSE,
#'                    angle.start = 0, angle.stop = -base::pi,
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' grid::grid.newpage()
#' grid::grid.draw(p1)
#' grid::grid.draw(p2)
#' grid::grid.draw(p3)
#' grid::grid.draw(p4)
#' grid::grid.draw(p5)
#' grid::grid.draw(p6)
#'
#' p1 <- pieglyphGrob(x = 200, y = 150,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 100, lwd = 5)
#'
#' p2 <- pieglyphGrob(x = 400, y = 300,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 100, lwd = 5, linejoin = "round")
#'
#' p3 <- pieglyphGrob(x = 600, y = 450,
#'                    z = c(0.24, 0.3, 0.8, 1.4, 0.6, 0.33),
#'                    size = 100, lwd = 5, linejoin = "bevel")
#'
#' grid::grid.newpage()
#' grid::grid.draw(p1)
#' grid::grid.draw(p2)
#' grid::grid.draw(p3)
#'
#' dims = c(1, 3, 2, 1, 2, 3)
#' gl <- split(x = rep(c(1, 2, 3), 6),
#'             f = rep(1:6, each = 3))
#'
#' p1 <- pieglyphGrob(x = 100, y = 150,
#'                    z = dims, size = 40,
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 2, col.grid = "black")
#'
#' p2 <- pieglyphGrob(x = 400, y = 150,
#'                    angle.start = 0, angle.stop = -base::pi,
#'                    z = dims, size = 40,
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 2, col.grid = "black")
#'
#' p3 <- pieglyphGrob(x = 100, y = 400,
#'                    z = dims, size = 40, scale.segment = TRUE,
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 2, col.grid = "black")
#'
#' p4 <- pieglyphGrob(x = 400, y = 400,
#'                    angle.start = 0, angle.stop = -base::pi,
#'                    z = dims, size = 40, scale.segment = TRUE,
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 2, col.grid = "black")
#'
#' grid::grid.newpage()
#' grid::grid.draw(p1)
#' grid::grid.draw(p2)
#' grid::grid.draw(p3)
#' grid::grid.draw(p4)
#'
#' dims = c(1, 3, 2, 1, 2, 3)
#' gl <- split(x = rep(c(1, 2, 3), 6),
#'             f = rep(1:6, each = 3))
#'
#' p1 <- pieglyphGrob(x = 100, y = 150,
#'                    z = dims, size = 40, col = "white",
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 3, col.grid = "white",
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p2 <- pieglyphGrob(x = 400, y = 150,
#'                    angle.start = 0, angle.stop = -base::pi,
#'                    z = dims, size = 40, col = "white",
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 3, col.grid = "white",
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p3 <- pieglyphGrob(x = 100, y = 400,
#'                    z = dims, size = 40,
#'                    col = "white", scale.segment = TRUE,
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 3, col.grid = "white",
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#' p4 <- pieglyphGrob(x = 400, y = 400,
#'                    angle.start = 0, angle.stop = -base::pi,
#'                    z = dims, size = 40,
#'                    col = "white", scale.segment = TRUE,
#'                    grid.lines = TRUE, grid.levels = gl,
#'                    lwd = 3, col.grid = "white",
#'                    fill = RColorBrewer::brewer.pal(6, "Dark2"))
#'
#'
#' grid::grid.newpage()
#' grid::grid.draw(p1)
#' grid::grid.draw(p2)
#' grid::grid.draw(p3)
#' grid::grid.draw(p4)
#'
pieglyphGrob <- function(x = .5, y = .5, z,
                         size = 1, edges = 200,
                         col = "black",
                         fill = NA,
                         lwd = 1,
                         alpha = 1,
                         angle.start = 0,
                         angle.stop = 2 * base::pi,
                         linejoin = c("mitre", "round", "bevel"),
                         scale.segment = FALSE,
                         scale.radius = TRUE,
                         grid.levels = NULL,
                         grid.lines = FALSE,
                         col.grid = "grey",
                         lwd.grid = lwd) {

  linejoin <- match.arg(linejoin)

  # grid::grid.points(x = x, y = y, pch =  20)

  drawgridlines <- FALSE

  # Checks for grid lines
  drawgridlines <- FALSE

    if (grid.lines) {
      if (scale.radius ) {
      if (!is.null(grid.levels)) { # Check if grid lines are to be plotted
        # Check if grid.levels is a list in appropriate format
        if (is.list(grid.levels) &
            all(unlist( lapply(grid.levels,
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

      } else {
        warning('"scale.radius" is FALSE.\n',
                'Unable to plot grid lines.')
      }

    }

  # Get polygon points
  dimension <- length(z)
  # angle <- seq(angle.start, 2*base::pi, length.out = dimension + 1)[1:dimension]

  # Convert z to cumulative proportions
  if (scale.segment) {
    cumpropz <- c(0, cumsum(z) / sum(z))
  } else {
    # Convert z to cumulative proportions
    # cumpropz <- c(0, scales::rescale(1:dimension))
    cumpropz <- c(scales::rescale(0:dimension))
  }

  diffz <- diff(cumpropz)

  segxylist <- vector("list", dimension)

  if (drawgridlines) {
    gridxylist <- vector("list", dimension)

    grid.levels <- mapply(function(a, b) b[b <= a], z, grid.levels)
    grid.levels <- mapply(function(a, b) setdiff(b, a), z, grid.levels)
  }

  for (i in 1:dimension) {
    # No. of nodes for sector arc
    n <- max(2, floor(edges * diffz[i]))

    # Sector arc points
    arcp <- seq.int(cumpropz[i], cumpropz[i + 1], length.out = n)

    # Sector arc point radians
    arcpr <- (angle.stop * arcp) + (angle.start * (pi / 180))

    # Sector arc coordinates

    if (!scale.radius) {
      segxylist[[i]] <- data.frame(arcx = c(x, x + (size * cos(arcpr))),
                                   arcy = c(y, y + (size * sin(arcpr))),
                                   arcid = i)
    } else {
      segxylist[[i]] <- data.frame(arcx = c(x, x + (z[i] * size * cos(arcpr))),
                                   arcy = c(y, y + (z[i] * size * sin(arcpr))),
                                   arcid = i)
      if (drawgridlines) {
        gridxylist[[i]] <- lapply(grid.levels[[i]],
                                  function(a) if (length(a) == 0) {
                                    NA
                                  } else{
                                    lapply(a,
                                           function(b) data.frame(arcx = c(x + (b * size * cos(arcpr))),
                                                                  arcy = c(y + (b * size * sin(arcpr))),
                                                                  arcid = paste(i, "_", b, sep = "")))
                                  })


      }
    }
  }

  segxy <- dplyr::bind_rows(segxylist)

  #NullGrobs
  circsegGrob <- grid::nullGrob()
  glinesGrob <- grid::nullGrob()

  circsegGrob <- grid::polygonGrob(x = segxy$arcx,
                                   y = segxy$arcy,
                                   id = segxy$arcid,
                                   default.units = "native",
                                   gp = grid::gpar(col = col,
                                                   fill = fill,
                                                   lwd = lwd,
                                                   alpha = alpha,
                                                   linejoin = linejoin))

  if (drawgridlines) {
    gridxy <- dplyr::bind_rows(lapply(gridxylist, dplyr::bind_rows))
    gridxy$arcid <- as.numeric(as.factor(gridxy$arcid))

    glinesGrob <- grid::polylineGrob(x = gridxy$arcx,
                                     y = gridxy$arcy,
                                     id = gridxy$arcid,
                                     default.units = "native",
                                     gp = grid::gpar(col = col.grid,
                                                     lwd = lwd.grid,
                                                     lineend = "butt",
                                                     alpha = alpha))
  }

  gridout <- grid::grobTree(circsegGrob, glinesGrob,
                            gp = grid::gpar(lwd = lwd, alpha = alpha,
                                            fill = fill,
                                            linejoin = linejoin))
  return(gridout)


}
