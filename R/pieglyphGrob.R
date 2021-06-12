pieglyphGrob <- function(x = .5, y = .5, z,
                         size = 1, edges = 200,
                         col = 'black', # equal to no. of segments
                         fill = NA, # equal to no. of segments
                         lwd = 1,
                         alpha = 1,
                         angle.start = 0,
                         angle.stop = 2*base::pi) {

  # grid::grid.points(x = x, y = y, pch =  20)

  # Get polygon points
  dimension <- length(z)
  angle <- seq(angle.start, 2*base::pi, length.out = dimension + 1)[1:dimension]

  # Convert z to cumulative proportions
  cumpropz <- c(0, cumsum(z)/sum(z))
  diffz <- diff(cumpropz)
  dimension <- length(diffz)

  gl <- grid::grobTree()

  for (i in 1:dimension) {
    # No. of nodes for sector arc
    n <- max(2, floor(edges * diffz[i]))

    # Sector arc points
    arcp <- seq.int(cumpropz[i], cumpropz[i + 1], length.out = n)

    # Sector arc point radians
    arcpr <- (angle.stop * arcp) + (angle.start * pi/180)

    # Sector arc coordinates
    arcx <- x + (size * cos(arcpr))
    arcy <- y + (size * sin(arcpr))

    gl <- grid::addGrob(gl,
                        polygonGrob(x = c(x, arcx), y = c(y, arcy),
                                    default.units = "native",
                                    gp = grid::gpar(col = col,
                                                    fill = fill,
                                                    lwd = lwd,
                                                    alpha = alpha)))
  }

  return(gl)

}
