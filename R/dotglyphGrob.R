

dotglyphGrob <- function(x = .5, y = .5, z,
                         radius = 10,
                         col = 'black',
                         fill = NA,
                         lwd = 1,
                         alpha = 1,
                         mirror = FALSE,
                         flip.axes = FALSE) {
  z <- round(z)
  dimension <- length(z)

  # grid::grid.points(x = x, y = y, pch =  20)

  if (!flip.axes) {
    xpos <- x + ((radius * 2) * seq(-(dimension-1)/2, (dimension-1)/2,
                                    length.out = dimension))
    ypos <- rep(y, dimension)

    circx <- mapply(function(a, b) rep(a, b), xpos, z)
    circy <- lapply(z, function(c) y - (1:c * (radius * 2)) + radius)

    if (mirror) {
      circy <- mapply(function(a, b) a + (radius * b[1]), circy, z)
    }

  } else {
    xpos <- rep(x, dimension)
    ypos <- y - ((radius * 2) * seq(-(dimension-1)/2, (dimension-1)/2,
                                    length.out = dimension))

    circy <- mapply(function(a, b) rep(a, b), ypos, z)
    circx <- lapply(z, function(c) x + (1:c * (radius * 2)) - radius)

    if (mirror) {
      circx <- mapply(function(a, b) a - (radius * b[1]), circx, z)
    }

  }

  circx <- unlist(circx)
  circy <- unlist(circy)

  # grid::grid.points(x = xpos, y = ypos, pch =  1)

  grid::circleGrob(x = circx, y = circy, r = radius,
                   default.units = "native", gp = gpar(col = col,
                                                       fill = fill,
                                                       lwd = lwd,
                                                       alpha = alpha))
}

