
tileglyphGrob <- function(x = .5, y = .5, z,
                          size = 10,
                          ratio = 1,
                          nrow = 1,
                          col = 'black',
                          fill = NA,
                          lwd = 1,
                          alpha = 1) {
  # grid::grid.points(x = x, y = y, pch =  20)

  dimension <- length(z)

  width <- size
  height <- size * ratio

  dim <- ceiling(dimension/nrow)

  zs <- split(z, sort(rep_len(1:nrow, length(z))))

  if (nrow > 1) {
    ys <- y + (height * seq(-(nrow-1)/2, (nrow-1)/2,
                            length.out = nrow))
  } else {
    ys <- y
  }

  xs <- x + (width * seq(-(dim-1)/2, (dim-1)/2,
                         length.out = dim))

  xylist <- vector("list", nrow)

  for (i in 1:nrow) {
    nz <- length(zs[[i]])
    xylist[[i]] <- data.frame(xpos = xs[1:nz],
                              ypos = rep(ys[[i]], nz))
  }

  xylist <- dplyr::bind_rows(xylist)

  grid::grid.points(x = xylist$xpos, y = xylist$ypos, pch =  1)

  grid::rectGrob(x= xylist$xpos, y = xylist$ypos,
                 width = width, height = height,
                 default.units = "native", just = "centre")
}
