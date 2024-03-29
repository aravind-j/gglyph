% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dotglyphGrob.R
\name{dotglyphGrob}
\alias{dotglyphGrob}
\title{Draw a Dot Profile Glyph}
\usage{
dotglyphGrob(
  x = 0.5,
  y = 0.5,
  z,
  radius = 1,
  col = "black",
  fill = NA,
  lwd = 1,
  alpha = 1,
  mirror = FALSE,
  flip.axes = FALSE
)
}
\arguments{
\item{x}{A numeric vector or unit object specifying x-locations.}

\item{y}{A numeric vector or unit object specifying y-locations.}

\item{z}{A numeric vector specifying the values to be plotted as dimensions
of the dot glyph (number of stacked dots).}

\item{radius}{The radius of the glyphs.}

\item{col}{The line colour.}

\item{fill}{The fill colour.}

\item{lwd}{The line width.}

\item{alpha}{The alpha transparency value.}

\item{mirror}{logical. If \code{TRUE}, mirror profile is plotted.}

\item{flip.axes}{logical. If \code{TRUE}, axes are flipped.}
}
\value{
A \code{\link[grid]{grob}} object.
}
\description{
Uses \code{\link[grid]{Grid}} graphics to draw a dot profile glyph
\insertCite{chambers_graphical_1983,dutoit_graphical_1986}{gglyph}.
}
\examples{
dg1 <- dotglyphGrob(x = 150, y = 300,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2)

dg2 <- dotglyphGrob(x = 550, y = 300,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, mirror = TRUE)

dg3 <- dotglyphGrob(x = 100, y = 550,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, flip.axes = TRUE)

dg4 <- dotglyphGrob(x = 550, y = 550,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, mirror = TRUE,
                    flip.axes = TRUE)

grid::grid.newpage()
grid::grid.draw(dg1)
grid::grid.draw(dg2)
grid::grid.draw(dg3)
grid::grid.draw(dg4)

dg1 <- dotglyphGrob(x = 150, y = 300,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, fill = "black", col = "white")

dg2 <- dotglyphGrob(x = 550, y = 300,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, mirror = TRUE,
                    fill = "salmon", col = "black")

dg3 <- dotglyphGrob(x = 100, y = 550,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, flip.axes = TRUE,
                    fill = "cyan", col = "grey")

dg4 <- dotglyphGrob(x = 550, y = 550,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, mirror = TRUE,
                    flip.axes = TRUE,
                    fill = "green", col = "grey")

grid::grid.newpage()
grid::grid.draw(dg1)
grid::grid.draw(dg2)
grid::grid.draw(dg3)
grid::grid.draw(dg4)

clrs <- mapply(function(a, b) rep(a, b),
               RColorBrewer::brewer.pal(6, "Dark2"),
               round(c(4, 3.5, 2.7, 6.8, 3.4, 5.7)))
clrs <- unlist(clrs)

dg1 <- dotglyphGrob(x = 150, y = 300,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, fill = clrs, col = "white")

dg2 <- dotglyphGrob(x = 550, y = 300,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, mirror = TRUE,
                    fill = clrs, col = "black")

dg3 <- dotglyphGrob(x = 100, y = 550,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, flip.axes = TRUE,
                    fill = "black", col = clrs, lwd = 5)

dg4 <- dotglyphGrob(x = 550, y = 550,
                    z = c(4, 3.5, 2.7, 6.8, 3.4, 5.7),
                    radius = 2, mirror = TRUE,
                    flip.axes = TRUE,
                    col = clrs)

grid::grid.newpage()
grid::grid.draw(dg1)
grid::grid.draw(dg2)
grid::grid.draw(dg3)
grid::grid.draw(dg4)

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link[gglyph]{geom_dotglyph}}

Other grobs: 
\code{\link{metroglyphGrob}()},
\code{\link{pieglyphGrob}()},
\code{\link{profileglyphGrob}()},
\code{\link{starglyphGrob}()},
\code{\link{tileglyphGrob}()}
}
\concept{grobs}
