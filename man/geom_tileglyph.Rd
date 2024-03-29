% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_tileglyph.R
\name{geom_tileglyph}
\alias{geom_tileglyph}
\title{Add Tile Glyphs as a Scatterplot}
\usage{
geom_tileglyph(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  cols = character(0L),
  colour = "black",
  ratio = 1,
  nrow = 1,
  linewidth = 1,
  fill.gradient = NULL,
  show.legend = NA,
  repel = FALSE,
  repel.control = gglyph.repel.control(),
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{...}{Other arguments passed on to \code{\link[ggplot2]{layer}()}. These
are often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "green"} or \code{size = 3}. They may also be parameters to
the paired geom/stat.}

\item{cols}{Name of columns specifying the variables to be plotted in the
glyphs as a character vector.}

\item{colour}{The colour of the tile glyphs.}

\item{ratio}{The aspect ratio (height / width).}

\item{nrow}{The number of rows.}

\item{linewidth}{The line width of the tile glyphs.}

\item{fill.gradient}{The palette for gradient fill of the segments. See
\strong{Details} section of \code{\link[scales]{col_numeric}()} function in
the \code{\link[scales]{scales}} package for available options.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{repel}{logical. If \code{TRUE}, the glyphs are repel away from each
other to avoid overlaps. Default is \code{FALSE}.}

\item{repel.control}{A list of control settings for the repel algorithm.
Ignored if \code{repel = FALSE}. See
\code{\link[gglyph]{gglyph.repel.control}} for details  on the various
control parameters.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A \code{geom} layer.
}
\description{
The tileglyph geom is used to plot multivariate data as tile glyphs similar
to 'autoglyph' \insertCite{beddow_shape_1990}{gglyph} or 'stripe glyph'
\insertCite{fuchs_evaluation_2013}{gglyph} in a scatterplot.
}
\section{Aesthetics}{
 \code{geom_tileglyph()} understands the following
  aesthetics (required aesthetics are in bold): \itemize{ \item{\strong{x}}
  \item{\strong{y}} \item{alpha} \item{group} \item{size} }

  See \code{vignette("ggplot2-specs", package = "ggplot2")} for further
  details on setting these aesthetics.

  The following additional aesthetics are considered if \code{repel = TRUE}:
  \itemize{ \item{point.size} \item{segment.linetype} \item{segment.colour}
  \item{segment.size} \item{segment.alpha} \item{segment.curvature}
  \item{segment.angle} \item{segment.ncp} \item{segment.shape}
  \item{segment.square} \item{segment.squareShape} \item{segment.inflect}
  \item{segment.debug} }

  See \code{ggrepel}
  \href{https://ggrepel.slowkow.com/articles/examples.html}{examples} page
  for further details on setting these aesthetics.
}

\examples{

# Scale the data
zs <- c("hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
mtcars[ , zs] <- lapply(mtcars[ , zs], scales::rescale)

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$lab <- row.names(mtcars)

library(ggplot2)
theme_set(theme_bw())
options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(8, "Dark2"))
options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(8, "Dark2"))

ggplot(data = mtcars) +
  geom_tileglyph(aes(x = mpg, y = disp),
                 cols = zs, size = 2,
                 fill.gradient = "Blues",
                 alpha =  0.5) +
  ylim(c(-0, 550))

ggplot(data = mtcars) +
  geom_tileglyph(aes(x = mpg, y = disp),
                 cols = zs, size = 2,
                 nrow = 2,
                 fill.gradient = "Greens",
                 alpha =  0.5) +
  ylim(c(-0, 550))

ggplot(data = mtcars) +
  geom_tileglyph(aes(x = mpg, y = disp),
                 cols = zs, size = 1,
                 ratio = 4,
                 fill.gradient = "RdYlBu",
                 alpha =  0.5) +
  ylim(c(-0, 550))

ggplot(data = mtcars) +
  geom_tileglyph(aes(x = mpg, y = disp),
                 cols = zs, size = 1,
                 ratio = 4, nrow = 2,
                 fill.gradient = "viridis",
                 alpha =  0.5) +
  ylim(c(-0, 550))

# Repel glyphs
ggplot(data = mtcars) +
  geom_point(aes(x = mpg, y = disp)) +
  geom_tileglyph(aes(x = mpg, y = disp),
                 cols = zs, size = 2,
                 fill.gradient = "Blues",
                 alpha = 1, repel = TRUE) +
  ylim(c(-0, 550))

ggplot(data = mtcars) +
  geom_point(aes(x = mpg, y = disp)) +
  geom_tileglyph(aes(x = mpg, y = disp),
                 cols = zs, size = 1,
                 ratio = 4, nrow = 2,
                 fill.gradient = "viridis",
                 alpha = 1, repel = TRUE) +
  ylim(c(-0, 550))

}
\references{
\insertAllCited{}
}
\seealso{
\code{\link[gglyph]{tileglyphGrob}}

Other geoms: 
\code{\link{geom_dotglyph}()},
\code{\link{geom_metroglyph}()},
\code{\link{geom_pieglyph}()},
\code{\link{geom_profileglyph}()},
\code{\link{geom_starglyph}()}
}
\concept{geoms}
