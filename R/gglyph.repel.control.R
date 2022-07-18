#' Control parameters for the repel algorithm
#'
#' Set the contol parameters for the repel algorithm
#' \insertCite{slowikowski_ggrepel_2021}{gglyph} used in various geoms
#' (\code{geom_*()}) implemented in \code{gglyph} to repel the glyphs from each
#' other.
#'
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param xlim,ylim Limits for the x and y axes. Text labels will be constrained
#'   to these limits. By default, text labels are constrained to the entire plot
#'   area.
#' @param box.padding Amount of padding around bounding box, as unit or number.
#'   Defaults to 0.25. (Default unit is lines, but other units can be specified
#'   by passing \code{unit(x, "units")}).
#' @param point.padding Amount of padding around labeled point, as unit or
#'   number. Defaults to 0. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param min.segment.length Skip drawing segments shorter than this, as unit or
#'   number. Defaults to 0.5. (Default unit is lines, but other units can be
#'   specified by passing \code{unit(x, "units")}).
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#' @param force Force of repulsion between overlapping text labels. Defaults to
#'   1.
#' @param force_pull Force of attraction between a text label and its
#'   corresponding data point. Defaults to 1.
#' @param max.time Maximum number of seconds to try to resolve overlaps.
#'   Defaults to 0.5.
#' @param max.iter Maximum number of iterations to try to resolve overlaps.
#'   Defaults to 10000.
#' @param max.overlaps Exclude text labels that overlap too many things.
#'   Defaults to 10.
#' @param direction "both", "x", or "y" -- direction in which to adjust position
#'   of labels
#' @param seed Random seed passed to \code{\link[base]{set.seed}}. Defaults to
#'   \code{NA}, which means that \code{set.seed} will not be called.
#' @param verbose If \code{TRUE}, some diagnostics of the repel algorithm are
#'   printed
#'
#' @return A list with the following components to control the repel algorithm
#'   corresponding to the same in \strong{Arguments}. \item{box.padding}{}
#'   \item{point.padding}{} \item{min.segment.length}{} \item{arrow}{}
#'   \item{force}{} \item{force_pull}{} \item{max.time}{} \item{max.iter}{}
#'   \item{max.overlaps}{} \item{nudge_x, nudge_y}{} \item{xlim, ylim}{}
#'   \item{direction}{} \item{seed}{} \item{verbose}{}
#'
#' @encoding UTF-8
#'
#' @seealso \code{\link[ggrepel]{ggrepel}}
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @export
#'
#' @examples
#'
#' # Adjust force
#' gglyph.repel.control(force = 0.5)
#'
#' # Adjust max.iter
#' gglyph.repel.control(max.iter = 5000)
#'
#'
gglyph.repel.control <- function(box.padding = 0.25,
                                 point.padding = 1e-6,
                                 min.segment.length = 0.5,
                                 arrow = NULL,
                                 force = 1,
                                 force_pull = 1,
                                 max.time = 0.5,
                                 max.iter = 10000,
                                 max.overlaps = getOption("ggrepel.max.overlaps", default = 10),
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 xlim = c(NA, NA),
                                 ylim = c(NA, NA),
                                 direction = c("both","y","x"),
                                 seed = NA,
                                 verbose = FALSE) {

  direction <- match.arg(direction)

  out <- list(box.padding = box.padding,
              point.padding = point.padding,
              min.segment.length = min.segment.length,
              arrow = arrow,
              force = force,
              force_pull = force_pull,
              max.time = max.time,
              max.iter = max.iter,
              max.overlaps = max.overlaps,
              nudge_x = nudge_x,
              nudge_y = nudge_y,
              xlim = xlim,
              ylim = ylim,
              direction = direction,
              seed = NA,
              verbose = FALSE)

  return(out)
}
