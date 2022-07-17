#' Control parameters for the repel algorithm
#'
#' Set the contol parameters for the repel algorithm
#' \insertCite{slowikowski_ggrepel_2021}{gglyph} used in various geoms
#' (\code{geom_*()}) implemented in \code{gglyph} to repel the glyphs from each
#' other.
#'
#' @template repel-arg
#'
#' @return A list with the following components to control the repel algorithm
#'   corresponding to the same in \strong{Arguments}. \item{box.padding}{}
#'   \item{point.padding}{} \item{min.segment.length}{} \item{arrow}{}
#'   \item{force}{} \item{force_pull}{} \item{max.time}{} \item{max.iter}{}
#'   \item{max.overlaps}{} \item{nudge_x, nudge_y}{} \item{xlim, ylim}{}
#'   \item{direction}{} \item{seed}{} \item{verbose}{} \item{repel}{}
#'   \item{repel.control}{}
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
                                 na.rm = FALSE,
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
              na.rm = na.rm,
              direction = direction,
              seed = NA,
              verbose = FALSE)

  return(out)
}
