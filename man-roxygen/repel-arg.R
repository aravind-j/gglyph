#' @param repel logical. If \code{TRUE}, the glyphs are repel away from each
#'   other to avoid overlaps. Default is \code{FALSE}.
#' @param repel.control A list of control settings for the repel algorithm.
#'   Ignored if \code{repel = FALSE}. See
#'   \code{\link[gglyph]{gglyph.repel.control}} for details  on the various
#'   control parameters.
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
