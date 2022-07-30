#' Alter scales for continuous data mapped to glyphs
#'
#' Scale variable(s) mapped to the glyphs to the given
#' range using \code{\link[scales]{rescale_pal}}.
#'
#' @param ... Additional arguments to be passed on to \code{\link[ggplot2]{continuous_scale}}.
#' @param range The range to which the variable(s) specified in argument \code{z} are to be scaled.
#' @param z The variable(s) mapped to the glyph as a character vector.
#'
#' @importFrom scales rescale_pal
#' @importFrom ggplot2 continuous_scale
#' @export
#'
scale_z_continuous <- function(..., range = c(0.1, 1), z) {
  continuous_scale(
    aesthetics = z,
    scale_name = "z_continuous",
    palette = scales::rescale_pal(range),
    ...
  )
}
