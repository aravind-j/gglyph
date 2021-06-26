geom_pieglyph <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ...,
                          cols = character(0L),
                          edges = 200,
                          fill.segment = NULL,
                          colour.grid = NULL,
                          linewidth = 1,
                          linewidth.grid = linewidth,
                          scale.segment = FALSE,
                          scale.radius = TRUE,
                          full = TRUE,
                          draw.grid = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  # Modify mapping to include cols
  mcols <- rlang::as_quosures(rlang::syms(cols), .GlobalEnv)
  names(mcols) <- cols
  mapping <- modifyList(mapping, mcols)

  params <- list(
    edges = edges,
    colour.grid = colour.grid,
    fill.segment = fill.segment,
    linewidth = linewidth,
    linewidth.grid = linewidth.grid,
    scale.segment = scale.segment,
    scale.radius = scale.radius,
    full = full,
    draw.grid = draw.grid,
    cols = cols, ...)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPieGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params)

}

GeomPieGlyph <- ggplot2::ggproto("GeomPieGlyph", ggplot2::Geom,
                                 required_aes = c("x", "y"),
                                 default_aes = ggplot2::aes(colour = "black",
                                                            size = 1,
                                                            shape = 19,
                                                            fill = NA,
                                                            stroke = 0.5,
                                                            linetype = 1,
                                                            alpha = 1,
                                                            linejoin = "mitre",
                                                            lineend = "round"),

                                 draw_key = ggplot2::draw_key_polygon,

                                 setup_params = function(data, params) {

                                   params
                                 },

                                 setup_data = function(data, params) {

                                   cols <- params$cols

                                   # Check if "cols" exist in data
                                   if (FALSE %in% (cols %in% colnames(data))) {
                                     stop(paste('The following column(s) specified as "cols" are not present in "data":\n',
                                                paste(cols[!(cols %in% colnames(data))],
                                                      collapse = ", "),
                                                sep = ""))
                                   }


                                   # Check if cols are numeric or factor
                                   intfactcols <- unlist(lapply(data[, cols],
                                                                function(x) FALSE %in% (is.vector(x, mode = "integer") |
                                                                                          is.vector(x, mode = "numeric") |
                                                                                          is.factor(x))))
                                   if (TRUE %in% intfactcols) {
                                     stop('The following column(s) specified as "cols" in ',
                                          '"data" are not of type numeric, integer or factor:\n',
                                          paste(names(intfactcols[intfactcols]), collapse = ", "))
                                   }

                                   draw.grid <- params$draw.grid

                                   if (draw.grid &
                                       !all(unlist(lapply(data[, cols], is.factor)))) {
                                     draw.grid <- FALSE
                                     warning('All the columns specified as "cols" in ',
                                             '"data" are not of type factor.\n',
                                             'Unable to plot grid points.')
                                   }

                                   # Remove rows with missing values in "cols"
                                   # check for missing values
                                   missvcols <- unlist(lapply(data[, cols], function(x) TRUE %in% is.na(x)))
                                   if (TRUE %in% missvcols) {
                                     warning(paste('The following column(s) in "data" have missing values:\n',
                                                   paste(names(missvcols[missvcols]), collapse = ", ")))

                                     data <- remove_missing(df = data, vars = cols)
                                   }

                                   # Check if fill.segment are valid
                                   if (!is.null(params$fill.segment)) {
                                     if (length(params$fill.segment) != length(cols))
                                       stop('The number of colours specified in',
                                            '"fill.segment" are not equal to the number',
                                            'of variables specified in "cols".')

                                     if (!all(iscolour(params$fill.segment))) {
                                       stop('Invalid colour(s) specified in "fill.segment".')
                                     }

                                     data$fill <- NULL
                                   }

                                   # browser()
                                   #
                                   # if (params$draw.grid) {
                                   #   data$point.size <- params$point.size
                                   # } else {
                                   #   data$point.size <- NA
                                   # }

                                   data$linewidth <- params$linewidth
                                   data$linewidth.grid <- params$linewidth.grid
                                   data

                                 },

                                 draw_panel = function(data, panel_params,
                                                       coord, cols,
                                                       edges,
                                                       scale.segment,
                                                       scale.radius,
                                                       linewidth,
                                                       linewidth.grid,
                                                       fill.segment,
                                                       colour.grid,
                                                       full,
                                                       draw.grid) {

                                   data <- coord$transform(data, panel_params)

                                   gl <- grid::grobTree()

                                   if (full) {
                                     astrt <- 0
                                     astp <- 2 * base::pi
                                   } else {
                                     astrt <- 0
                                     astp <- base::pi
                                   }

                                   grid.levels <- NULL

                                   # Convert factor columns to equivalent numeric
                                   if (draw.grid) {
                                     grid.levels <- lapply(data[, cols], function(a) as.integer(levels(a)))
                                   }

                                   fcols <- names(Filter(is.factor, data[, cols]))

                                   if (length(fcols) > 0)  {
                                     data[, fcols] <- lapply(data[, cols], function(f) as.numeric(levels(f))[f])
                                   }

                                   for (i in seq_along(data$x)) {
                                     # addGrob to get proper overlappin of glyphs
                                     gl <- grid::addGrob(gl,
                                                         pieglyphGrob(x = data$x[i],
                                                                      y = data$y[i],
                                                                      z = unlist(data[i, cols]),
                                                                      size = data$size[i],
                                                                      edges = edges,
                                                                      col = data$colour[i],
                                                                      fill = if (is.null(fill.segment)) {
                                                                        data$fill[i]
                                                                      } else {
                                                                        fill.segment
                                                                      },
                                                                      lwd = data$linewidth[i],
                                                                      lwd.grid = data$linewidth.grid[i],
                                                                      alpha = data$alpha[i],
                                                                      angle.start = astrt,
                                                                      angle.stop = astp,
                                                                      scale.segment = scale.segment,
                                                                      scale.radius = scale.radius,
                                                                      linejoin = data$linejoin[i],
                                                                      grid.levels = grid.levels,
                                                                      draw.grid = draw.grid,
                                                                      col.grid = if (is.null(colour.segment)) {
                                                                        data$colour[i]
                                                                      } else {
                                                                        colour.segment
                                                                      }))
                                   }

                                   ggname("geom_pieglyph",
                                          gl)

                                   # ggname("geom_pieglyph",
                                   #        grid::gTree(
                                   #          children = grid::gList(
                                   #            grid::pointsGrob(x = data$x,
                                   #                             y = data$y,
                                   #                             default.units = "native",
                                   #                             pch = 20,
                                   #                             gp = grid::gpar(col = data$colour,
                                   #                                             fill = data$fill))
                                   #          )))
                                 }
)
