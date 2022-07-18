# Set up the data - scale
zs <- c("hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
mtcars[ , zs] <- lapply(mtcars[ , zs], scales::rescale)

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$lab <- row.names(mtcars)


library(ggplot2)


test_that("Both whiskers and contour", {

  vdiffr::expect_doppelganger("GSG: full = T",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10, alpha =  0.5) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: full = F",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10, alpha =  0.5, full = FALSE) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: Adjust linewidth.*v1",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10, alpha =  0.5,
                                               linewidth.whisker = 3, linewidth.contour = 0.1) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: Adjust linewidth.* v2",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10, alpha =  0.5,
                                               linewidth.whisker = 1, linewidth.contour = 3) +
                                ylim(c(-0, 550))
  )

})

test_that("Only contours (polygon)", {

  vdiffr::expect_doppelganger("GSG: contour = T; full = T",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = FALSE, contour = TRUE,
                                               size = 10, alpha =  0.5) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: contour = T; full = F",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = FALSE, contour = TRUE,
                                               size = 10, alpha =  0.5, full = FALSE) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: contour = T; Adjust linewidth.*",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = FALSE, contour = TRUE,
                                               size = 10, alpha =  0.5, linewidth.contour = 3) +
                                ylim(c(-0, 550))
  )

})

test_that("Only whiskers", {

  vdiffr::expect_doppelganger("GSG: whisker = T; full = T",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
                                               cols = zs, whisker = TRUE, contour = FALSE,
                                               size = 10) +
                                geom_point(data = mtcars, aes(x = mpg, y = disp, colour = cyl)) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: whisker = T; full = F",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
                                               cols = zs, whisker = TRUE, contour = FALSE,
                                               size = 10, full = FALSE) +
                                geom_point(data = mtcars, aes(x = mpg, y = disp, colour = cyl)) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: whisker = T; Adjust linewidth.*",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = FALSE,
                                               size = 10, alpha =  0.5, linewidth.whisker = 3) +
                                ylim(c(-0, 550))
  )

})

test_that("Whiskers with colours", {

  vdiffr::expect_doppelganger("GSG: Map colour.whisker aes",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp),
                                               cols = zs, whisker = TRUE, contour = FALSE,
                                               size = 10,
                                               colour.whisker = RColorBrewer::brewer.pal(8, "Dark2")) +
                                geom_point(data = mtcars, aes(x = mpg, y = disp)) +
                                ylim(c(-0, 550))
  )

})

test_that("Faceted", {

  vdiffr::expect_doppelganger("GSG: facet_grid v1",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10, alpha =  0.5) +
                                ylim(c(-0, 550)) +
                                facet_grid(. ~ cyl)
  )

  vdiffr::expect_doppelganger("GSG: facet_grid v2",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10) +
                                ylim(c(-0, 550)) +
                                facet_grid(. ~ cyl)
  )

  vdiffr::expect_doppelganger("GSG: facet_grid v3",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10, alpha =  0.5) +
                                ylim(c(-0, 550)) +
                                facet_grid(cyl ~ .)
  )

  vdiffr::expect_doppelganger("GSG: facet_grid v4",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10) +
                                ylim(c(-0, 550)) +
                                facet_grid(cyl ~ .)
  )

  vdiffr::expect_doppelganger("GSG: facet_wrap v1",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10, alpha =  0.5) +
                                ylim(c(-0, 550)) +
                                facet_wrap(vars(cyl, vs))
  )

  vdiffr::expect_doppelganger("GSG: facet_wrap v2",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 10) +
                                ylim(c(-0, 550)) +
                                facet_wrap(vars(cyl, vs))
  )

})

rm(mtcars)
mtcars[ , zs] <- lapply(mtcars[ , zs], scales::rescale)

mtcars[ , zs] <- lapply(mtcars[, zs],
                        function(x) cut(x, breaks = 3,
                                        labels = c(1, 2, 3)))
mtcars[ , zs] <- lapply(mtcars[ , zs], as.factor)

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$lab <- row.names(mtcars)

test_that("Grid points", {

  vdiffr::expect_doppelganger("GSG: factor cols; draw.grid = T",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, fill = cyl),
                                               cols = zs, whisker = TRUE, contour = TRUE,
                                               size = 3, alpha =  0.5, draw.grid = TRUE,
                                               point.size = 5) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: factor cols; draw.grid = T; adjust point size",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp, colour = cyl),
                                               cols = zs, whisker = TRUE, contour = FALSE,
                                               size = 3, draw.grid = TRUE, point.size = 7,
                                               linewidth.whisker = 2, alpha = 0.7) +
                                ylim(c(-0, 550))
  )

  vdiffr::expect_doppelganger("GSG: factor cols; draw.grid = T; Map colour.whisker aes",
                              ggplot(data = mtcars) +
                                geom_starglyph(aes(x = mpg, y = disp),
                                               cols = zs, whisker = TRUE, contour = FALSE,
                                               size = 3, draw.grid = TRUE,
                                               point.size = 5, alpha =  0.8,
                                               colour.whisker = RColorBrewer::brewer.pal(8, "Dark2")) +
                                geom_point(data = mtcars, aes(x = mpg, y = disp)) +
                                ylim(c(-0, 550))
  )
})


# map other aes
# test repel
#  - test too many overlaps
# test other geoms

