
# Get unexported functions from ggplot2
ggname <- utils::getFromNamespace("ggname", "ggplot2")
remove_missing <- utils::getFromNamespace("remove_missing", "ggplot2")

# Get unexported functions from grid
upgradeUnit.unit.list <- utils::getFromNamespace("upgradeUnit.unit.list",
                                                 "grid")

# Check if a colour is valid

#' @importFrom grDevices col2rgb
iscolour <- function(x) {
  sapply(x, function(x) {
    tryCatch(is.matrix(grDevices::col2rgb(x)),
             error = function(e) FALSE)
  })
}

# Helper to find dimensions of bounding box
boxdim <- function(x, what = c("min", "max")) {

  df <- lapply(x, unlist)
  df <- data.frame(do.call(rbind,  df))

  d1 <- unique(df[, 2])

  if (length(d1) > 1) {
    stop("Unable to compute bounding box coordinates.")
  }

  if (what == "min") {
    d2 <-  min(df[, 4])
  }

  if (what == "max") {
    d2 <- max(df[, 4])
  }

  out <- unit(d1, "native") + unit(d2, "mm")

  return(out)

}
