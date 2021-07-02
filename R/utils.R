
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
