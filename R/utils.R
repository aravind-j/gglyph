
# Get unexported functions from ggplot2
ggname <- utils::getFromNamespace("ggname", "ggplot2")



# Check if a colour is valid

#' @importFrom grDevices col2rgb
iscolour <- function(x) {
  sapply(x, function(x) {
    tryCatch(is.matrix(grDevices::col2rgb(x)),
             error = function(e) FALSE)
  })
}
