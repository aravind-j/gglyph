
# Get unexported functions from ggplot2
ggname <- utils::getFromNamespace("ggname", "ggplot2")



# Check if a colour is valid
iscolour <- function(x) {
  sapply(x, function(x) {
    tryCatch(is.matrix(col2rgb(x)),
             error = function(e) FALSE)
  })
}
