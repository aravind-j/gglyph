

wlcm <- paste0("\n",
               "--------------------------------------------------------------------------------\n",
               "Welcome to gglyph version ", utils::packageDescription("gglyph")$Version, "\n",
               "\n", "\n",
               # "# To know how to use this package type:", "\n",
               # "  browseVignettes(package = 'gglyph')", "\n", "  for the package vignette.", "\n",
               # "\n",
               "# To know whats new in this version type:", "\n",
               "  news(package='gglyph')", "\n", "  for the NEWS file.", "\n",
               "\n",
               "# To cite the methods in the package type:", "\n",
               "  citation(package='gglyph')", "\n",
               "\n",
               "# To suppress this message use:", "\n",
               "  suppressPackageStartupMessages(library(gglyph))", "\n",
               "--------------------------------------------------------------------------------\n")

.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage(wlcm)


}
