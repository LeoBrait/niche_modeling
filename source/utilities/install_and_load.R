#' Install and load libraries
#' @param libs a vector of libraries to install and load
#' It verifies if the given libs are installed and if not, it installs them.
install_and_load <- function(libs) {
  for (lib in seq_along(libs)) {
    if (libs[lib] %in% rownames(installed.packages()) == FALSE) {
      install.packages(libs[lib], dependencies = TRUE)
    }
  }
  lapply(libs, require, character.only = TRUE)
}