#’ Install (if needed) and attach all your personal deps
#’
#’ @param pkgs Character vector of package names.  Defaults to the personal set.
#’ @export
init_env <- function(pkgs = c(
  "rlang","devtools","reticulate","R.utils","installr",
  "readr","qs","data.table","readxl","haven","arrow","vroom","foreign",
  "tidyverse","tidyr","dplyr","purrr","magrittr","skimr","gt","broom",
  "stringi","stringr","glue","stringdist","lubridate","janitor",
  "ggplot2","ggtext","showtext","scales","gtools",
  "rvest","XML","xml2","httr","jsonlite","tidyjson","rjson","RCurl","curl","chromote","later","promises",
  "foreach","doParallel","future","future.apply","furrr",
  "fixest","insight","modelsummary",
  "labelled","kableExtra","knitr"
)) {
  install_one <- function(pkg) {
    if (!requireNamespace(pkg, quietly=TRUE)) {
      message("Installing ", pkg, " …")
      install.packages(pkg)
    }
  }
  invisible(lapply(pkgs, install_one))

  # Now attach everything
  invisible(lapply(pkgs, function(pkg) {
    message("Loading ", pkg, " …")
    library(pkg, character.only=TRUE)
  }))

  message("All done—your environment is ready.")
}
