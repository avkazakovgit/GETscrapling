#' @import reticulate
#' @import assertthat
#' @import stats
#' @import glue
#' @import stringr
#' @import stringi
#' @import lubridate
#' @import janitor
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import dbplyr
#' @import magrittr
#' @import curl
#' @import RCurl
#' @import httr
#' @import xml2
#' @import XML
#' @import rvest
#' @import jsonlite
#' @import tidyjson
#' @import rjson
#' @import DBI
.onLoad <- function( libname ,  pkgname ) {

  # 1) Tell reticulate which Python to use:
  python <- Sys.getenv("RETICULATE_PYTHON", "<your‐path>/python3")
  reticulate::use_python(python, required = TRUE)

  # 2) Check for key packages (warn if missing):
  needed    <- c("scrapling","nest_asyncio","uvloop","regex","jsonfinder")
  installed <- reticulate::py_list_packages()$package
  missing   <- setdiff(needed, installed)
  if (length(missing)) {
    warning("GETscrapling needs Python packages: ", paste(missing, collapse = ", "),
            ". You can install them with reticulate::py_install().")
  }

  # 3) Import & configure Python modules:
  asyncio <- reticulate::import("asyncio", delay_load = TRUE)
  uvloop  <- reticulate::import("uvloop",  delay_load = TRUE)
  asyncio$set_event_loop_policy(uvloop$EventLoopPolicy())

  scrapling        <- reticulate::import("scrapling",    delay_load = TRUE)
  StealthyFetcher  <- scrapling$fetchers$StealthyFetcher

  # 4) Tweak global flags:
  StealthyFetcher$auto_match     <- FALSE
  StealthyFetcher$huge_tree      <- FALSE
  StealthyFetcher$keep_comments  <- FALSE
  StealthyFetcher$keep_cdata     <- FALSE

  # 5) Expose into the package namespace
  pkgenv <- parent.env(environment())
  assign("asyncio",         asyncio,        envir = pkgenv)
  assign("StealthyFetcher", StealthyFetcher, envir = pkgenv)
  assign("dict",            reticulate::import_builtins()$dict,
         envir = pkgenv)

}
