#’ Convert a deep list‐of‐lists into a data.frame via JSON round‐trip
#’
#’ Sometimes JSON→R conversion flattens a nested list into a data.frame
#’ automatically.  This helper does `toJSON()` + `fromJSON(simplifyDataFrame=TRUE)`.
#’
#’ @param jsonlst A list or nested list.
#’ @return A data.frame (or tibble) representing the JSON data.
#’ @importFrom jsonlite toJSON fromJSON
#’ @export
jsonlst_endf <- function(jsonlst) {
  jsonlite::toJSON(jsonlst) %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
}
