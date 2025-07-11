#’ Convert a list of records into a tibble
#’
#’ Attempts a JSON‐based flatten (`jsonlite::fromJSON(flatten=TRUE)`),
#’ falling back to `tidyjson` on error.
#’
#’ @param json_list A list where each element is a named list (record).
#’ @return A tibble.
#’ @importFrom jsonlite toJSON fromJSON
#’ @importFrom tibble as_tibble
#’ @importFrom tidyjson as.tbl_json spread_all enter_object
#’ @export
list2df <- function(json_list) {
  json_data <- jsonlite::toJSON(json_list, auto_unbox = TRUE) %>% as.character()
  tryCatch({
    jsonlite::fromJSON(json_data, flatten = TRUE) %>% tibble::as_tibble()
  }, error = function(e) {
    json_data %>%
      tidyjson::as.tbl_json() %>%
      tidyjson::spread_all() %>%
      tidyjson::enter_object(name) %>%
      tidyjson::spread_all()
  })
}
