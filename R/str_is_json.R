#’ Test whether a string contains valid JSON
#’
#’ Attempts to `json_parse()` the input and returns `TRUE` if it
#’ successfully yields a JSON object or array, `FALSE` otherwise.
#’
#’ @param json_str A length-1 character string.
#’ @return Logical scalar: `TRUE` if `json_str` contains valid JSON,
#’   `FALSE` if parsing fails.
#’ @examples
#’ str_is_json('{"a":1,"b":2}')
#’ str_is_json("not json")
#’ @export
str_is_json <- function(json_str) {
  tryCatch({
    parsed <- json_parse(json_str)
    TRUE
  }, error = function(e) {
    FALSE
  })
}
