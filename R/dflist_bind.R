#’ Bind multiple data.frame lists together
#’
#’ Shortcut for `reduce(..., ~ map2(.x, .y, bind_rows))`.
#’
#’ @param ... Two or more lists of data.frames.
#’ @return A list of data.frames.
#’ @importFrom purrr reduce map2
#’ @importFrom dplyr bind_rows
#’ @export
dflist_bind <- function(...) {
  purrr::reduce(list(...), ~ purrr::map2(.x, .y, dplyr::bind_rows))
}
