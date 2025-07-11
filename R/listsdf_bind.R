#’ Bind‐row multiple lists of data.frames element-wise
#’
#’ If you have e.g. `list(df1, df2)` and `list(df3, df4)`, returns
#’ `list(bind_rows(df1, df3), bind_rows(df2, df4))`.
#’
#’ @param ... Two or more lists of data.frames.
#’ @return A list of data.frames.
#’ @importFrom purrr reduce map2
#’ @importFrom dplyr bind_rows
#’ @export
listsdf_bind <- function(...) {
  purrr::reduce(list(...), ~ purrr::map2(.x, .y, dplyr::bind_rows))
}
