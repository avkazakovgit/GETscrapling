#’ Append multiple lists element-wise
#’
#’ Given two or more lists of the same shape, appends each element:
#’ `out[[i]] = append(list1[[i]], list2[[i]], …)`.
#’
#’ @param ... Two or more lists.
#’ @return A single list.
#’ @importFrom purrr reduce map2
#’ @export
lists_append <- function(...) {
  purrr::reduce(list(...), ~ purrr::map2(.x, .y, append))
}
