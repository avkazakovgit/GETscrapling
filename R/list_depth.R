
#’ Measure the nesting depth of a list
#’
#’ @param x An R object.
#’ @return An integer: 0 for non-lists; otherwise 1 + the maximum depth of any element.
#’ @examples
#’ list_depth(1)                      # 0
#’ list_depth(list(a=1,b=2))         # 1
#’ list_depth(list(a=list(b=list())) ) # 3
#’ @export
list_depth <- function(x) {
  if (!is.list(x)) {
    return(0L)
  }
  depths <- vapply(x, list_depth, integer(1))
  1L + if (length(depths)) max(depths) else 0L
}
