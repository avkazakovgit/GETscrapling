#’ Test if a list is non-NULL and non-empty
#’
#’ @param list An R object.
#’ @return `TRUE` if `list` is a non-NULL list of length > 0, otherwise `FALSE`.
#’ @examples
#’ nzlist(NULL)       # FALSE
#’ nzlist(list())     # FALSE
#’ nzlist(list(a=1))  # TRUE
#’ @export
nzlist <- function(list) {
  if (!is.null(list)) {
    if (is.list(list) && length(list) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}
