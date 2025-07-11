#’ Test if an object has non-NULL, non-zero length
#’
#’ @param x Any R object.
#’ @return `FALSE` if `x` is `NULL` or has `length(x) == 0`, otherwise `TRUE`.
#’ @examples
#’ nzlen(NULL)    # FALSE
#’ nzlen(character(0))    # FALSE
#’ nzlen(1:5)     # TRUE
#’ @export
nzlen <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  } else {
    if (length(x) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
