#’ Split a vector into N roughly-equal chunks
#’
#’ @param vec  Atomic vector.
#’ @param N    Integer ≥ 1, number of groups.
#’ @return A named list of length N, each element a subset of `vec`.
#’ @examples
#’ split2n(1:10, 3)
#’ @export
split2n <- function(vec, N) {
  if (N == 0) stop("N must be ≥ 1")
  if (N == 1) {
    return(list(`1` = vec))
  } else {
    split(vec, cut(seq_along(vec), breaks = N, labels = FALSE))
  }
}
