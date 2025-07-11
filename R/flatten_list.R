#’ Recursively flatten a nested list until no further flattening is possible
#’
#’ @param x A possibly nested list.
#’ @param name_spec A glue‐style template for naming flattened elements,
#’   e.g. `"{outer}_{inner}"`.
#’ @return A fully flattened list with unique names.
#’ @importFrom purrr list_flatten
#’ @export
flatten_list <- function(x, name_spec = '{outer}_{inner}') {
  out <- x
  repeat {
    prev <- out
    out  <- purrr::list_flatten(
      out,
      name_spec   = name_spec,
      name_repair = "unique"
    )
    if (identical(prev, out)) break
  }
  out
}
