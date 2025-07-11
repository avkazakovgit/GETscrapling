#’ Extract all JSON objects from an HTML string via Python’s jsonfinder
#’
#’ Uses the Python `jsonfinder` module to locate and parse every JSON
#’ object embedded in an HTML/text blob.
#’
#’ @param html A length-1 character string of raw HTML (or any text).
#’ @return A **list** of R objects (lists or vectors) corresponding to each
#’   JSON object found.  If none are found, returns an empty list.
#’ @examples
#’ \dontrun{
#’ html <- '<script>var data = {"a":1,"b":2};</script>'
#’ json_find(html)
#’ }
#’ @importFrom reticulate py_run_string py_to_r py
#’ @export
json_find <- function(html) {
  reticulate::py_run_string("
import jsonfinder

def extract_with_jsonfinder(html):
    '''Returns a list of parsed JSON objects found in `html` via jsonfinder.jsonfinder().'''
    results = []
    for start, end, obj in jsonfinder.jsonfinder(html):
        if obj is not None:
            results.append(obj)
    return results
")
  stopifnot(is.character(html), length(html) == 1)
  py_fun  <- reticulate::py$extract_with_jsonfinder
  objs_py <- py_fun(html)
  lapply(objs_py, reticulate::py_to_r)
}
